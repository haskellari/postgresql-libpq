module Main (main) where

import Control.Monad (unless, when)
import Database.PostgreSQL.LibPQ
import Data.Foldable (toList)
import System.Environment (getEnvironment)
import System.Exit (exitFailure)

import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = do
    libpqVersion >>= print
    withConnstring $ \connstring -> do
        smoke connstring
        when isEnabledPipeline $ testPipeline connstring

withConnstring :: (BS8.ByteString -> IO ()) -> IO ()
withConnstring kont = do
    env <- getEnvironment
    case lookup "DATABASE_CONNSTRING" env of
        Just s  -> kont (BS8.pack (special s))
        Nothing -> case lookup "GITHUB_ACTIONS" env of
            Just "true" -> kont (BS8.pack gha)
            _           -> putStrLn "Set DATABASE_CONNSTRING environment variable"
   where
    -- https://www.appveyor.com/docs/services-databases/
    special "appveyor" = "dbname='TestDb' user='postgres' password='Password12!'"
    special "travis"   = ""
    special s          = s

    gha = unwords
        [ "dbname='postgres'"
        , "user='postgres'"
        , "password='postgres'"
        , "host='postgres'"
        , "port=5432"
        ]

smoke :: BS8.ByteString -> IO ()
smoke connstring = do
    conn <- connectdb connstring

    -- status functions
    db conn                >>= print
    user conn              >>= print
    host conn              >>= print
    port conn              >>= print
    status conn            >>= print
    transactionStatus conn >>= print
    protocolVersion conn   >>= print
    serverVersion conn     >>= print

    s <- status conn
    unless (s == ConnectionOk) exitFailure

    finish conn

testPipeline :: BS8.ByteString -> IO ()
testPipeline connstring = do
    conn <- connectdb connstring

    pipelineStatus conn    >>= print

    setnonblocking conn True `shouldReturn` True
    enterPipelineMode conn `shouldReturn` True
    pipelineStatus conn `shouldReturn` PipelineOn
    sendQueryParams conn (BS8.pack "select 1") [] Text `shouldReturn` True
    sendQueryParams conn (BS8.pack "select 2") [] Text `shouldReturn` True
    pipelineSync conn `shouldReturn` True

    Just r1 <- getResult conn
    resultStatus r1 `shouldReturn` TuplesOk
    getvalue r1 0 0 `shouldReturn` Just (BS8.pack "1")
    Nothing <- getResult conn

    Just r2 <- getResult conn
    getvalue r2 0 0 `shouldReturn` Just (BS8.pack "2")
    Nothing <- getResult conn

    Just r3 <- getResult conn
    resultStatus r3 `shouldReturn` PipelineSync

    finish conn
  where
    shouldBe r value =
      unless (r == value) $ do
        print $ "expected " <> show value <> ", got " <> show r
        exitFailure
    shouldReturn action value = do
      r <- action
      r `shouldBe` value
