module Main (main) where

import Control.Monad (unless)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCaseSteps, assertEqual)
import Database.PostgreSQL.LibPQ
import Data.Foldable (toList)
import System.Environment (getEnvironment)
import System.Exit (exitFailure)

import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = do
    libpqVersion >>= print
    withConnstring $ \connString -> defaultMain $ testGroup "postgresql-libpq"
        [ testCaseSteps "smoke" $ \info -> smoke info connString
        ]

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

smoke :: (String -> IO ()) -> BS8.ByteString -> IO ()
smoke info connstring = do
    let infoShow x = info (show x)

    conn <- connectdb connstring

    -- status functions
    db conn                >>= infoShow
    user conn              >>= infoShow
    host conn              >>= infoShow
    port conn              >>= infoShow
    status conn            >>= infoShow
    transactionStatus conn >>= infoShow
    protocolVersion conn   >>= infoShow
    serverVersion conn     >>= infoShow

    s <- status conn
    assertEqual "connection not ok" s ConnectionOk

    finish conn
