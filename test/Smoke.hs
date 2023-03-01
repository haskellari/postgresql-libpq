module Main (main) where

import Control.Monad (unless)
import Database.PostgreSQL.LibPQ
import Data.Foldable (toList)
import System.Environment (getEnvironment)
import System.Exit (exitFailure)

import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = do
    libpqVersion >>= print
    withConnstring smoke

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
    pipelineStatus conn    >>= print

    s <- status conn
    unless (s == ConnectionOk) exitFailure

    finish conn
