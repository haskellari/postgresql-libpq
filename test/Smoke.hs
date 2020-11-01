module Main (main) where

import Database.PostgreSQL.LibPQ
import System.Environment (getEnvironment)

import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = do
    env <- getEnvironment
    case lookup "DATABASE_CONNSTRING" env of
        Nothing -> putStrLn "Set DATABASE_CONNSTRING environment variable"
        Just s  -> smoke (BS8.pack (special s))
  where
    -- https://www.appveyor.com/docs/services-databases/
    special "appveyor" = "dbname='TestDb' user='postgres' password='Password12!'"
    special "travis"   = ""
    special s          = s

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

    finish conn
