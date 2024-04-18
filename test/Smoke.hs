{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad             (unless)
import Data.Foldable             (toList)
import Database.PostgreSQL.LibPQ
import System.Environment        (getEnvironment)
import System.Exit               (exitFailure)
import Test.Tasty                (defaultMain, testGroup)
import Test.Tasty.HUnit          (assertEqual, testCaseSteps)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = do
    libpqVersion >>= print
    withConnstring $ \connString -> defaultMain $ testGroup "postgresql-libpq"
        [ testCaseSteps "smoke" $ smoke connString
        , testCaseSteps "issue54" $ issue54 connString
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

smoke :: BS8.ByteString -> (String -> IO ()) -> IO ()
smoke connstring info = do
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
    assertEqual "connection not ok" ConnectionOk s

    finish conn

issue54 :: BS8.ByteString -> (String -> IO ()) -> IO ()
issue54 connString info = do
    conn <- connectdb connString

    Just result <- execParams conn
        "SELECT ($1 :: bytea), ($2 :: bytea)"
        [Just (Oid 17,"",Binary), Just (Oid 17,BS.empty,Binary)]
        Binary
    s <- resultStatus result
    assertEqual "result status" TuplesOk s

    -- ntuples result >>= info . show
    -- nfields result >>= info . show

    null1 <- getisnull result 0 0
    null2 <- getisnull result 0 1
    assertEqual "fst not null" False null1
    assertEqual "snd not null" False null2

    Just val1 <- getvalue result 0 0
    Just val2 <- getvalue result 0 1

    assertEqual "fst not null" BS.empty val1
    assertEqual "snd not null" BS.empty val2
