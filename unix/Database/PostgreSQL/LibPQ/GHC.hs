------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.LibPQ.GHC
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- On Unix,  this re-implements some of blocking libpq functions in terms of
-- on-blocking libpq operations and GHC's IO manager.   This allows
-- these operations to be interrupted by asynchronous exceptions,  and means
-- that GHC's runtime is responsible for handling the concurrency instead
-- of the OS kernel.
--
-- This module also re-exports the rest of libpq for convenience's sake.
-- Thus taking advantage of these features should be as simple as importing
-- @LibPQ.GHC@ instead of @LibPQ@.
--
-- On Windows,  this just re-exports the vanilla libpq bindings,  due to
-- the lack of a satisfactory IO manager on that platform.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.LibPQ.GHC
    ( module Database.PostgreSQL.LibPQ
    , exec
    , connectdb
    ) where

import Database.PostgreSQL.LibPQ hiding (exec, connectdb)
import qualified Data.ByteString as B
import qualified Database.PostgreSQL.LibPQ as LibPQ

exec :: Connection -> B.ByteString -> IO (Maybe Result)
exec = LibPQ.exec

connectdb :: B.ByteString -> IO Connection
connectdb = LibPQ.connectdb
