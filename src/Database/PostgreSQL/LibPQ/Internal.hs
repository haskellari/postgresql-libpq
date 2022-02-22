-----------------------------------------------------------------------------
-- |
-- Module      :  Database.PostgreSQL.LibPQ.Internal
-- Copyright   :  (c) 2010 Grant Monroe,
--                (c) 2011 Leon P Smith
-- License     :  BSD3
--
-- This module exports the data constructor for the database connection
-- object so that people may create their own foreign bindings to libpq
-- functions that may not exist yet in vanilla postgresql-libpq.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE EmptyDataDecls #-}

module Database.PostgreSQL.LibPQ.Internal where

import Control.Concurrent.MVar (MVar)
import Foreign


-- | 'Connection' encapsulates a connection to the backend.
data Connection = Conn {-# UNPACK #-} !(ForeignPtr PGconn)
                       {-# UNPACK #-} !(MVar NoticeBuffer)
                       {-# UNPACK #-} !(MVar ()) -- ^ This MVar is filled finishonly once the connection is fully closed (@PQfinish()@ completed on it). See comment in `Database.PostgreSQL.LibPQ.finish`.

instance Eq Connection where
    (Conn c _ _) == (Conn d _ _) = c == d
    (Conn c _ _) /= (Conn d _ _) = c /= d

withConn :: Connection
         -> (Ptr PGconn -> IO b)
         -> IO b
withConn (Conn !fp _ _) f = withForeignPtr fp f
{-# INLINE withConn #-}

data PGconn

data CNoticeBuffer
type NoticeBuffer = Ptr CNoticeBuffer
