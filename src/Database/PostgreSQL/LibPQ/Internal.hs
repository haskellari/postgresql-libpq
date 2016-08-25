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

{-# LANGUAGE BangPatterns, EmptyDataDecls #-}

module Database.PostgreSQL.LibPQ.Internal where

import Foreign
import Control.Concurrent.MVar ( MVar )


-- | 'Connection' encapsulates a connection to the backend.
data Connection = Conn {-# UNPACK #-} !(ForeignPtr PGconn)
                       {-# UNPACK #-} !(MVar NoticeBuffer)

instance Eq Connection where
    (Conn c _) == (Conn d _) = c == d
    (Conn c _) /= (Conn d _) = c /= d

withConn :: Connection
         -> (Ptr PGconn -> IO b)
         -> IO b
withConn (Conn !fp _) f = withForeignPtr fp f
{-# INLINE withConn #-}

data PGconn

data CNoticeBuffer
type NoticeBuffer = Ptr CNoticeBuffer
