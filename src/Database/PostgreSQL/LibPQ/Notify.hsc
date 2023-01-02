module Database.PostgreSQL.LibPQ.Notify where

#include <libpq-fe.h>
#include "noticehandlers.h"

import Foreign (Ptr, Storable (..))
import Foreign.C.Types (CInt, CSize)
import System.Posix.Types (CPid)

import qualified Data.ByteString as B

-------------------------------------------------------------------------------
-- Notify
-------------------------------------------------------------------------------

data Notify = Notify {
      notifyRelname :: {-# UNPACK #-} !B.ByteString -- ^ notification channel name
    , notifyBePid   :: {-# UNPACK #-} !CPid         -- ^ process ID of notifying server process
    , notifyExtra   :: {-# UNPACK #-} !B.ByteString -- ^ notification payload string
    } deriving Show

instance Storable Notify where
  sizeOf _ = #{size PGnotify}

  alignment _ = #{alignment PGnotify}

  peek ptr = do
      relname <- B.packCString =<< #{peek PGnotify, relname} ptr
      extra   <- B.packCString =<< #{peek PGnotify, extra} ptr
      be_pid  <- fmap f $ #{peek PGnotify, be_pid} ptr
      return $! Notify relname be_pid extra
      where
        f :: CInt -> CPid
        f = fromIntegral

  poke ptr (Notify a b c) =
      B.useAsCString a $ \a' ->
        B.useAsCString c $ \c' ->
            do #{poke PGnotify, relname} ptr a'
               #{poke PGnotify, be_pid}  ptr (fromIntegral b :: CInt)
               #{poke PGnotify, extra}   ptr c'

-------------------------------------------------------------------------------
-- Notice
-------------------------------------------------------------------------------

data PGnotice

pgNoticePeekLen :: Ptr PGnotice -> IO CSize
pgNoticePeekLen = #{peek PGnotice, len}

pgNoticeOffsetStr :: Int
pgNoticeOffsetStr = #{offset PGnotice, str}
