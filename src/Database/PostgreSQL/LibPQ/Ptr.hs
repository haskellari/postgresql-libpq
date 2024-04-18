{-# LANGUAGE MagicHash #-}
module Database.PostgreSQL.LibPQ.Ptr (emptyPtr) where

import GHC.Ptr (Ptr (..))

emptyPtr :: Ptr a
emptyPtr = Ptr ""#
