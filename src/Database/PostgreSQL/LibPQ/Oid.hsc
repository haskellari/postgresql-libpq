{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.PostgreSQL.LibPQ.Oid where

#include <libpq-fe.h>

import Data.Typeable (Typeable)
import Foreign.C.Types (CUInt)
import Foreign.Storable (Storable)

newtype Oid = Oid CUInt deriving (Eq, Ord, Read, Show, Storable, Typeable)

invalidOid :: Oid
invalidOid = Oid (#const InvalidOid)
