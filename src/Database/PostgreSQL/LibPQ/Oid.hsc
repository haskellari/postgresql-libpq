{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.PostgreSQL.LibPQ.Oid where

#include <libpq-fe.h>

import Foreign.C.Types (CUInt)
import Foreign.Storable (Storable)

newtype Oid = Oid CUInt
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (Storable)

invalidOid :: Oid
invalidOid = Oid (#const InvalidOid)
