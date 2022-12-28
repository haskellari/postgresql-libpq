{-# LANGUAGE CPP #-}
module Database.PostgreSQL.LibPQ.Marshal where

import Foreign (Ptr,nullPtr,Storable,allocaArray,pokeArray)

unsafeWithArray :: Storable a => Int -> [a] -> (Ptr a -> IO b) -> IO b
unsafeWithArray len vals f =
#if 0
  if len /= length vals then error "unsafeWithArray: len mismatch" else
#endif
  allocaArray len $ \ptr -> do
      pokeArray ptr vals
      f ptr

-- | Like maybe with but takes an int. Usable with 'withArrayLen'.
-- In 'Nothing' case uses 0 and 'nullPtr'.
maybeWithInt :: (     a -> (Int -> Ptr b -> IO c) -> IO c)
          -> (Maybe a -> (Int -> Ptr b -> IO c) -> IO c)
maybeWithInt = maybe (\f -> f 0 nullPtr)
