{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.PostgreSQL.LibPQ.Connect where

#include "hs-libpq.h"

import Foreign (Storable (..), nullPtr)
import Foreign.C (CInt)

import qualified Data.ByteString as B

-------------------------------------------------------------------------------
-- ConninfoOption
-------------------------------------------------------------------------------

-- typedef struct
-- {
--     char   *keyword;   /* The keyword of the option */
--     char   *envvar;    /* Fallback environment variable name */
--     char   *compiled;  /* Fallback compiled in default value */
--     char   *val;       /* Option's current value, or NULL */
--     char   *label;     /* Label for field in connect dialog */
--     char   *dispchar;  /* Indicates how to display this field
--                           in a connect dialog. Values are:
--                           ""        Display entered value as is
--                           "*"       Password field - hide value
--                           "D"       Debug option - don't show by default */
--     int     dispsize;  /* Field size in characters for dialog */
-- } PQconninfoOption;
data ConninfoOption = ConninfoOption {
      conninfoKeyword  :: B.ByteString -- ^ The keyword of the option
    , conninfoEnvVar   :: Maybe B.ByteString -- ^ Fallback environment variable name
    , conninfoCompiled :: Maybe B.ByteString -- ^ Fallback compiled in default value
    , conninfoValue    :: Maybe B.ByteString -- ^ Option's current value, or NULL
    , conninfoLabel    :: B.ByteString -- ^ Label for field in connect dialog
      -- | Indicates how to display this field in a connect dialog. Values are:
      --     ""   Display entered value as is
      --     "*"  Password field - hide value
      --     "D"  Debug option - don't show by default
    , conninfoDispChar :: B.ByteString
    , conninfoDispSize :: CInt -- ^ Field size in characters for dialog
    }
    deriving Show

instance Storable ConninfoOption where
  sizeOf _ = #{size PQconninfoOption}

  alignment _ = #{alignment PQconninfoOption}

  peek ptr = do
      conninfoKeyword <- B.packCString =<< #{peek PQconninfoOption, keyword} ptr
      conninfoEnvVar <- do
        p <- #{peek PQconninfoOption, envvar} ptr
        if p == nullPtr then pure Nothing else Just <$> B.packCString p
      conninfoCompiled <- do
        p <- #{peek PQconninfoOption, compiled} ptr
        if p == nullPtr then pure Nothing else Just <$> B.packCString p
      conninfoValue <- do
        p <- #{peek PQconninfoOption, val} ptr
        if p == nullPtr then pure Nothing else Just <$> B.packCString p
      conninfoLabel <- B.packCString =<< #{peek PQconninfoOption, label} ptr
      conninfoDispChar <- B.packCString =<< #{peek PQconninfoOption, dispchar} ptr
      conninfoDispSize <- #{peek PQconninfoOption, dispsize} ptr
      return $! ConninfoOption{..}

  poke ptr ConninfoOption{..} = do
      B.useAsCString conninfoKeyword $ \keyword ->
        maybe ($ nullPtr) B.useAsCString conninfoEnvVar $ \envvar ->
          maybe ($ nullPtr) B.useAsCString conninfoCompiled $ \compiled ->
            maybe ($ nullPtr) B.useAsCString conninfoValue $ \value ->
              B.useAsCString conninfoLabel $ \label ->
                B.useAsCString conninfoDispChar $ \dispchar -> do
                  #{poke PQconninfoOption, keyword} ptr keyword
                  #{poke PQconninfoOption, envvar} ptr envvar
                  #{poke PQconninfoOption, compiled} ptr compiled
                  #{poke PQconninfoOption, val} ptr value
                  #{poke PQconninfoOption, label} ptr label
                  #{poke PQconninfoOption, dispchar} ptr dispchar
                  #{poke PQconninfoOption, dispsize} ptr conninfoDispSize

-------------------------------------------------------------------------------
-- PQconninfoOption
-------------------------------------------------------------------------------

data PQconninfoOption

pqConninfoOptionKeyword :: Int
pqConninfoOptionKeyword = #{offset PQconninfoOption, keyword}
