{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module PQ (Connection, connect, reset)
where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import GHC.Conc ( threadWaitRead, threadWaitWrite)
import System.Posix.Types ( Fd(..) )

#include <libpq-fe.h>

newtype Connection = Connection (ForeignPtr PGconn) deriving (Eq, Ord, Show)


data PGconn
data PGresult

foreign import ccall unsafe "libpq-fe.h PQconnectStart"
    c_PQconnectStart :: CString -> IO (Ptr PGconn)

foreign import ccall unsafe "libpq-fe.h PQconnectPoll"
    c_PQconnectPoll :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQsocket"
    c_PQsocket :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQresetStart"
    c_PQresetStart :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQresetPoll"
    c_PQresetPoll :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h &PQfinish"
    c_PQfinish :: FunPtr (Ptr PGconn -> IO ())



--void PQfinish(PGconn *conn);

-- int PQsendQuery(PGconn *conn, const char *command);

-- int PQsendQueryParams(PGconn *conn,
--                               const char *command,
--                               int nParams,
--                               const Oid *paramTypes,
--                               const char * const *paramValues,
--                               const int *paramLengths,
--                               const int *paramFormats,
--                               int resultFormat);

-- int PQsendPrepare(PGconn *conn,
--                           const char *stmtName,
--                           const char *query,
--                           int nParams,
--                           const Oid *paramTypes);

-- int PQsendQueryPrepared(PGconn *conn,
--                                 const char *stmtName,
--                                 int nParams,
--                                 const char * const *paramValues,
--                                 const int *paramLengths,
--                                 const int *paramFormats,
--                                 int resultFormat);

-- PGresult *PQgetResult(PGconn *conn);




connWaitRead :: Ptr PGconn -> IO a -> IO a
connWaitRead pgconn ioa = do
  fd <- Fd `fmap` c_PQsocket pgconn
  threadWaitRead fd
  ioa

connWaitWrite :: Ptr PGconn -> IO a -> IO a
connWaitWrite pgconn ioa = do
  fd <- Fd `fmap` c_PQsocket pgconn
  threadWaitWrite fd
  ioa


connect :: String -> IO Connection
connect s = do
  cs <- newCString s
  conn <- (newForeignPtr c_PQfinish) =<< c_PQconnectStart cs
  withForeignPtr conn poll
  return $ Connection conn
    where
      poll pgconn =
          do code <- c_PQconnectPoll pgconn
             case code of
               (#const PGRES_POLLING_OK) -> return ()
               (#const PGRES_POLLING_FAILED) -> fail "PGRES_POLLING_FAILED"
               (#const PGRES_POLLING_READING) ->
                   connWaitRead pgconn (poll pgconn)
               (#const PGRES_POLLING_WRITING) ->
                   connWaitWrite pgconn (poll pgconn)
               _ -> fail $ "PQconnectPoll returned " ++ show code

reset :: Connection -> IO ()
reset (Connection conn) =
    withForeignPtr conn $ \pgconn -> do
      result <- c_PQresetStart pgconn
      case result of
        1 -> poll pgconn
        _ -> fail $ "PQresetStart returned " ++ show result

    where
      poll pgconn = do
        code <- c_PQresetPoll pgconn
        case code of
          (#const PGRES_POLLING_OK) -> return ()
          (#const PGRES_POLLING_FAILED) -> fail "PGRES_POLLING_FAILED"
          (#const PGRES_POLLING_READING) -> connWaitRead pgconn (poll pgconn)
          (#const PGRES_POLLING_WRITING) -> connWaitWrite pgconn (poll pgconn)
          _ -> fail $ "PQresetPoll returned " ++ show code


