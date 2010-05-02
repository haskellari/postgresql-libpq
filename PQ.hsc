{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module PQ (connect, reset, exec)
where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import GHC.Conc ( threadWaitRead, threadWaitWrite)
import System.Posix.Types ( Fd(..) )

#include <libpq-fe.h>

data PGconn
data PGresult


data ConnStatus = ConnectionOk
                | ConnectionBad
                | ConnectionStarted
                | ConnectionMade
                | ConnectionAwaitingResponse
                | ConnectionAuthOk
                | ConnectionSetEnv
                | ConnectionSSLStartup
                | ConnectionNeeded
                | ConnectionOther CInt

data PollingStatus = PollingFailed
                   | PollingReading
                   | PollingWriting
                   | PollingOk


--newtype Connection = Connection (ForeignPtr PGconn) deriving (Eq, Show)
--newtype Result = Result (ForeignPtr PGresult) deriving (Eq, Show)


connect :: String -> IO (ForeignPtr PGconn)
connect connStr = do
  connPtr <- pqConnectStart connStr
  connectPoll connPtr
  newForeignPtr c_PQfinish connPtr
    where
      connectPoll connPtr = do
        status <- pqConnectPoll connPtr
        case status of
          PollingReading -> connWaitRead connPtr connectPoll
          PollingOk      -> return ()
          PollingFailed  -> pqErrorMessage connPtr >>= fail
          PollingWriting -> connWaitWrite connPtr connectPoll


reset :: ForeignPtr PGconn -> IO ()
reset conn =
    withForeignPtr conn $ \connPtr -> do
      pqResetStart connPtr
      resetPoll connPtr
    where
      resetPoll connPtr = do
        status <- pqResetPoll connPtr
        case status of
          PollingReading -> connWaitRead connPtr resetPoll
          PollingOk      -> return ()
          PollingFailed  -> pqErrorMessage connPtr >>= fail
          PollingWriting -> connWaitWrite connPtr resetPoll


exec :: ForeignPtr PGconn -> String -> IO [ForeignPtr PGresult]
exec conn query =
    withForeignPtr conn $ \connPtr -> do
      pqSendQuery connPtr query
      pqFlush connPtr
      threadWaitRead $ pqSocket connPtr
      getResults connPtr []

    where
      getResults connPtr results = do
        readData connPtr
        result <- pqGetResult connPtr
        case result of
          Nothing -> return results
          Just r -> getResults connPtr (r:results)

      readData connPtr = do
        pqConsumeInput connPtr
        busy <- pqIsBusy connPtr
        if busy
          then connWaitRead connPtr readData
          else return ()


connWaitRead :: Ptr PGconn -> (Ptr PGconn -> IO a) -> IO a
connWaitRead connPtr ioa = do
  threadWaitRead $ pqSocket connPtr
  ioa connPtr

connWaitWrite :: Ptr PGconn -> (Ptr PGconn -> IO a) -> IO a
connWaitWrite connPtr ioa = do
  threadWaitWrite $ pqSocket connPtr
  ioa connPtr


pqConnectStart :: String -> IO (Ptr PGconn)
pqConnectStart connStr = do
  connPtr <- withCString connStr c_PQconnectStart
  if connPtr == nullPtr
    then fail $ "PQconnectStart failed to allocate memory for a new connection"
    else return connPtr

pqStatus :: Ptr PGconn -> ConnStatus
pqStatus connPtr =
    case c_PQstatus connPtr of
      (#const CONNECTION_OK)                -> ConnectionOk
      (#const CONNECTION_BAD)               -> ConnectionBad
      (#const CONNECTION_STARTED)           -> ConnectionStarted
      (#const CONNECTION_MADE)              -> ConnectionMade
      (#const CONNECTION_AWAITING_RESPONSE) -> ConnectionAwaitingResponse
      (#const CONNECTION_AUTH_OK)           -> ConnectionAuthOk
      (#const CONNECTION_SETENV)            -> ConnectionSetEnv
      (#const CONNECTION_SSL_STARTUP)       -> ConnectionSSLStartup
      (#const CONNECTION_NEEDED)            -> ConnectionNeeded
      code                                  -> ConnectionOther code

pqSocket :: Ptr PGconn -> Fd
pqSocket = Fd . c_PQsocket


pqConnectPoll :: Ptr PGconn -> IO PollingStatus
pqConnectPoll connPtr = do
  code <- c_PQconnectPoll connPtr
  case code of
    (#const PGRES_POLLING_OK)      -> return PollingOk
    (#const PGRES_POLLING_FAILED)  -> return PollingFailed
    (#const PGRES_POLLING_READING) -> return PollingReading
    (#const PGRES_POLLING_WRITING) -> return PollingWriting
    _ -> fail $ "PQconnectPoll returned " ++ show code


pqResetStart :: Ptr PGconn -> IO ()
pqResetStart connPtr = do
  result <- c_PQresetStart connPtr
  case result of
    1 -> return ()
    _ -> fail $ "PQresetStart returned " ++ show result


pqResetPoll :: Ptr PGconn -> IO PollingStatus
pqResetPoll connPtr = do
  code <- c_PQresetPoll connPtr
  case code of
    (#const PGRES_POLLING_OK)      -> return PollingOk
    (#const PGRES_POLLING_FAILED)  -> return PollingFailed
    (#const PGRES_POLLING_READING) -> return PollingReading
    (#const PGRES_POLLING_WRITING) -> return PollingWriting
    _ -> fail $ "PQresetPoll returned " ++ show code



pqSendQuery :: Ptr PGconn -> String -> IO ()
pqSendQuery connPtr query = do
  stat <- withCString query (c_PQsendQuery connPtr)
  case stat of
    1 -> return ()
    _ -> pqErrorMessage connPtr >>= fail


pqFlush :: Ptr PGconn -> IO ()
pqFlush connPtr = do
  stat <- c_PQflush connPtr
  case stat of
    0 -> return ()
    1 -> connWaitWrite connPtr pqFlush
    _ -> pqErrorMessage connPtr >>= fail


pqConsumeInput :: Ptr PGconn -> IO ()
pqConsumeInput connPtr = do
  stat <- c_PQconsumeInput connPtr
  case stat of
    1 -> return ()
    _ -> pqErrorMessage connPtr >>= fail


pqIsBusy :: Ptr PGconn -> IO Bool
pqIsBusy connPtr = do
  stat <- c_PQisBusy connPtr
  case stat of
    1 -> return True
    0 -> return False
    _ -> fail $ "PQisBusy returned unexpected result " ++ show stat


pqErrorMessage :: Ptr PGconn -> IO String
pqErrorMessage = peekCString . c_PQerrorMessage


pqGetResult :: Ptr PGconn ->IO (Maybe (ForeignPtr PGresult))
pqGetResult connPtr = do
  resPtr <- c_PQgetResult connPtr
  if resPtr == nullPtr
    then return Nothing
    else Just `fmap` newForeignPtr c_PQclear resPtr



foreign import ccall unsafe "libpq-fe.h PQstatus"
    c_PQstatus :: Ptr PGconn -> CInt

foreign import ccall unsafe "libpq-fe.h PQsocket"
    c_PQsocket :: Ptr PGconn -> CInt

foreign import ccall unsafe "libpq-fe.h PQerrorMessage"
    c_PQerrorMessage :: Ptr PGconn -> CString

foreign import ccall unsafe "libpq-fe.h PQconnectStart"
    c_PQconnectStart :: CString ->IO (Ptr PGconn)

foreign import ccall unsafe "libpq-fe.h PQconnectPoll"
    c_PQconnectPoll :: Ptr PGconn ->IO CInt

foreign import ccall unsafe "libpq-fe.h PQresetStart"
    c_PQresetStart :: Ptr PGconn ->IO CInt

foreign import ccall unsafe "libpq-fe.h PQresetPoll"
    c_PQresetPoll :: Ptr PGconn ->IO CInt

foreign import ccall unsafe "libpq-fe.h &PQfinish"
    c_PQfinish :: FunPtr (Ptr PGconn -> IO ())

foreign import ccall unsafe "libpq-fe.h PQsendQuery"
    c_PQsendQuery :: Ptr PGconn -> CString ->IO CInt

foreign import ccall unsafe "libpq-fe.h PQflush"
    c_PQflush :: Ptr PGconn ->IO CInt

foreign import ccall unsafe "libpq-fe.h PQconsumeInput"
    c_PQconsumeInput :: Ptr PGconn ->IO CInt

foreign import ccall unsafe "libpq-fe.h PQisBusy"
    c_PQisBusy :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQgetResult"
    c_PQgetResult :: Ptr PGconn ->IO (Ptr PGresult)

foreign import ccall unsafe "libpq-fe.h &PQclear"
    c_PQclear :: FunPtr (Ptr PGresult ->IO ())



--int PQconsumeInput(PGconn *conn);
--int PQisBusy(PGconn *conn);

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


