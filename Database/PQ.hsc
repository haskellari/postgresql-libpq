{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, OverloadedStrings, ScopedTypeVariables #-}

-- | This incomplete binding uses the libpq asynchronous API in
-- combination with threadWaitRead and threadWaitWrite to take
-- advantage of the I/O manager. exec, execParams, prepare, and
-- execPrepared are built from asynchronous primitives.
module Database.PQ
    (
    -- * Overview
    -- $overview

    -- * Database Connection Control Functions
      Connection
    --, connectdb
    , connectStart
    , connectPoll
    --, reset
    , resetStart
    , resetPoll
    , PollingStatus(..)
    , finish

    -- * Connection Status Functions
    , db
    , user
    , pass
    , host
    , port
    , options
    , ConnStatus(..)
    , status
    --, transactionStatus
    --, parameterStatus
    --, protocolVersion
    --, serverVersion
    , errorMessage
    , socket
    , backendPID
    --, connectionNeedsPassword
    --, connectionUsedPassword
    --, getssl


    -- * Command Execution Functions
    , Result
    --, exec
    , Format(..)
    , Oid
    --, execParams
    --, prepare
    --, execPrepared
    --, describePrepared
    --, describePortal
    , ResultStatus(..)
    , resultStatus
    --, resStatus
    , resultErrorMessage
    --, resultErrorField

    -- * Retrieving Query Result Information
    , ntuples
    , nfields
    , fname
    , fnumber
    , ftable
    , ftablecol
    , fformat
    , ftype
    , fmod
    , fsize
    , getvalue
    , getisnull
    , getlength
    --, nparams
    --, paramtype
    , print
    , PrintOpt(..)
    , defaultPrintOpt

    -- Retrieving Result Information for Other Commands
    --, cmdStatus
    --, cmdTuples
    --, oidValue
    --, oidStatus

    -- * Escaping Strings for Inclusion in SQL Commands
    , escapeStringConn

    -- * Escaping Binary Strings for Inclusion in SQL Commands
    , escapeByteaConn
    --, unescapeBytea

    -- * Asynchronous Command Processing
    , sendQuery
    , sendQueryParams
    , sendPrepare
    , sendQueryPrepared
    --, sendDescribePrepared
    --, sendDescribePortal
    , getResult
    , consumeInput
    , isBusy
    , setnonblocking
    , isnonblocking
    , flush

    -- * Control Functions
    --, clientEncoding
    , setClientEncoding
    --, setErrorVerbosity
    --, trace
    --, untrace
    )
where

#include <libpq-fe.h>

import Prelude hiding ( print )
import Control.Monad ( when )
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import GHC.Conc ( -- threadWaitRead
                 threadWaitWrite)
import System.Posix.Types ( Fd(..) )
import Data.List ( foldl' )
import System.IO ( Handle )
import GHC.Handle ( hDuplicate )
import System.Posix.IO ( handleToFd )
import Control.Concurrent.MVar
import System.Posix.Types ( CPid )

import Data.ByteString.Char8 ()
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B (fromForeignPtr)
import qualified Data.ByteString as B

data Format = Text | Binary deriving Enum

-- | 'Connection' encapsulates a connection to the backend.
newtype Connection = Conn (MVar (Maybe (ForeignPtr PGconn))) deriving Eq
data PGconn

-- | 'Result' encapsulates the result of a query (or more precisely,
-- of a single SQL command --- a query string given to 'sendQuery' can
-- contain multiple commands and thus return multiple instances of
-- 'Result'.
newtype Result = Result (ForeignPtr PGresult) deriving (Eq, Show)
data PGresult


-- | Obtains the file descriptor number of the connection socket to
-- the server.
socket :: Connection
       -> IO (Maybe Fd)
socket connection =
    do cFd <- withConn connection c_PQsocket
       return $ case cFd of
                  -1 -> Nothing
                  _ -> Just $ Fd cFd


-- | Returns the database name of the connection.
db :: Connection
   -> IO B.ByteString
db = statusString c_PQdb


-- | Returns the user name of the connection.
user :: Connection
     -> IO B.ByteString
user = statusString c_PQuser


-- | Returns the password of the connection.
pass :: Connection
     -> IO B.ByteString
pass = statusString c_PQpass


-- | Returns the server host name of the connection.
host :: Connection
     -> IO B.ByteString
host = statusString c_PQhost


-- | Returns the port of the connection.
port :: Connection
     -> IO B.ByteString
port = statusString c_PQport


-- | Returns the command-line options passed in the connection request.
options :: Connection
        -> IO B.ByteString
options = statusString c_PQoptions


data ConnStatus
    = ConnectionOk                 -- ^ The 'Connection' is ready.
    | ConnectionBad                -- ^ The connection procedure has failed.
    | ConnectionStarted            -- ^ Waiting for connection to be made.
    | ConnectionMade               -- ^ Connection OK; waiting to send.
    | ConnectionAwaitingResponse   -- ^ Waiting for a response from the server.
    | ConnectionAuthOk             -- ^ Received authentication;
                                   -- waiting for backend start-up to
                                   -- finish.
    | ConnectionSetEnv             -- ^ Negotiating environment-driven
                                   -- parameter settings.
    | ConnectionSSLStartup         -- ^ Negotiating SSL encryption.
    | ConnectionOther Int          -- ^ Unknown connection state
      deriving Show


-- | Returns the status of the connection.
--
-- The status can be one of a number of values. However, only two of
-- these are seen outside of an asynchronous connection procedure:
-- 'ConnectionOk' and 'ConnectionBad'. A good connection to the
-- database has the status 'ConnectionOk'. A failed connection attempt
-- is signaled by status 'ConnectionBad'. Ordinarily, an OK status
-- will remain so until 'finish', but a communications failure might
-- result in the status changing to 'ConnectionBad' prematurely. In
-- that case the application could try to recover by calling 'reset'.
--
-- See the entry for 'connectStart' and 'connectPoll' with regards to
-- other status codes that might be seen.
status :: Connection
       -> IO ConnStatus
status connection =
    withConn connection (return . status')
    where
      status' connPtr =
          case c_PQstatus connPtr of
            (#const CONNECTION_OK)               -> ConnectionOk
            (#const CONNECTION_BAD)              -> ConnectionBad
            (#const CONNECTION_STARTED)          -> ConnectionStarted
            (#const CONNECTION_MADE)             -> ConnectionMade
            (#const CONNECTION_AWAITING_RESPONSE)-> ConnectionAwaitingResponse
            (#const CONNECTION_AUTH_OK)          -> ConnectionAuthOk
            (#const CONNECTION_SETENV)           -> ConnectionSetEnv
            (#const CONNECTION_SSL_STARTUP)      -> ConnectionSSLStartup
            --(#const CONNECTION_NEEDED)           -> ConnectionNeeded
            c                                    -> ConnectionOther $ fromEnum c

-- | Returns the error message most recently generated by an operation
-- on the connection.
--
-- Nearly all libpq functions will set a message for 'errorMessage' if
-- they fail. Note that by libpq convention, a nonempty 'errorMessage'
-- result can be multiple lines, and will include a trailing
-- newline. The result string should not be expected to remain the
-- same across operations on the 'Connection'.
errorMessage :: Connection
             -> IO B.ByteString
errorMessage = statusString c_PQerrorMessage


-- | Helper function that checks for nullPtrs and returns the empty
-- string.
statusString :: (Ptr PGconn -> IO CString)
             -> Connection
             -> IO B.ByteString
statusString f connection =
    withConn connection $ \ptr ->
        do cstr <- f ptr
           if cstr == nullPtr
             then return ""
             else B.packCString cstr


-- | Returns the process 'CPid' of the backend server process
-- handling this connection.
--
-- The backend PID is useful for debugging purposes and for comparison
-- to NOTIFY messages (which include the PID of the notifying backend
-- process). Note that the PID belongs to a process executing on the
-- database server host, not the local host!
backendPID :: Connection
           -> IO CPid
backendPID connection =
    fmap fromIntegral $ withConn connection c_PQbackendPID


-- | Escapes a string for use within an SQL command. This is useful
-- when inserting data values as literal constants in SQL
-- commands. Certain characters (such as quotes and backslashes) must
-- be escaped to prevent them from being interpreted specially by the
-- SQL parser.
escapeStringConn :: Connection
                 -> B.ByteString
                 -> IO (Maybe B.ByteString)
escapeStringConn connection bs =
    withConn connection $ \conn ->
        B.unsafeUseAsCStringLen bs $ \(from, bslen) ->
            alloca $ \err -> do
              to <- mallocBytes (bslen*2+1)
              num <- c_PQescapeStringConn conn to from (fromIntegral bslen) err
              stat <- peek err
              case stat of
                0 -> do tofp <- newForeignPtr finalizerFree to
                        return $ Just $ B.fromForeignPtr tofp 0 (fromIntegral num)
                _ -> do free to
                        return Nothing


-- | Escapes binary data for use within an SQL command with the type
-- bytea. As with 'escapeStringConn', this is only used when inserting
-- data directly into an SQL command string.
escapeByteaConn :: Connection
                -> B.ByteString
                -> IO (Maybe B.ByteString)
escapeByteaConn connection bs =
    withConn connection $ \conn ->
        B.unsafeUseAsCStringLen bs $ \(from, bslen) ->
            alloca $ \to_length -> do
              to <- c_PQescapeByteaConn conn from (fromIntegral bslen) to_length
              if to == nullPtr
                then return Nothing
                else do tofp <- newForeignPtr p_PQfreemem to
                        l <- peek to_length
                        return $ Just $ B.fromForeignPtr tofp 0 ((fromIntegral l) - 1)



-- | Sets the client encoding.
setClientEncoding :: Connection -> B.ByteString -> IO Bool
setClientEncoding connection enc =
    do stat <- withConn connection $ \c ->
               B.useAsCString enc $ \s ->
                   c_PQsetClientEncoding c s

       return $ stat == 0


-- | Submits a command to the server without waiting for the
-- result(s). 'True' is returned if the command was successfully
-- dispatched and 'False' if not (in which case, use 'errorMessage' to
-- get more information about the failure).
sendQuery :: Connection
          -> B.ByteString
          -> IO Bool
sendQuery connection query =
    boolFromConn connection $ \p ->
        B.useAsCString query $ c_PQsendQuery p


-- | Submits a command and separate parameters to the server without
-- waiting for the result(s).
sendQueryParams :: Connection
                -> B.ByteString
                -> [Maybe (Oid, B.ByteString, Format)]
                -> Format
                -> IO Bool
sendQueryParams connection statement params rFmt =
    do let (oids, values, lengths, formats) =
               foldl' accum ([],[],[],[]) $ reverse params
           c_lengths = map toEnum lengths :: [CInt]
           n = toEnum $ length params
           f = toEnum $ fromEnum rFmt
       boolFromConn connection $ \c ->
           B.useAsCString statement $ \s ->
               withArray oids $ \ts ->
                   withMany (maybeWith B.useAsCString) values $ \c_values ->
                       withArray c_values $ \vs ->
                           withArray c_lengths $ \ls ->
                               withArray formats $ \fs ->
                                   c_PQsendQueryParams c s n ts vs ls fs f

    where
      accum (a,b,c,d) Nothing = ( 0:a
                                , Nothing:b
                                , 0:c
                                , 0:d
                                )
      accum (a,b,c,d) (Just (t,v,f)) = ( t:a
                                       , (Just v):b
                                       , (B.length v):c
                                       , (toEnum $ fromEnum f):d
                                       )


-- | Sends a request to create a prepared statement with the given
-- parameters, without waiting for completion.
sendPrepare :: Connection
            -> B.ByteString
            -> B.ByteString
            -> Maybe [Oid]
            -> IO Bool
sendPrepare connection stmtName query mParamTypes =
    boolFromConn connection $ \c ->
        B.useAsCString stmtName $ \s ->
            B.useAsCString query $ \q ->
                maybeWith withArray mParamTypes $ \o ->
                    let l = maybe 0 (toEnum . length) mParamTypes
                    in c_PQsendPrepare c s q l o


-- | Sends a request to execute a prepared statement with given
-- parameters, without waiting for the result(s).
sendQueryPrepared :: Connection
                  -> B.ByteString
                  -> [Maybe (B.ByteString, Format)]
                  -> Format
                  -> IO Bool
sendQueryPrepared connection stmtName mPairs rFmt =
    do let (values, lengths, formats) = foldl' accum ([],[],[]) $ reverse mPairs
           c_lengths = map toEnum lengths :: [CInt]
           n = toEnum $ length mPairs
           f = toEnum $ fromEnum rFmt
       boolFromConn connection $ \c ->
           B.useAsCString stmtName $ \s ->
               withMany (maybeWith B.useAsCString) values $ \c_values ->
                   withArray c_values $ \vs ->
                       withArray c_lengths $ \ls ->
                           withArray formats $ \fs ->
                               c_PQsendQueryPrepared c s n vs ls fs f

    where
      accum (a,b,c) Nothing       = ( Nothing:a
                                    , 0:b
                                    , 0:c
                                    )
      accum (a,b,c) (Just (v, f)) = ( (Just v):a
                                    , (B.length v):b
                                    , (toEnum $ fromEnum f):c
                                    )




-- | Make a connection to the database server in a nonblocking manner.
connectStart :: B.ByteString -- ^ Connection Info
             -> IO Connection
connectStart connStr =
    do connPtr <- B.useAsCString connStr c_PQconnectStart
       if connPtr == nullPtr
           then fail "libpq failed to allocate a PGconn structure"
           else do fp <- newForeignPtr p_PQfinish connPtr
                   mvar <- newMVar $ Just fp
                   return $ Conn mvar


-- | If 'connectStart' succeeds, the next stage is to poll libpq so
-- that it can proceed with the connection sequence. Use 'socket' to
-- obtain the 'Fd' of the socket underlying the database
-- connection. Loop thus: If 'connectPoll' last returned
-- 'PollingReading', wait until the socket is ready to read (as
-- indicated by select(), poll(), or similar system function). Then
-- call 'connectPoll' again. Conversely, if 'connectPoll' last
-- returned 'PollingWriting', wait until the socket is ready to write,
-- then call 'connectPoll' again. If you have yet to call
-- 'connectPoll', i.e., just after the call to 'connectStart', behave
-- as if it last returned 'PollingWriting'. Continue this loop until
-- 'connectPoll' returns 'PollingFailed', indicating the connection
-- procedure has failed, or 'PollingOk', indicating the connection has
-- been successfully made.
connectPoll :: Connection
            -> IO PollingStatus
connectPoll = pollHelper c_PQconnectPoll


-- | Reset the communication channel to the server, in a nonblocking manner.
resetStart :: Connection
           -> IO Bool
resetStart connection =
    boolFromConn connection c_PQresetStart


resetPoll :: Connection
          -> IO PollingStatus
resetPoll = pollHelper c_PQresetPoll

data PollingStatus
    = PollingFailed
    | PollingReading
    | PollingWriting
    | PollingOk deriving Show

pollHelper :: (Ptr PGconn -> IO CInt)
           -> Connection
           -> IO PollingStatus
pollHelper poller connection =
    do code <- withConn connection poller
       case code of
         (#const PGRES_POLLING_READING) -> return PollingReading
         (#const PGRES_POLLING_OK)      -> return PollingOk
         (#const PGRES_POLLING_WRITING) -> return PollingWriting
         (#const PGRES_POLLING_FAILED)  -> return PollingFailed
         _ -> error $ "unexpected polling status " ++ show code



data FlushStatus = FlushOk
                 | FlushFailed
                 | FlushWriting

-- | Attempts to flush any queued output data to the server. Returns
-- 'FlushOk' if successful (or if the send queue is empty),
-- 'FlushFailed' if it failed for some reason, or 'FlushWriting' if it
-- was unable to send all the data in the send queue yet (this case
-- can only occur if the connection is nonblocking).
flush :: Connection
      -> IO FlushStatus
flush connection =
    do stat <- withConn connection c_PQflush
       return $ case stat of
                  0 -> FlushOk
                  1 -> FlushWriting
                  _ -> FlushFailed


-- | If input is available from the server, consume it.
--
-- 'consumeInput' normally returns 'True' indicating "no error", but
-- returns 'False' if there was some kind of trouble (in which case
-- 'errorMessage' can be consulted). Note that the result does not say
-- whether any input data was actually collected. After calling
-- 'consumeInput', the application can check 'isBusy' and/or
-- 'notifies' to see if their state has changed.
consumeInput :: Connection
             -> IO Bool
consumeInput connection = boolFromConn connection c_PQconsumeInput


-- | Returns True if a command is busy, that is, getResult would block
-- waiting for input. A False return indicates that getResult can be
-- called with assurance of not blocking.
--
-- 'isBusy' will not itself attempt to read data from the server;
-- therefore 'consumeInput' must be invoked first, or the busy state
-- will never end.
isBusy :: Connection
       -> IO Bool
isBusy connection = boolFromConn connection c_PQisBusy


-- | Sets the nonblocking status of the connection.
setnonblocking :: Connection
               -> Bool
               -> IO Bool
setnonblocking connection blocking =
    do let arg = fromIntegral $ fromEnum blocking
       stat <- withConn connection $ \ptr -> c_PQsetnonblocking ptr arg
       return $ stat == 0


-- | Returns the blocking status of the database connection.
isnonblocking :: Connection
              -> IO Bool
isnonblocking connection = boolFromConn connection c_PQisnonblocking


-- | Waits for the next result from a prior sendQuery, sendQueryParams,
-- sendPrepare, or sendQueryPrepared call, and returns it. A null
-- pointer is returned when the command is complete and there will be
-- no more results.
getResult :: Connection
          -> IO (Maybe Result)
getResult connection =
    do resPtr <- withConn connection c_PQgetResult
       if resPtr == nullPtr
           then return Nothing
           else (Just . Result) `fmap` newForeignPtr p_PQclear resPtr


data ResultStatus = EmptyQuery
                  | CommandOk
                  | TuplesOk
                  | CopyOut
                  | CopyIn
                  | BadResponse
                  | NonfatalError
                  | FatalError deriving Show


-- | Returns the result status of the command.
resultStatus :: Result
             -> IO ResultStatus
resultStatus (Result res) =
    do code <- withForeignPtr res c_PQresultStatus
       case code of
         (#const PGRES_EMPTY_QUERY)    -> return EmptyQuery
         (#const PGRES_COMMAND_OK)     -> return CommandOk
         (#const PGRES_TUPLES_OK)      -> return TuplesOk
         (#const PGRES_COPY_OUT)       -> return CopyOut
         (#const PGRES_COPY_IN)        -> return CopyIn
         (#const PGRES_BAD_RESPONSE)   -> return BadResponse
         (#const PGRES_NONFATAL_ERROR) -> return NonfatalError
         (#const PGRES_FATAL_ERROR)    -> return FatalError
         s -> fail $ "Unexpected result from PQresultStatus" ++ show s


-- | Returns the number of rows (tuples) in the query result. Because
-- it returns an integer result, large result sets might overflow the
-- return value on 32-bit operating systems.
ntuples :: Result
        -> IO Int
ntuples (Result res) = withForeignPtr res (return . fromEnum . c_PQntuples)


-- | Returns the number of columns (fields) in each row of the query
-- result.
nfields :: Result
        -> IO Int
nfields (Result res) = withForeignPtr res (return . fromEnum . c_PQnfields)


-- | Returns the column name associated with the given column
-- number. Column numbers start at 0.
fname :: Result
      -> Int
      -> IO B.ByteString
fname result@(Result res) colNum =
      do nf <- nfields result
         when (colNum < 0 || colNum >= nf) (failure nf)
         cs <- withForeignPtr res $ (flip c_PQfname) $ toEnum colNum
         if cs == nullPtr
           then failure nf
           else B.packCString cs
    where
      failure nf = fail ("column number " ++
                         show colNum ++
                         " is out of range 0.." ++
                         show (nf - 1))


-- | Returns the column number associated with the given column name.
fnumber :: Result
        -> B.ByteString
        -> IO (Maybe Int)
fnumber (Result res) columnName =
    do num <- withForeignPtr res $ \resPtr ->
              B.useAsCString columnName $ \columnNamePtr ->
                  c_PQfnumber resPtr columnNamePtr
       return $ if num == -1
                  then Nothing
                  else Just $ fromIntegral num


-- | Returns the OID of the table from which the given column was
-- fetched. Column numbers start at 0.
ftable :: Result
       -> Int -- ^ column_number
       -> IO Oid
ftable (Result res) columnNumber =
    withForeignPtr res $ flip c_PQftable $ fromIntegral columnNumber


-- | Returns the column number (within its table) of the column making
-- up the specified query result column. Query-result column numbers
-- start at 0, but table columns have nonzero numbers.
ftablecol :: Result
          -> Int -- ^ column_number
          -> IO Int
ftablecol (Result res) columnNumber =
    fmap fromIntegral $ withForeignPtr res $ \ptr -> do
      c_PQftablecol ptr $ fromIntegral columnNumber


-- | Returns the 'Format' of the given column. Column numbers start at
-- 0.
fformat :: Result
        -> Int -- ^ column_number
        -> IO Format
fformat (Result res) columnNumber =
    fmap (toEnum . fromIntegral) $ withForeignPtr res $ \ptr -> do
      c_PQfformat ptr $ fromIntegral columnNumber


-- | Returns the data type associated with the given column
-- number. The 'Oid' returned is the internal OID number of the
-- type. Column numbers start at 0.
--
-- You can query the system table pg_type to obtain the names and
-- properties of the various data types. The OIDs of the built-in data
-- types are defined in the file src/include/catalog/pg_type.h in the
-- source tree.
ftype :: Result
      -> Int -- ^ column_number
      -> IO Oid
ftype (Result res) columnNumber =
    withForeignPtr res $ \ptr -> do
      c_PQftype ptr $ fromIntegral columnNumber


-- | Returns the type modifier of the column associated with the given
-- column number. Column numbers start at 0.
--
-- The interpretation of modifier values is type-specific; they
-- typically indicate precision or size limits. The value -1 is used
-- to indicate "no information available". Most data types do not use
-- modifiers, in which case the value is always -1.
fmod :: Result
     -> Int -- ^ column_number
     -> IO Int
fmod (Result res) columnNumber =
    fmap fromIntegral $ withForeignPtr res $ \ptr -> do
      c_PQfmod ptr $ fromIntegral columnNumber


-- | Returns the size in bytes of the column associated with the given
-- column number. Column numbers start at 0.
--
-- 'fsize' returns the space allocated for this column in a database
-- row, in other words the size of the server's internal
-- representation of the data type. (Accordingly, it is not really
-- very useful to clients.) A negative value indicates the data type
-- is variable-length.
fsize :: Result
      -> Int -- ^ column_number
      -> IO Int
fsize (Result res) columnNumber =
    fmap fromIntegral $ withForeignPtr res $ \ptr -> do
      c_PQfsize ptr $ fromIntegral columnNumber


-- | Returns a single field value of one row of a PGresult. Row and
-- column numbers start at 0.
--
-- For convenience, this binding uses 'getisnull' and 'getlength' to
-- help construct the result.
getvalue :: Result
         -> Int -- ^ row_number
         -> Int -- ^ column_number
         -> IO (Maybe B.ByteString)
getvalue (Result res) rowNumber columnNumber = do
  let row = fromIntegral rowNumber
      col = fromIntegral columnNumber
  withForeignPtr res $ \ptr -> do
    isnull <- c_PQgetisnull ptr row col
    if toEnum $ fromIntegral isnull
      then return $ Nothing

      else do cstr <- c_PQgetvalue ptr row col
              len <- c_PQgetlength ptr row col
              fmap Just $ B.packCStringLen (cstr, fromIntegral len)


-- | Tests a field for a null value. Row and column numbers start at
-- 0.
getisnull :: Result
          -> Int -- ^ row_number
          -> Int -- ^ column_number
          -> IO Bool
getisnull (Result res) rowNumber columnNumber =
    do let row = fromIntegral rowNumber
           col = fromIntegral columnNumber
       cint <- withForeignPtr res $ \ptr ->
               c_PQgetisnull ptr row col
       return . toEnum $ fromIntegral cint


-- | Returns the actual length of a field value in bytes. Row and
-- column numbers start at 0.
--
-- This is the actual data length for the particular data value, that
-- is, the size of the object pointed to by 'getvalue'. For text data
-- format this is the same as strlen(). For binary format this is
-- essential information. Note that one should not rely on 'fsize' to
-- obtain the actual data length.
getlength :: Result
          -> Int -- ^ row_number
          -> Int -- ^ column_number
          -> IO Int
getlength (Result res) rowNumber columnNumber =
    fmap fromIntegral $ withForeignPtr res $ \ptr -> do
      c_PQgetlength ptr (fromIntegral rowNumber) (fromIntegral columnNumber)


data PrintOpt = PrintOpt {
      poHeader     :: Bool -- ^ print output field headings and row count
    , poAlign      :: Bool -- ^ fill align the fields
    , poStandard   :: Bool -- ^ old brain dead format
    , poHtml3      :: Bool -- ^ output HTML tables
    , poExpanded   :: Bool -- ^ expand tables
    , poPager      :: Bool -- ^ use pager for output if needed
    , poFieldSep   :: B.ByteString   -- ^ field separator
    , poTableOpt   :: B.ByteString   -- ^ attributes for HTML table element
    , poCaption    :: B.ByteString   -- ^ HTML table caption
    , poFieldName  :: [B.ByteString] -- ^ list of replacement field names
    }


defaultPrintOpt :: PrintOpt
defaultPrintOpt = PrintOpt True True False False False False "|" "" "" []


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
instance Storable PrintOpt where
  sizeOf _ = #{size PQprintOpt}

  alignment _ = #{alignment PQprintOpt}

  peek ptr = do
      a <- fmap pqbool $ #{peek PQprintOpt, header  } ptr
      b <- fmap pqbool $ #{peek PQprintOpt, align   } ptr
      c <- fmap pqbool $ #{peek PQprintOpt, standard} ptr
      d <- fmap pqbool $ #{peek PQprintOpt, html3   } ptr
      e <- fmap pqbool $ #{peek PQprintOpt, expanded} ptr
      f <- fmap pqbool $ #{peek PQprintOpt, pager   } ptr
      g <- B.packCString =<< #{peek PQprintOpt, fieldSep} ptr
      h <- B.packCString =<< #{peek PQprintOpt, tableOpt} ptr
      i <- B.packCString =<< #{peek PQprintOpt, caption} ptr
      j <- #{peek PQprintOpt, fieldName} ptr
      j' <- peekArray0 nullPtr j
      j'' <- mapM B.packCString j'
      return $ PrintOpt a b c d e f g h i j''
      where
        pqbool :: CChar -> Bool
        pqbool = toEnum . fromIntegral

  poke ptr (PrintOpt a b c d e f g h i j) =
      B.useAsCString g $ \g' -> do
        B.useAsCString h $ \h' -> do
          B.useAsCString i $ \i' -> do
            withMany B.useAsCString j $ \j' ->
              withArray0 nullPtr j' $ \j'' -> do
                let a' = (fromIntegral $ fromEnum a)::CChar
                    b' = (fromIntegral $ fromEnum b)::CChar
                    c' = (fromIntegral $ fromEnum c)::CChar
                    d' = (fromIntegral $ fromEnum d)::CChar
                    e' = (fromIntegral $ fromEnum e)::CChar
                    f' = (fromIntegral $ fromEnum f)::CChar
                #{poke PQprintOpt, header}    ptr a'
                #{poke PQprintOpt, align}     ptr b'
                #{poke PQprintOpt, standard}  ptr c'
                #{poke PQprintOpt, html3}     ptr d'
                #{poke PQprintOpt, expanded}  ptr e'
                #{poke PQprintOpt, pager}     ptr f'
                #{poke PQprintOpt, fieldSep}  ptr g'
                #{poke PQprintOpt, tableOpt}  ptr h'
                #{poke PQprintOpt, caption}   ptr i'
                #{poke PQprintOpt, fieldName} ptr j''


-- | Prints out all the rows and, optionally, the column names to the
-- specified output stream.
--
-- This function was formerly used by psql to print query results, but
-- this is no longer the case. Note that it assumes all the data is in
-- text format.
print :: Handle
      -> Result
      -> PrintOpt
      -> IO ()
print h (Result res) po =
    withForeignPtr res $ \resPtr -> do
      B.useAsCString "w" $ \mode -> do
        with po $ \poPtr -> do
          dup_h <- hDuplicate h
          fd <- handleToFd dup_h
          threadWaitWrite fd
          cfile <- c_fdopen (fromIntegral fd) mode
          c_PQprint cfile resPtr poPtr



finish :: Connection
       -> IO ()
finish (Conn mvar) =
    do mFp <- takeMVar mvar
       case mFp of
         Nothing -> putMVar mvar Nothing
         Just fp -> do finalizeForeignPtr fp
                       putMVar mvar Nothing


withConn :: Connection
         -> (Ptr PGconn -> IO b)
         -> IO b
withConn (Conn mvar) f =
    withMVar mvar $ \mFp ->
        case mFp of
          Nothing -> error "Database connection has been closed"
          Just fp -> withForeignPtr fp f


boolFromConn :: Integral b => Connection
             -> (Ptr PGconn -> IO b)
             -> IO Bool
boolFromConn connection f = fmap (toEnum . fromIntegral) $ withConn connection f


-- | Returns the error message most recently generated by an operation
-- on the connection.
resultErrorMessage :: Result
                   -> IO B.ByteString
resultErrorMessage (Result res) =
    B.packCString =<< withForeignPtr res c_PQresultErrorMessage


type Oid = CUInt

foreign import ccall unsafe "libpq-fe.h PQdb"
    c_PQdb :: Ptr PGconn -> IO CString

foreign import ccall unsafe "libpq-fe.h PQuser"
    c_PQuser :: Ptr PGconn -> IO CString

foreign import ccall unsafe "libpq-fe.h PQpass"
    c_PQpass :: Ptr PGconn -> IO CString

foreign import ccall unsafe "libpq-fe.h PQhost"
    c_PQhost :: Ptr PGconn -> IO CString

foreign import ccall unsafe "libpq-fe.h PQport"
    c_PQport :: Ptr PGconn -> IO CString

foreign import ccall unsafe "libpq-fe.h PQoptions"
    c_PQoptions :: Ptr PGconn -> IO CString

foreign import ccall unsafe "libpq-fe.h PQbackendPID"
    c_PQbackendPID :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQstatus"
    c_PQstatus :: Ptr PGconn -> CInt

foreign import ccall unsafe "libpq-fe.h PQsocket"
    c_PQsocket :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQerrorMessage"
    c_PQerrorMessage :: Ptr PGconn -> IO CString

foreign import ccall unsafe "libpq-fe.h PQconnectStart"
    c_PQconnectStart :: CString ->IO (Ptr PGconn)

foreign import ccall unsafe "libpq-fe.h PQconnectPoll"
    c_PQconnectPoll :: Ptr PGconn ->IO CInt

foreign import ccall unsafe "libpq-fe.h PQresetStart"
    c_PQresetStart :: Ptr PGconn ->IO CInt

foreign import ccall unsafe "libpq-fe.h PQresetPoll"
    c_PQresetPoll :: Ptr PGconn ->IO CInt

foreign import ccall unsafe "libpq-fe.h &PQfinish"
    p_PQfinish :: FunPtr (Ptr PGconn -> IO ())

foreign import ccall unsafe "libpq-fe.h PQsetClientEncoding"
    c_PQsetClientEncoding :: Ptr PGconn -> CString -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQsendQuery"
    c_PQsendQuery :: Ptr PGconn -> CString ->IO CInt

foreign import ccall unsafe "libpq-fe.h PQsendQueryParams"
    c_PQsendQueryParams :: Ptr PGconn -> CString -> CInt -> Ptr Oid
                        -> Ptr CString -> Ptr CInt -> Ptr CInt -> CInt
                        -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQsendPrepare"
    c_PQsendPrepare :: Ptr PGconn -> CString -> CString -> CInt -> Ptr Oid
                    -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQsendQueryPrepared"
    c_PQsendQueryPrepared :: Ptr PGconn -> CString -> CInt -> Ptr CString
                          -> Ptr CInt -> Ptr CInt -> CInt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQflush"
    c_PQflush :: Ptr PGconn ->IO CInt

foreign import ccall unsafe "libpq-fe.h PQconsumeInput"
    c_PQconsumeInput :: Ptr PGconn ->IO CInt

foreign import ccall unsafe "libpq-fe.h PQisBusy"
    c_PQisBusy :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQsetnonblocking"
    c_PQsetnonblocking :: Ptr PGconn -> CInt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQisnonblocking"
    c_PQisnonblocking :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQgetResult"
    c_PQgetResult :: Ptr PGconn ->IO (Ptr PGresult)

foreign import ccall unsafe "libpq-fe.h &PQclear"
    p_PQclear :: FunPtr (Ptr PGresult ->IO ())

foreign import ccall unsafe "libpq-fe.h PQresultStatus"
    c_PQresultStatus :: Ptr PGresult -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQresultErrorMessage"
    c_PQresultErrorMessage :: Ptr PGresult -> IO CString

foreign import ccall unsafe "libpq-fe.h PQntuples"
    c_PQntuples :: Ptr PGresult -> CInt

foreign import ccall unsafe "libpq-fe.h PQnfields"
    c_PQnfields :: Ptr PGresult -> CInt

foreign import ccall unsafe "libpq-fe.h PQfname"
    c_PQfname :: Ptr PGresult -> CInt -> IO CString

foreign import ccall unsafe "libpq-fe.h PQfnumber"
    c_PQfnumber :: Ptr PGresult -> CString -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQftable"
    c_PQftable :: Ptr PGresult -> CInt -> IO Oid

foreign import ccall unsafe "libpq-fe.h PQftablecol"
    c_PQftablecol :: Ptr PGresult -> CInt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQfformat"
    c_PQfformat :: Ptr PGresult -> CInt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQftype"
    c_PQftype :: Ptr PGresult -> CInt -> IO Oid

foreign import ccall unsafe "libpq-fe.h PQfmod"
    c_PQfmod :: Ptr PGresult -> CInt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQfsize"
    c_PQfsize :: Ptr PGresult -> CInt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQgetvalue"
    c_PQgetvalue :: Ptr PGresult -> CInt -> CInt -> IO CString

foreign import ccall unsafe "libpq-fe.h PQgetisnull"
    c_PQgetisnull :: Ptr PGresult -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQgetlength"
    c_PQgetlength :: Ptr PGresult -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "stdio.h fdopen"
    c_fdopen :: CInt -> CString -> IO (Ptr CFile)

foreign import ccall unsafe "libpq-fe.h PQprint"
    c_PQprint :: Ptr CFile -> Ptr PGresult -> Ptr PrintOpt -> IO ()

foreign import ccall unsafe "libpq-fe.h PQescapeStringConn"
    c_PQescapeStringConn :: Ptr PGconn
                         -> Ptr Word8 -- Actually (CString)
                         -> CString
                         -> CSize
                         -> Ptr CInt
                         -> IO CSize

foreign import ccall unsafe "libpq-fe.h PQescapeByteaConn"
    c_PQescapeByteaConn :: Ptr PGconn
                        -> CString -- Actually (Ptr CUChar)
                        -> CSize
                        -> Ptr CSize
                        -> IO (Ptr Word8) -- Actually (IO (Ptr CUChar))

foreign import ccall unsafe "libpq-fe.h &PQfreemem"
    p_PQfreemem :: FunPtr (Ptr a -> IO ())





-- /*
--  * Common code for PQexec and sibling routines: prepare to send command
--  */
-- static bool
-- PQexecStart(PGconn *conn)
-- {
--         PGresult   *result;

--         if (!conn)
--                 return false;

--         /*
--          * Silently discard any prior query result that application didn't eat.
--          * This is probably poor design, but it's here for backward compatibility.
--          */
--         while ((result = PQgetResult(conn)) != NULL)
--         {
--                 ExecStatusType resultStatus = result->resultStatus;

--                 PQclear(result);                /* only need its status */
--                 if (resultStatus == PGRES_COPY_IN)
--                 {
--                         if (PG_PROTOCOL_MAJOR(conn->pversion) >= 3)
--                         {
--                                 /* In protocol 3, we can get out of a COPY IN state */
--                                 if (PQputCopyEnd(conn,
--                                                  libpq_gettext("COPY terminated by new PQexec")) < 0)
--                                         return false;
--                                 /* keep waiting to swallow the copy's failure message */
--                         }
--                         else
--                         {
--                                 /* In older protocols we have to punt */
--                                 printfPQExpBuffer(&conn->errorMessage,
--                                   libpq_gettext("COPY IN state must be terminated first\n"));
--                                 return false;
--                         }
--                 }
--                 else if (resultStatus == PGRES_COPY_OUT)
--                 {
--                         if (PG_PROTOCOL_MAJOR(conn->pversion) >= 3)
--                         {
--                                 /*
--                                  * In protocol 3, we can get out of a COPY OUT state: we just
--                                  * switch back to BUSY and allow the remaining COPY data to be
--                                  * dropped on the floor.
--                                  */
--                                 conn->asyncStatus = PGASYNC_BUSY;
--                                 /* keep waiting to swallow the copy's completion message */
--                         }
--                         else
--                         {
--                                 /* In older protocols we have to punt */
--                                 printfPQExpBuffer(&conn->errorMessage,
--                                  libpq_gettext("COPY OUT state must be terminated first\n"));
--                                 return false;
--                         }
--                 }
--                 /* check for loss of connection, too */
--                 if (conn->status == CONNECTION_BAD)
--                         return false;
--         }

--         /* OK to send a command */
--         return true;
-- }

