module Database.PostgreSQL.LibPQ.Enums where

#include "hs-libpq.h"

import Data.Bits ((.|.))
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CInt (..))
import System.IO (IOMode(..), SeekMode(..))

-------------------------------------------------------------------------------
-- Type classes
-------------------------------------------------------------------------------

class ToCInt a where
  toCInt   :: a -> CInt

class FromCInt a where
  fromCInt :: CInt -> Maybe a

-------------------------------------------------------------------------------
-- Enumerations
-------------------------------------------------------------------------------

data ExecStatus
    = EmptyQuery    -- ^ The string sent to the server was empty.
    | CommandOk     -- ^ Successful completion of a
                    -- command returning no data.
    | TuplesOk      -- ^ Successful completion of a
                    -- command returning data (such as a
                    -- SELECT or SHOW).
    | CopyOut       -- ^ Copy Out (from server) data
                    -- transfer started.
    | CopyIn        -- ^ Copy In (to server) data transfer
                    -- started.
    | CopyBoth      -- ^ Copy In/Out data transfer started.
    | BadResponse   -- ^ The server's response was not understood.
    | NonfatalError -- ^ A nonfatal error (a notice or
                    -- warning) occurred.
    | FatalError    -- ^ A fatal error occurred.
    | SingleTuple   -- ^ The 'Result' contains a single result tuple
                    -- from the current command. This status occurs
                    -- only when single-row mode has been selected
                    -- for the query.

    | PipelineSync  -- ^ The 'Result' represents a synchronization
                    -- point in pipeline mode, requested by
                    -- 'pipelineSync'. This status occurs only
                    -- when pipeline mode has been selected.
                    --
                    -- @since 0.11.0.0

    | PipelineAbort -- ^ The 'Result' represents a pipeline that
                    -- has received an error from the server.
                    -- 'getResult' must be called repeatedly,
                    -- and each time it will return this status
                    -- code until the end of the current pipeline,
                    -- at which point it will return 'PipelineSync'
                    -- and normal processing can resume.
                    --
                    -- @since 0.11.0.0
  deriving (Eq, Show)

instance FromCInt ExecStatus where
    fromCInt (#const PGRES_EMPTY_QUERY)      = Just EmptyQuery
    fromCInt (#const PGRES_COMMAND_OK)       = Just CommandOk
    fromCInt (#const PGRES_TUPLES_OK)        = Just TuplesOk
    fromCInt (#const PGRES_COPY_OUT)         = Just CopyOut
    fromCInt (#const PGRES_COPY_IN)          = Just CopyIn
    fromCInt (#const PGRES_COPY_BOTH)        = Just CopyBoth
    fromCInt (#const PGRES_BAD_RESPONSE)     = Just BadResponse
    fromCInt (#const PGRES_NONFATAL_ERROR)   = Just NonfatalError
    fromCInt (#const PGRES_FATAL_ERROR)      = Just FatalError
    fromCInt (#const PGRES_SINGLE_TUPLE)     = Just SingleTuple
    fromCInt (#const PGRES_PIPELINE_SYNC)    = Just PipelineSync
    fromCInt (#const PGRES_PIPELINE_ABORTED) = Just PipelineAbort
    fromCInt _ = Nothing

instance ToCInt ExecStatus where
    toCInt EmptyQuery    = (#const PGRES_EMPTY_QUERY)
    toCInt CommandOk     = (#const PGRES_COMMAND_OK)
    toCInt TuplesOk      = (#const PGRES_TUPLES_OK)
    toCInt CopyOut       = (#const PGRES_COPY_OUT)
    toCInt CopyIn        = (#const PGRES_COPY_IN)
    toCInt CopyBoth      = (#const PGRES_COPY_BOTH)
    toCInt BadResponse   = (#const PGRES_BAD_RESPONSE)
    toCInt NonfatalError = (#const PGRES_NONFATAL_ERROR)
    toCInt FatalError    = (#const PGRES_FATAL_ERROR)
    toCInt SingleTuple   = (#const PGRES_SINGLE_TUPLE)
    toCInt PipelineSync  = (#const PGRES_PIPELINE_SYNC)
    toCInt PipelineAbort = (#const PGRES_PIPELINE_ABORTED)


data FieldCode
    = DiagSeverity
    -- ^ The severity; the field contents are ERROR, FATAL,
    -- or PANIC (in an error message), or WARNING, NOTICE,
    -- DEBUG, INFO, or LOG (in a notice message), or a
    -- localized translation of one of these. Always
    -- present.

    | DiagSqlstate
    -- ^ The SQLSTATE code for the error. The SQLSTATE code
    -- identifies the type of error that has occurred; it
    -- can be used by front-end applications to perform
    -- specific operations (such as error handling) in
    -- response to a particular database error. For a list
    -- of the possible SQLSTATE codes, see Appendix A. This
    -- field is not localizable, and is always present.

    | DiagMessagePrimary
    -- ^ The primary human-readable error message
    -- (typically one line). Always present.

    | DiagMessageDetail
    -- ^ Detail: an optional secondary error message
    -- carrying more detail about the problem. Might run to
    -- multiple lines.

    | DiagMessageHint
    -- ^ Hint: an optional suggestion what to do about the
    -- problem. This is intended to differ from detail in
    -- that it offers advice (potentially inappropriate)
    -- rather than hard facts. Might run to multiple lines.

    | DiagStatementPosition
    -- ^ A string containing a decimal integer indicating
    -- an error cursor position as an index into the
    -- original statement string. The first character has
    -- index 1, and positions are measured in characters
    -- not bytes.

    | DiagInternalPosition
    -- ^ This is defined the same as the
    -- 'DiagStatementPosition' field, but it is used when
    -- the cursor position refers to an internally
    -- generated command rather than the one submitted by
    -- the client. The 'DiagInternalQuery' field will
    -- always appear when this field appears.

    | DiagInternalQuery
    -- ^ The text of a failed internally-generated
    -- command. This could be, for example, a SQL query
    -- issued by a PL/pgSQL function.

    | DiagContext
    -- ^ An indication of the context in which the error
    -- occurred. Presently this includes a call stack
    -- traceback of active procedural language functions
    -- and internally-generated queries. The trace is one
    -- entry per line, most recent first.

    | DiagSourceFile
    -- ^ The file name of the source-code location where
    -- the error was reported.

    | DiagSourceLine
    -- ^ The line number of the source-code location where
    -- the error was reported.

    | DiagSourceFunction
    -- ^ The name of the source-code function reporting the
    -- error.

  deriving (Eq, Show)

instance FromCInt FieldCode where
    fromCInt (#const PG_DIAG_SEVERITY)           = Just DiagSeverity
    fromCInt (#const PG_DIAG_SQLSTATE)           = Just DiagSqlstate
    fromCInt (#const PG_DIAG_MESSAGE_PRIMARY)    = Just DiagMessagePrimary
    fromCInt (#const PG_DIAG_MESSAGE_DETAIL)     = Just DiagMessageDetail
    fromCInt (#const PG_DIAG_MESSAGE_HINT)       = Just DiagMessageHint
    fromCInt (#const PG_DIAG_STATEMENT_POSITION) = Just DiagStatementPosition
    fromCInt (#const PG_DIAG_INTERNAL_POSITION)  = Just DiagInternalPosition
    fromCInt (#const PG_DIAG_INTERNAL_QUERY)     = Just DiagInternalQuery
    fromCInt (#const PG_DIAG_CONTEXT)            = Just DiagContext
    fromCInt (#const PG_DIAG_SOURCE_FILE)        = Just DiagSourceFile
    fromCInt (#const PG_DIAG_SOURCE_LINE)        = Just DiagSourceLine
    fromCInt (#const PG_DIAG_SOURCE_FUNCTION)    = Just DiagSourceFunction
    fromCInt _ = Nothing

instance ToCInt FieldCode where
    toCInt DiagSeverity          = (#const PG_DIAG_SEVERITY)
    toCInt DiagSqlstate          = (#const PG_DIAG_SQLSTATE)
    toCInt DiagMessagePrimary    = (#const PG_DIAG_MESSAGE_PRIMARY)
    toCInt DiagMessageDetail     = (#const PG_DIAG_MESSAGE_DETAIL)
    toCInt DiagMessageHint       = (#const PG_DIAG_MESSAGE_HINT)
    toCInt DiagStatementPosition = (#const PG_DIAG_STATEMENT_POSITION)
    toCInt DiagInternalPosition  = (#const PG_DIAG_INTERNAL_POSITION)
    toCInt DiagInternalQuery     = (#const PG_DIAG_INTERNAL_QUERY)
    toCInt DiagContext           = (#const PG_DIAG_CONTEXT)
    toCInt DiagSourceFile        = (#const PG_DIAG_SOURCE_FILE)
    toCInt DiagSourceLine        = (#const PG_DIAG_SOURCE_LINE)
    toCInt DiagSourceFunction    = (#const PG_DIAG_SOURCE_FUNCTION)


data Verbosity
    = ErrorsTerse
    | ErrorsDefault
    | ErrorsVerbose
    | ErrorsSqlstate
  deriving (Eq, Show)

instance FromCInt Verbosity where
    fromCInt (#const PQERRORS_TERSE)    = Just ErrorsTerse
    fromCInt (#const PQERRORS_DEFAULT)  = Just ErrorsDefault
    fromCInt (#const PQERRORS_VERBOSE)  = Just ErrorsVerbose
    fromCInt (#const PQERRORS_SQLSTATE) = Just ErrorsSqlstate
    fromCInt _ = Nothing

instance ToCInt Verbosity where
    toCInt ErrorsTerse   = (#const PQERRORS_TERSE)
    toCInt ErrorsDefault = (#const PQERRORS_DEFAULT)
    toCInt ErrorsVerbose = (#const PQERRORS_VERBOSE)


data PollingStatus
    = PollingFailed
    | PollingReading
    | PollingWriting
    | PollingOk
  deriving (Eq, Show)

instance FromCInt PollingStatus where
    fromCInt (#const PGRES_POLLING_READING) = return PollingReading
    fromCInt (#const PGRES_POLLING_OK)      = return PollingOk
    fromCInt (#const PGRES_POLLING_WRITING) = return PollingWriting
    fromCInt (#const PGRES_POLLING_FAILED)  = return PollingFailed
    fromCInt _ = Nothing


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
  deriving (Eq, Show)

instance FromCInt ConnStatus where
    fromCInt (#const CONNECTION_OK)                = return ConnectionOk
    fromCInt (#const CONNECTION_BAD)               = return ConnectionBad
    fromCInt (#const CONNECTION_STARTED)           = return ConnectionStarted
    fromCInt (#const CONNECTION_MADE)              = return ConnectionMade
    fromCInt (#const CONNECTION_AWAITING_RESPONSE) = return ConnectionAwaitingResponse
    fromCInt (#const CONNECTION_AUTH_OK)           = return ConnectionAuthOk
    fromCInt (#const CONNECTION_SETENV)            = return ConnectionSetEnv
    fromCInt (#const CONNECTION_SSL_STARTUP)       = return ConnectionSSLStartup
    -- fromCInt (#const CONNECTION_NEEDED)         = return ConnectionNeeded
    fromCInt _ = Nothing


data TransactionStatus
    = TransIdle    -- ^ currently idle
    | TransActive  -- ^ a command is in progress
    | TransInTrans -- ^ idle, in a valid transaction block
    | TransInError -- ^ idle, in a failed transaction block
    | TransUnknown -- ^ the connection is bad
  deriving (Eq, Show)

instance FromCInt TransactionStatus where
    fromCInt (#const PQTRANS_IDLE)    = return TransIdle
    fromCInt (#const PQTRANS_ACTIVE)  = return TransActive
    fromCInt (#const PQTRANS_INTRANS) = return TransInTrans
    fromCInt (#const PQTRANS_INERROR) = return TransInError
    fromCInt (#const PQTRANS_UNKNOWN) = return TransUnknown
    fromCInt _ = Nothing


data Format
    = Text
    | Binary
  deriving (Eq, Ord, Show, Enum)

instance ToCInt Format where
    toCInt Text   = 0
    toCInt Binary = 1

instance FromCInt Format where
    fromCInt 0 = Just Text
    fromCInt 1 = Just Binary
    fromCInt _ = Nothing


-- |
--
-- @since 0.11.0.0
data PipelineStatus
    = PipelineOn           -- ^ The 'Connection' is in pipeline mode.
    | PipelineOff          -- ^ The 'Connection' is /not/ in pipeline mode.
    | PipelineAborted      -- ^ The 'Connection' is in pipeline mode and an error
                           -- occurred while processing the current pipeline. The
                           -- aborted flag is cleared when 'getResult' returns a
                           -- result with status 'PipelineSync'.
  deriving (Eq, Show)

instance FromCInt PipelineStatus where
    fromCInt (#const PQ_PIPELINE_ON) = return PipelineOn
    fromCInt (#const PQ_PIPELINE_OFF) = return PipelineOff
    fromCInt (#const PQ_PIPELINE_ABORTED) = return PipelineAborted
    fromCInt _ = Nothing

-------------------------------------------------------------------------------
-- System.IO enumerations
-------------------------------------------------------------------------------

instance ToCInt IOMode where
    toCInt ReadMode      = (#const INV_READ)
    toCInt WriteMode     = (#const INV_WRITE)
    toCInt ReadWriteMode = (#const INV_READ) .|. (#const INV_WRITE)
    toCInt AppendMode    = (#const INV_WRITE)

instance ToCInt SeekMode where
    toCInt AbsoluteSeek = #const SEEK_SET
    toCInt RelativeSeek = #const SEEK_CUR
    toCInt SeekFromEnd  = #const SEEK_END

-------------------------------------------------------------------------------
-- Prelude
-------------------------------------------------------------------------------

instance ToCInt Bool where
    toCInt False = 0
    toCInt True  = 1

instance FromCInt Bool where
    fromCInt 0 = Just False
    fromCInt 1 = Just True
    fromCInt _ = Nothing

-------------------------------------------------------------------------------
-- Enum instances (for backwards compatibility)
-------------------------------------------------------------------------------

instance Enum ExecStatus where
    toEnum   = fromMaybe (error "toEnum @Database.PostgreSQL.LibPQ.ExecStatus") . fromCInt . toEnum
    fromEnum = fromEnum . toCInt

instance Enum FieldCode where
    toEnum   = fromMaybe (error "toEnum @Database.PostgreSQL.LibPQ.FieldCode") . fromCInt . toEnum
    fromEnum = fromEnum . toCInt

instance Enum Verbosity where
    toEnum   = fromMaybe (error "toEnum @Database.PostgreSQL.LibPQ.Verbosity") . fromCInt . toEnum
    fromEnum = fromEnum . toCInt
