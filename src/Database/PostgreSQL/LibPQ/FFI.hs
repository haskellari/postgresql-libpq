{-# LANGUAGE CApiFFI        #-}
{-# LANGUAGE EmptyDataDecls #-}
module Database.PostgreSQL.LibPQ.FFI where

import Data.Word        (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types  (CChar, CInt (..), CSize (..), CUInt (..))
import Foreign.Ptr      (FunPtr, Ptr)

import Database.PostgreSQL.LibPQ.Internal (CNoticeBuffer, NoticeBuffer, PGconn)
import Database.PostgreSQL.LibPQ.Notify   (Notify, PGnotice)
import Database.PostgreSQL.LibPQ.Oid      (Oid (..))

-------------------------------------------------------------------------------
-- Types / Ptr tags
-------------------------------------------------------------------------------

data PGresult
data PGcancel

type CFd = CInt

type NoticeReceiver = NoticeBuffer -> Ptr PGresult -> IO ()

-------------------------------------------------------------------------------
-- FFI imports
-------------------------------------------------------------------------------

foreign import ccall        "libpq-fe.h PQconnectdb"
    c_PQconnectdb :: CString -> IO (Ptr PGconn)

foreign import ccall        "libpq-fe.h PQconnectStart"
    c_PQconnectStart :: CString -> IO (Ptr PGconn)

foreign import ccall        "libpq-fe.h PQconnectPoll"
    c_PQconnectPoll :: Ptr PGconn -> IO CInt

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

foreign import ccall unsafe "libpq-fe.h PQconnectionNeedsPassword"
    c_PQconnectionNeedsPassword :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQconnectionUsedPassword"
    c_PQconnectionUsedPassword :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQstatus"
    c_PQstatus :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQtransactionStatus"
    c_PQtransactionStatus :: Ptr PGconn -> IO CInt

foreign import ccall        "libpq-fe.h PQparameterStatus"
    c_PQparameterStatus :: Ptr PGconn -> CString -> IO CString

foreign import ccall unsafe "libpq-fe.h PQprotocolVersion"
    c_PQprotocolVersion :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQserverVersion"
    c_PQserverVersion :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQlibVersion"
    c_PQlibVersion :: IO CInt

foreign import ccall unsafe "libpq-fe.h PQsocket"
    c_PQsocket :: Ptr PGconn -> IO CInt

foreign import ccall        "libpq-fe.h PQerrorMessage"
    c_PQerrorMessage :: Ptr PGconn -> IO CString

foreign import ccall        "libpq-fe.h PQfinish"
    c_PQfinish :: Ptr PGconn -> IO ()

foreign import ccall        "libpq-fe.h PQreset"
    c_PQreset :: Ptr PGconn -> IO ()

foreign import ccall        "libpq-fe.h PQresetStart"
    c_PQresetStart :: Ptr PGconn -> IO CInt

foreign import ccall        "libpq-fe.h PQresetPoll"
    c_PQresetPoll :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQclientEncoding"
    c_PQclientEncoding :: Ptr PGconn -> IO CInt

foreign import ccall        "libpq-fe.h pg_encoding_to_char"
    c_pg_encoding_to_char :: CInt -> IO CString

foreign import ccall        "libpq-fe.h PQsetClientEncoding"
    c_PQsetClientEncoding :: Ptr PGconn -> CString -> IO CInt

type PGVerbosity = CInt
foreign import ccall unsafe "libpq-fe.h PQsetErrorVerbosity"
    c_PQsetErrorVerbosity :: Ptr PGconn -> PGVerbosity -> IO PGVerbosity

foreign import ccall        "libpq-fe.h PQputCopyData"
    c_PQputCopyData :: Ptr PGconn -> Ptr CChar -> CInt -> IO CInt

foreign import ccall        "libpq-fe.h PQputCopyEnd"
    c_PQputCopyEnd :: Ptr PGconn -> CString -> IO CInt

foreign import ccall        "libpq-fe.h PQgetCopyData"
    c_PQgetCopyData :: Ptr PGconn -> Ptr (Ptr Word8) -> CInt -> IO CInt

foreign import ccall        "libpq-fe.h PQsendQuery"
    c_PQsendQuery :: Ptr PGconn -> CString -> IO CInt

foreign import ccall        "libpq-fe.h PQsendQueryParams"
    c_PQsendQueryParams :: Ptr PGconn -> CString -> CInt -> Ptr Oid
                        -> Ptr CString -> Ptr CInt -> Ptr CInt -> CInt
                        -> IO CInt

foreign import ccall        "libpq-fe.h PQsendPrepare"
    c_PQsendPrepare :: Ptr PGconn -> CString -> CString -> CInt -> Ptr Oid
                    -> IO CInt

foreign import ccall        "libpq-fe.h PQsendQueryPrepared"
    c_PQsendQueryPrepared :: Ptr PGconn -> CString -> CInt -> Ptr CString
                          -> Ptr CInt -> Ptr CInt -> CInt -> IO CInt

foreign import ccall        "libpq-fe.h PQsendDescribePrepared"
    c_PQsendDescribePrepared :: Ptr PGconn -> CString -> IO CInt

foreign import ccall        "libpq-fe.h PQsendDescribePortal"
    c_PQsendDescribePortal :: Ptr PGconn -> CString -> IO CInt

foreign import ccall        "libpq-fe.h PQflush"
    c_PQflush :: Ptr PGconn -> IO CInt

foreign import ccall        "libpq-fe.h PQgetCancel"
    c_PQgetCancel :: Ptr PGconn -> IO (Ptr PGcancel)

foreign import ccall        "libpq-fe.h &PQfreeCancel"
    p_PQfreeCancel :: FunPtr (Ptr PGcancel -> IO ())

foreign import ccall        "libpq-fe.h PQcancel"
    c_PQcancel :: Ptr PGcancel -> CString -> CInt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQnotifies"
    c_PQnotifies :: Ptr PGconn -> IO (Ptr Notify)

foreign import ccall        "libpq-fe.h PQconsumeInput"
    c_PQconsumeInput :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQisBusy"
    c_PQisBusy :: Ptr PGconn -> IO CInt

foreign import ccall        "libpq-fe.h PQsetnonblocking"
    c_PQsetnonblocking :: Ptr PGconn -> CInt -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQisnonblocking"
    c_PQisnonblocking :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQsetSingleRowMode"
    c_PQsetSingleRowMode :: Ptr PGconn -> IO CInt

foreign import ccall        "libpq-fe.h PQgetResult"
    c_PQgetResult :: Ptr PGconn -> IO (Ptr PGresult)

foreign import ccall        "libpq-fe.h PQexec"
    c_PQexec :: Ptr PGconn -> CString -> IO (Ptr PGresult)

foreign import ccall        "libpq-fe.h PQexecParams"
    c_PQexecParams :: Ptr PGconn -> CString -> CInt -> Ptr Oid
                   -> Ptr CString -> Ptr CInt -> Ptr CInt -> CInt
                   -> IO (Ptr PGresult)

foreign import ccall        "libpq-fe.h PQprepare"
    c_PQprepare :: Ptr PGconn -> CString -> CString -> CInt -> Ptr Oid
                -> IO (Ptr PGresult)

foreign import ccall        "libpq-fe.h PQexecPrepared"
    c_PQexecPrepared :: Ptr PGconn -> CString -> CInt -> Ptr CString
                     -> Ptr CInt -> Ptr CInt -> CInt -> IO (Ptr PGresult)

foreign import ccall        "libpq-fe.h PQdescribePrepared"
    c_PQdescribePrepared :: Ptr PGconn -> CString -> IO (Ptr PGresult)

foreign import ccall        "libpq-fe.h PQdescribePortal"
    c_PQdescribePortal :: Ptr PGconn -> CString -> IO (Ptr PGresult)

foreign import ccall        "libpq-fe.h &PQclear"
    p_PQclear :: FunPtr (Ptr PGresult -> IO ())

foreign import ccall unsafe "libpq-fe.h PQresultStatus"
    c_PQresultStatus :: Ptr PGresult -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQresStatus"
    c_PQresStatus :: CInt -> IO CString

foreign import ccall unsafe "libpq-fe.h PQresultErrorMessage"
    c_PQresultErrorMessage :: Ptr PGresult -> IO CString

foreign import ccall        "libpq-fe.h PQresultErrorField"
    c_PQresultErrorField :: Ptr PGresult -> CInt -> IO CString

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

foreign import ccall unsafe "libpq-fe.h PQnparams"
    c_PQnparams :: Ptr PGresult -> IO CInt

foreign import ccall unsafe "libpq-fe.h PQparamtype"
    c_PQparamtype :: Ptr PGresult -> CInt -> IO Oid

foreign import ccall unsafe "libpq-fe.h PQcmdStatus"
    c_PQcmdStatus :: Ptr PGresult -> IO CString

foreign import ccall unsafe "libpq-fe.h PQcmdTuples"
    c_PQcmdTuples :: Ptr PGresult -> IO CString

foreign import ccall        "libpq-fe.h PQescapeStringConn"
    c_PQescapeStringConn :: Ptr PGconn
                         -> Ptr Word8 -- Actually (CString)
                         -> CString
                         -> CSize
                         -> Ptr CInt
                         -> IO CSize

foreign import ccall        "libpq-fe.h PQescapeByteaConn"
    c_PQescapeByteaConn :: Ptr PGconn
                        -> CString -- Actually (Ptr CUChar)
                        -> CSize
                        -> Ptr CSize
                        -> IO (Ptr Word8) -- Actually (IO (Ptr CUChar))

foreign import ccall        "libpq-fe.h PQunescapeBytea"
    c_PQunescapeBytea :: CString -- Actually (Ptr CUChar)
                      -> Ptr CSize
                      -> IO (Ptr Word8) -- Actually (IO (Ptr CUChar))

foreign import ccall unsafe "libpq-fe.h PQescapeIdentifier"
    c_PQescapeIdentifier :: Ptr PGconn
                         -> CString
                         -> CSize
                         -> IO CString

foreign import ccall unsafe "libpq-fe.h &PQfreemem"
    p_PQfreemem :: FunPtr (Ptr a -> IO ())

foreign import ccall unsafe "libpq-fe.h PQfreemem"
    c_PQfreemem :: Ptr a -> IO ()

foreign import ccall unsafe "noticehandlers.h hs_postgresql_libpq_malloc_noticebuffer"
    c_malloc_noticebuffer :: IO (Ptr CNoticeBuffer)

foreign import ccall unsafe "noticehandlers.h hs_postgresql_libpq_free_noticebuffer"
    c_free_noticebuffer :: Ptr CNoticeBuffer -> IO ()

foreign import ccall unsafe "noticehandlers.h hs_postgresql_libpq_get_notice"
    c_get_notice :: Ptr CNoticeBuffer -> IO (Ptr PGnotice)

foreign import ccall unsafe "noticehandlers.h &hs_postgresql_libpq_discard_notices"
    p_discard_notices :: FunPtr NoticeReceiver

foreign import ccall unsafe "noticehandlers.h &hs_postgresql_libpq_store_notices"
    p_store_notices :: FunPtr NoticeReceiver

foreign import ccall unsafe "libpq-fe.h PQsetNoticeReceiver"
    c_PQsetNoticeReceiver :: Ptr PGconn -> FunPtr NoticeReceiver -> Ptr CNoticeBuffer -> IO (FunPtr NoticeReceiver)

-------------------------------------------------------------------------------
-- FFI imports: Large Objects
-------------------------------------------------------------------------------

foreign import ccall        "libpq-fs.h lo_creat"
    c_lo_creat :: Ptr PGconn -> CInt -> IO Oid

foreign import ccall        "libpq-fs.h lo_create"
    c_lo_create :: Ptr PGconn -> Oid -> IO Oid

foreign import ccall        "libpq-fs.h lo_import"
    c_lo_import :: Ptr PGconn -> CString -> IO Oid

foreign import ccall        "libpq-fs.h lo_import_with_oid"
    c_lo_import_with_oid :: Ptr PGconn -> CString -> Oid -> IO Oid

foreign import ccall        "libpq-fs.h lo_export"
    c_lo_export :: Ptr PGconn -> Oid -> CString -> IO CInt

foreign import ccall        "libpq-fs.h lo_open"
    c_lo_open :: Ptr PGconn -> Oid -> CInt -> IO CFd

foreign import ccall        "libpq-fs.h lo_write"
    c_lo_write :: Ptr PGconn -> CFd -> CString -> CSize -> IO CInt

foreign import ccall        "libpq-fs.h lo_read"
    c_lo_read :: Ptr PGconn -> CFd -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall        "libpq-fs.h lo_lseek"
    c_lo_lseek :: Ptr PGconn -> CFd -> CInt -> CInt -> IO CInt

foreign import ccall        "libpq-fs.h lo_tell"
    c_lo_tell :: Ptr PGconn -> CFd -> IO CInt

foreign import ccall        "libpq-fs.h lo_truncate"
    c_lo_truncate :: Ptr PGconn -> CFd -> CSize -> IO CInt

foreign import ccall        "libpq-fs.h lo_close"
    c_lo_close :: Ptr PGconn -> CFd -> IO CInt

foreign import ccall        "libpq-fs.h lo_unlink"
    c_lo_unlink :: Ptr PGconn -> Oid -> IO CInt
