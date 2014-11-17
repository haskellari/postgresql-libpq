#include "noticehandlers.h"
#include <stdlib.h>

void
hs_postgresql_libpq_discard_notices(NoticeBuffer* arg, const PGresult* res) {
  return;
}

void
hs_postgresql_libpq_store_notices(NoticeBuffer* arg, const PGresult* res) {
  if (arg == NULL || res == NULL) return;
  const char* msg = PQresultErrorMessage(res);
  if (msg == NULL) return;
  CStringLen str;
  str.len = strlen(msg);
  str.str = (char*)malloc(str.len+1);
  if (str.str == NULL) return;
  memcpy(str.str, msg, str.len+1);
  unsigned int start = arg->start;
  unsigned int count = arg->count;
  if ( count == NOTICE_BUFFER_SIZE ) {
    free(arg->buffer[start].str);
    arg->buffer[start] = str;
    arg->start = (start + 1) % NOTICE_BUFFER_SIZE;
  } else {
    arg->buffer[ (start + count) % NOTICE_BUFFER_SIZE ] = str;
    arg->count++;
  }
}

CStringLen *
hs_postgresql_libpq_get_notice(NoticeBuffer* arg) {
  if (arg == NULL || arg->count == 0) return NULL;
  CStringLen * res = &(arg->buffer[arg->start]);
  arg->start = (arg->start + 1) % NOTICE_BUFFER_SIZE;
  arg->count--;
  return res;
}

NoticeBuffer *
hs_postgresql_libpq_malloc_noticebuffer (void) {
  NoticeBuffer * arg = (NoticeBuffer*)malloc(sizeof(NoticeBuffer));
  if (arg == NULL) return NULL;
  memset(arg, 0, sizeof(NoticeBuffer));
  return arg;
}
