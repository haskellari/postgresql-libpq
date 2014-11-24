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
  size_t len = strlen(msg);
  PGnotice* notice = (PGnotice*)malloc(sizeof(PGnotice) + sizeof(char)*(len + 1));
  notice->next = NULL;
  notice->len  = len;
  memcpy(notice->str, msg, len+1);
  if (arg->last == NULL) {
    arg->first = notice;
    arg->last  = notice;
  } else {
    arg->last->next = notice;
    arg->last = notice;
  }
}

PGnotice *
hs_postgresql_libpq_get_notice(NoticeBuffer* arg) {
  if (arg == NULL) return NULL;
  PGnotice * res  = arg->first;
  if (res == NULL) return NULL;
  PGnotice * next = res->next;
  arg->first = next;
  if (next == NULL) arg->last = NULL;
  return res;
}

NoticeBuffer *
hs_postgresql_libpq_malloc_noticebuffer (void) {
  NoticeBuffer * arg = (NoticeBuffer*)malloc(sizeof(NoticeBuffer));
  if (arg == NULL) return NULL;
  memset(arg, 0, sizeof(NoticeBuffer));
  return arg;
}
