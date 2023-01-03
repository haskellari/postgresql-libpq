#ifndef HS_POSTGRESQL_LIBPQ
#define HS_POSTGRESQL_LIBPQ

#include <string.h>
#include <stdlib.h>
#include <libpq-fe.h>
#include <libpq/libpq-fs.h>

typedef struct PGnotice{
  struct PGnotice * next;
  size_t len;
  char   str[];
} PGnotice;

typedef struct {
  PGnotice * first;
  PGnotice * last;
} NoticeBuffer;

void hs_postgresql_libpq_discard_notices(NoticeBuffer* arg, const PGresult* res);
void hs_postgresql_libpq_store_notices(NoticeBuffer* arg, const PGresult* res);
PGnotice * hs_postgresql_libpq_get_notice(NoticeBuffer* arg);
NoticeBuffer * hs_postgresql_libpq_malloc_noticebuffer (void);
void hs_postgresql_libpq_free_noticebuffer (NoticeBuffer * arg);

#endif
