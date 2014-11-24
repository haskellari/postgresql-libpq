#include <libpq-fe.h>
#include <string.h>

typedef struct PGnotice{
  struct PGnotice * next;
  size_t len;
  char   str[];
} PGnotice;

typedef struct {
  PGnotice * first;
  PGnotice * last;
} NoticeBuffer;
