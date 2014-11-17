#include <libpq-fe.h>
#include <string.h>
#define NOTICE_BUFFER_SIZE 32

typedef struct {
  char*  str;
  size_t len;
} CStringLen;

typedef struct {
  unsigned int start;
  unsigned int count;
  CStringLen buffer[NOTICE_BUFFER_SIZE];
} NoticeBuffer;
