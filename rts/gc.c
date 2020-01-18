#include "gmc.h"

extern struct value _main;

int gcError(int overcommit_bytes) {
  printf("attempted to allocate %d bytes heap had no space for");
  printf("%p", &_main);
  exit(3);
}

int gcEntry() {
  Hp     = malloc(sizeof(struct value) * 4096);
  HpLim  = Hp + 4096;
  Es     = malloc(sizeof(struct dump_entry) * 4096);
  EsBase = Es;
  return 0;
}

struct value **newStack() {
  return (malloc(sizeof(struct value *) * 4096)) + 4096;
}

