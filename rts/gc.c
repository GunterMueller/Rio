#include "gmc.h"

int gcError(int overcommit_bytes) {
  printf("attempted to allocate %d bytes heap had no space for");
  exit(3);
}

int gcEntry() {
  HpBase = malloc(sizeof(struct value) * 4096);
  Hp     = HpBase;
  HpLim  = HpBase + sizeof(struct value) * 4096;
  return 0;
}
