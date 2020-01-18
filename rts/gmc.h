#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct value {
  char tag;
  union {
    struct value *link;
    int64_t intv;
    struct {
      struct value *fun;
      struct value *arg;
    };
    struct {
      int (*code)(void);
      int arity;
    };
  };
} val;

#define TAG_LINK 1
#define TAG_SUPERCOMB 2
#define TAG_AP 3
#define TAG_INT 4

register val *HpBase __asm__("r13");
register val *Hp     __asm__("r14");
register val *HpLim  __asm__("r15");
register val **Sp    __asm__("rsp");
