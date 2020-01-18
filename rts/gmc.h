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

struct dump_entry {
  long stack_pointer;
  long program_cntr;
  long new_stack;
};

#define TAG_LINK 1
#define TAG_SUPERCOMB 2
#define TAG_AP 3
#define TAG_INT 4

register val *Hp                    asm("r14");
register val *HpLim                 asm("r15");
register struct dump_entry *Es      asm("r12");
register struct dump_entry *EsBase  asm("r13");
