C_SOURCE = $(wildcard *.c)
C_OBJECT = $(C_SOURCE:.c=.o)
C_DEPEND = $(C_OBJECT:.o=.d)

S_SOURCE = $(wildcard *.s)
S_OBJECT = $(S_SOURCE:.s=.o)

rts.o: $(C_OBJECT) $(S_OBJECT)

%.d: %.c
 $(CPP) $(CFLAGS) $< -MMD -MT $(@:.d=.o) >$@

-include $(C_DEPEND)
