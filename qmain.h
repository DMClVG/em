#include "qruntime.h"

static inline int q_main(int start)
{
  q_stack stack;
  stack.base = calloc(Q_STACK_SIZE, sizeof(q_value));
  stack.top = stack.base;

  printf("Starting program..\n");
  q_call(stack, Q_NUMBER(start));
  return 0;
}
