#include "qruntime.h"

#define Q_MAIN(f) { \
  q_stack stack;\
  stack.base = calloc(Q_STACK_SIZE, sizeof(q_value));\
  stack.top = stack.base;\
  printf("Starting program..\n"); \
  f(stack); \
}
  
