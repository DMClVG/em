#include "qruntime.h"

void f_0(q_stack s);

int main()
{
  q_stack stack;
  stack.base = calloc(Q_STACK_SIZE, sizeof(q_value));
  stack.top = stack.base;

  printf("Starting program..\n");
  f_0(stack);
}
