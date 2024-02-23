#include "qruntime.h"
void a_toplevel(q_stack *s, q_rets *r, void **next);

void program_exit(q_stack *s, q_rets *r, void **next)
{
  printf("end.\n");
  exit(0);
}

int main()
{
  q_stack s; 
  q_rets r;
  q_function next;

  q_init_stack(&s);
  q_init_rets(&r);

  q_push_ret(&r, &program_exit);
  next = ((q_function) &a_toplevel);

  while (1)
  {
    next(&s, &r, (void *) &next);
  }
  
  return 0;
}