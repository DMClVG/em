#include "qruntime.h"
void a_toplevel(q_stack *s, q_rets *r, q_pairs *p, void **next);

void program_exit(q_stack *s, q_rets *r, q_pairs *p, void **next)
{
  printf("end. p=%ld\n", p->top - p->base);
  exit(0);
}

int main()
{
  q_stack s; 
  q_rets r;
  q_pairs p;
  q_function next;

  q_init_stack(&s);
  q_init_rets(&r);
  q_init_pairs(&p);

  q_push_ret(&r, &p, &program_exit);
  next = ((q_function) &a_toplevel);

  while (1)
  {
    next(&s, &r, &p, (void *) &next);
  }
  
  return 0;
}
