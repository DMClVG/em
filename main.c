#include "qruntime.h"
void a_toplevel(q_run *q, void **next);

void program_exit(q_run *q, void **next)
{
  printf("end. p=%ld\n", q->pairs.top - q->pairs.base);
  exit(0);
}

int main()
{

  q_run q;

  q_init(&q);

  q_function next;
  q_push_ret(&q, &program_exit);

  next = ((q_function) &a_toplevel);

  while (1)
  {
    next(&q, (void *) &next);
  }
  
  return 0;
}
