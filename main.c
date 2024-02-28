#include "qruntime.h"
void a_toplevel(q_run *q, void **next);

void superduper(q_run *q, void **next)
{
  q_value x;

  Q_FETCH(q, 0, &x);

  switch(x.type)
  {
  case Q_TYPE_PAIR:
    {
      printf("this is a pair.\n");
    } break;
  case Q_TYPE_SYMBOL:
    {
      printf("this is a symbol.\n");
    } break;
  case Q_TYPE_NUMBER:
    {
      printf("this is a number.\n");
    } break;
  case Q_TYPE_LAMBDA:
    {
      printf("this is a lambda.\n");
    } break;
  default:
    {
      printf("I'm dumb idk what this is :(\n");
    } break;
  }
  q_pop_ret(q, 0, next);
}

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
