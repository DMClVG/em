#ifndef Q_RUNTIME 
#define Q_RUNTIME
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#define Q_TYPE_NUMBER 1
#define Q_TYPE_PAIR 2
#define Q_TYPE_LAMBDA 3

typedef struct {
  uint8_t type;
  uint64_t data;
} q_value;

typedef struct {
  q_value x;
  q_value y;
} q_pair;

typedef struct {
  q_value* base;
  q_value* top;
} q_stack;

typedef void (*q_function)(q_stack s);

#define Q_NUMBER(n) ((q_value) { .type = Q_TYPE_NUMBER, .data = n } )
#define Q_LAMBDA(f) ((q_value) { .type = Q_TYPE_LAMBDA, .data = (uint64_t)f } )

#define Q_STORE(s, n, v) s.top[-n] = v;
#define Q_FETCH(s, n) (s.top[-n])
#define Q_RESIZE(s, n) s.top = s.base + n;
#define Q_POP(s, n) s.top -= n;
#define Q_PUSH(s, n) s.top += n;
#define Q_BRANCH(s, a, b) { if(s.top[0].data) { Q_POP(s, 1); a(s); } else { Q_POP(s, 1); b(s); } }

#define Q_STACK_SIZE 1024

static inline q_stack q_print(q_stack s)
{
  q_value x = Q_FETCH(s, 0);

  printf("print %x:%ld\n", x.type, x.data);
  
  Q_POP(s, 1);
  return s;
}

static inline q_stack q_add(q_stack s)
{
  q_value a = Q_FETCH(s, 0);
  q_value b = Q_FETCH(s, 1);

  Q_POP(s, 2);
  Q_PUSH(s, 1);
  Q_STORE(s, 0, Q_NUMBER(a.data + b.data));

  return s;
}


static inline q_stack q_sub(q_stack s)
{
  q_value a = Q_FETCH(s, 0);
  q_value b = Q_FETCH(s, 1);

  Q_POP(s, 2);
  Q_PUSH(s, 1);
  Q_STORE(s, 0, Q_NUMBER(a.data - b.data));

  return s;
}

static inline q_stack q_mul(q_stack s)
{
  q_value a = Q_FETCH(s, 0);
  q_value b = Q_FETCH(s, 1);

  Q_POP(s, 2);
  Q_PUSH(s, 1);
  Q_STORE(s, 0, Q_NUMBER(a.data * b.data));

  return s;
}


static inline q_stack q_div(q_stack s)
{
  q_value a = Q_FETCH(s, 0);
  q_value b = Q_FETCH(s, 1);

  Q_POP(s, 2);
  Q_PUSH(s, 1);
  Q_STORE(s, 0, Q_NUMBER(a.data / b.data));

  return s;
}

static inline q_stack q_make_lambda(q_stack s, q_function f)
{
  Q_PUSH(s, 1);
  Q_STORE(s, 0, Q_LAMBDA(f));

  return s;
}

static inline void q_call(q_stack s)
{
  q_value f = Q_FETCH(s, 0);
  Q_POP(s, 1);

  if (f.type == Q_TYPE_LAMBDA)
  {
    printf("Calling %lx. s=%ld\n", f.data, s.top - s.base);
    ((q_function) f.data)(s);
  }
  else
  {
    fprintf(stderr, "Tried to call value of type %x. Aborting\n", f.type);
    exit(-1);
  }
}

static inline q_stack q_exit(q_stack s)
{
  q_value code = Q_FETCH(s, 0);
  exit(code.data);
  return s;
}

#endif
