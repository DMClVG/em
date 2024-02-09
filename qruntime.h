#ifndef Q_RUNTIME 
#define Q_RUNTIME
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#define Q_TYPE_NUMBER 1
#define Q_TYPE_PAIR 2


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

extern q_function Q_GFT[];

#define Q_NUMBER(n) ((q_value) { .type = Q_TYPE_NUMBER, .data = n } )
#define Q_STORE(s, n, v) s.top[-n] = v;
#define Q_FETCH(s, n) (s.top[-n])
#define Q_RESIZE(s, n) s.top = s.base + n;
#define Q_POP(s, n) s.top -= n;

#define Q_STACK_SIZE 1024

static inline void q_call(q_stack s, q_value k)
{
  printf("call %ld\n", k.data);
  q_function f = Q_GFT[k.data];
  f(s);
}

static inline void q_print(q_stack s)
{
  q_value k = Q_FETCH(s, 0);
  q_value x = Q_FETCH(s, 1);

  printf("print %x:%ld\n", x.type, x.data);
  
  Q_POP(s, 2);
  q_call(s, k);
}

#endif
