#ifndef Q_RUNTIME 
#define Q_RUNTIME
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#define Q_TYPE_NUMBER 1
#define Q_TYPE_PAIR 2
#define Q_TYPE_LAMBDA 3
#define Q_TYPE_BUFFER 4

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

typedef struct {
  uint64_t size;
  uint8_t data[];
} q_buffer;

typedef void (*q_function)(q_stack s);

#define Q_NUMBER(n) ((q_value) { .type = Q_TYPE_NUMBER, .data = n } )
#define Q_LAMBDA(f) ((q_value) { .type = Q_TYPE_LAMBDA, .data = (uint64_t)f } )
#define Q_BUFFER(p) ((q_value) { .type = Q_TYPE_BUFFER, .data = (uint64_t)p } )

#define Q_STORE(s, n, v) (s)->top[-n] = v;
#define Q_FETCH(s, n) ((s)->top[-n])
#define Q_RESIZE(s, n) (s)->top = (s)->base + n;
#define Q_POP(s, n) (s)->top -= n;
#define Q_PUSH(s, n) (s)->top += n;
#define Q_BRANCH(s, a, b) { if((s)->top[0].data) { Q_POP(s, 1); goto a; } else { Q_POP(s, 1); goto b; } }

#define Q_STACK_SIZE 1024

static inline void q_print(q_stack *s)
{
  q_value x = Q_FETCH(s, 0);

  printf("print %x:%ld\n", x.type, x.data);
  
  Q_POP(s, 1);
}

static inline void q_trace(q_stack *s, int i)
{
  q_value x = Q_FETCH(s, i);

  printf("print %x:%ld. s=%ld\n", x.type, x.data, (s->top - s->base));
}

static inline void q_make_lambda(q_stack *s, void* f)
{
  Q_PUSH(s, 1);
  Q_STORE(s, 0, Q_LAMBDA(f));
}

static inline void q_call(q_stack *s, void** next)
{
  q_value f = Q_FETCH(s, 0);
  Q_POP(s, 1);

  if (f.type == Q_TYPE_LAMBDA)
  {
    printf("Calling %lx. s=%ld\n", f.data, s->top - s->base);
    *next = (void*)f.data;
  }
  else
  {
    fprintf(stderr, "Tried to call value of type %x. Aborting\n", f.type);
    exit(-1);
  }
}

static inline void q_exit(q_stack *s)
{
  q_value code = Q_FETCH(s, 0);
  exit(code.data);
}

static inline void q_add(q_stack *s)
{
  q_value a = Q_FETCH(s, 0);
  q_value b = Q_FETCH(s, 1);
  Q_POP(s, 2);
  Q_PUSH(s, 1);

  Q_STORE(s, 0, Q_NUMBER(a.data + b.data));
}

static inline void q_is_equal(q_stack *s)
{
  q_value a = Q_FETCH(s, 0);
  q_value b = Q_FETCH(s, 1);
  Q_POP(s, 2);
  Q_PUSH(s, 1);

  Q_STORE(s, 0, Q_NUMBER(a.data == b.data));
}

static inline void q_is_greater(q_stack *s)
{
  q_value a = Q_FETCH(s, 0);
  q_value b = Q_FETCH(s, 1);
  Q_POP(s, 2);
  Q_PUSH(s, 1);

  Q_STORE(s, 0, Q_NUMBER(a.data > b.data));
}

static inline void q_alloc(q_stack *s)
{
  q_value size = Q_FETCH(s, 0);
  Q_POP(s, 1);
  Q_PUSH(s, 1);

  uint64_t buffer_size = size.data;

  q_buffer* buffer = calloc(buffer_size, sizeof(uint64_t) + sizeof(uint8_t) * buffer_size);
  buffer->size = buffer_size;

  Q_STORE(s, 0, Q_BUFFER(buffer));
}

static inline void q_store(q_stack *s)
{
  q_value p_buffer = Q_FETCH(s, 0);
  q_value p_idx = Q_FETCH(s, 1);
  q_value p_value = Q_FETCH(s, 2);
  Q_POP(s, 3);

  if(p_buffer.type != Q_TYPE_BUFFER) { 
    fprintf(stderr, "Argument is not a buffer\n");
    exit(-1);
  }

  q_buffer* buffer = (q_buffer*)p_buffer.data;
  if(p_idx.data >= buffer->size || p_idx.data < 0) {
    fprintf(stderr, "Out of bounds\n"); 
    exit(-1);
  }

  buffer->data[p_idx.data] = (uint8_t) p_value.data;
}

static inline void q_load(q_stack *s)
{
  q_value p_buffer = Q_FETCH(s, 0);
  q_value p_idx = Q_FETCH(s, 1);
  Q_POP(s, 2);
  Q_PUSH(s, 1);

  if(p_buffer.type != Q_TYPE_BUFFER) { 
    fprintf(stderr, "Argument is not a buffer\n");
    exit(-1);
  }

  q_buffer* buffer = (q_buffer*)p_buffer.data;
  if(p_idx.data >= buffer->size || p_idx.data < 0) {
    fprintf(stderr, "Out of bounds\n"); 
    exit(-1);
  }

  Q_STORE(s, 0, Q_NUMBER((uint64_t) buffer->data[p_idx.data]));
}

#endif
