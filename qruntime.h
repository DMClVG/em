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
  void** base;
  void** top;
} q_rets;

typedef struct {
  uint64_t size;
  uint8_t data[];
} q_buffer;

typedef void (*q_function)(q_stack *s, q_rets *r, void **next);

#define Q_NUMBER(n) ((q_value) { .type = Q_TYPE_NUMBER, .data = n } )
#define Q_LAMBDA(f) ((q_value) { .type = Q_TYPE_LAMBDA, .data = (uint64_t)f } )
#define Q_BUFFER(p) ((q_value) { .type = Q_TYPE_BUFFER, .data = (uint64_t)p } )

#define Q_STORE(s, n, v) { q_check_stack_in_bounds(s, n); (s)->top[-n] = v; }
#define Q_FETCH(s, n, v) { q_check_stack_in_bounds(s, n); *v = (s)->top[-n]; }
// #define Q_RESIZE(s, n) (s)->top = (s)->base + n;
#define Q_POP(s, n) { q_check_stack_underflow(s, n); (s)->top -= n; }
#define Q_PUSH(s, n) { q_check_stack_overflow(s, n); (s)->top += n; }
#define Q_BRANCH(s, a, b) { if((s)->top[0].data) { Q_POP(s, 1); goto a; } else { Q_POP(s, 1); goto b; } }

#define Q_STACK_SIZE 1024

static inline void q_check_stack_in_bounds(q_stack *s, uint64_t i)
{
  if (!(i >= 0 && i <= s->top - s->base))
  {
    fprintf(stderr, "Stack invalid access %ld\n", i);
    exit(-1);
  }
}

static inline void q_check_stack_underflow(q_stack *s, uint64_t n)
{
  if (n > s->top - s->base)
  {
    fprintf(stderr, "Stack underflow\n");
    exit(-1);
  }
}

static inline void q_check_stack_overflow(q_stack *s, uint64_t n)
{
  if (s->top - s->base + n >= Q_STACK_SIZE)
  {
    fprintf(stderr, "Stack overflow\n"); 
    exit(-1);
  }
}

static inline void q_init_stack(q_stack *s)
{
  s->base = calloc(Q_STACK_SIZE, sizeof(q_value));
  s->top = s->base;
}


static inline void q_init_rets(q_rets *r)
{
  r->base = calloc(Q_STACK_SIZE, sizeof(void*));
  r->top = r->base;
}

static inline void q_push_ret(q_rets *r, void *p)
{
  *r->top = p;
  r->top++;
  if (r->top - r->base >= Q_STACK_SIZE)
    exit(-1);
}


static inline void q_pop_ret(q_rets *r, void **p)
{
  if (r->top - r->base == 0)
    exit(-1);

  r->top--;
  *p = *r->top;
}

static inline void q_print(q_stack *s)
{
  q_value x;
  Q_FETCH(s, 0, &x);

  printf("print %x:%ld\n", x.type, x.data);
  
  Q_POP(s, 1);
  Q_PUSH(s, 1);
  Q_STORE(s, 0, Q_NUMBER(0));
}

static inline void q_trace(q_stack *s, int i)
{
  q_value x;
  Q_FETCH(s, i, &x);

  printf("print %x:%ld. s=%ld\n", x.type, x.data, (s->top - s->base));
}

static inline void q_make_lambda(q_stack *s, void* f)
{
  Q_PUSH(s, 1);
  Q_STORE(s, 0, Q_LAMBDA(f));
}

static inline void q_call(q_stack *s, void** next)
{
  q_value f;
  Q_FETCH(s, 0, &f);

  if (f.type == Q_TYPE_LAMBDA)
  {
    printf("Calling %lx. s=%ld\n", f.data, s->top - s->base);
    *next = (void*)f.data;
    Q_POP(s, 1);
  }
  else
  {
    fprintf(stderr, "Tried to call value of type %x. Aborting\n", f.type);
    exit(-1);
  }
}


static inline void q_call_tail(q_stack *s, uint64_t argcount, uint64_t paramcount, void** next)
{
  q_call(s, next);

  if (paramcount > 0)
  {
    for (int64_t i = 0; i < argcount; i++)
    {
      q_value temp;
      Q_FETCH(s, argcount - i, &temp);
      Q_STORE(s, argcount + paramcount - i, temp);
    }
    Q_POP(s, paramcount);
  }
}

static inline void q_exit(q_stack *s)
{
  exit(0);
}

static inline void q_add(q_stack *s)
{
  q_value a, b;

  Q_FETCH(s, 0, &a);
  Q_FETCH(s, 1, &b);

  Q_POP(s, 2);
  Q_PUSH(s, 1);

  Q_STORE(s, 0, Q_NUMBER(a.data + b.data));
}

static inline void q_is_equal(q_stack *s)
{
  q_value a, b;

  Q_FETCH(s, 0, &a);
  Q_FETCH(s, 1, &b);

  Q_POP(s, 2);
  Q_PUSH(s, 1);

  Q_STORE(s, 0, Q_NUMBER(a.data == b.data));
}

static inline void q_is_greater(q_stack *s)
{
  q_value a, b;

  Q_FETCH(s, 0, &a);
  Q_FETCH(s, 1, &b);

  Q_POP(s, 2);
  Q_PUSH(s, 1);

  Q_STORE(s, 0, Q_NUMBER(a.data > b.data));
}

static inline void q_alloc(q_stack *s, q_stack *o)
{
  q_value p_size;

  Q_FETCH(s, 0, &p_size);
  Q_POP(s, 1);

  uint64_t size = p_size.data;

  q_buffer* buffer = calloc(size, sizeof(q_buffer) + sizeof(uint8_t) * size);
  buffer->size = size;

  Q_PUSH(o, 1);
  Q_STORE(o, 0, Q_BUFFER(buffer));
}

static inline void q_store(q_stack *s, q_stack *o)
{
  q_value p_buffer, p_idx, p_value;

  Q_FETCH(o, 0, &p_buffer);
  Q_FETCH(s, 0, &p_idx);
  Q_FETCH(s, 1, &p_value);

  Q_POP(s, 2);

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

static inline void q_load(q_stack *s, q_stack *o)
{
  q_value p_buffer, p_idx;

  Q_FETCH(o, 0, &p_buffer);
  Q_FETCH(s, 0, &p_idx);

  Q_POP(s, 1);
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

static inline void q_drop(q_stack *o) 
{
  q_value p_buffer;

  Q_FETCH(o, 0, &p_buffer);

  Q_POP(o, 1);

  if(p_buffer.type == Q_TYPE_BUFFER) { 
    q_buffer* buffer = (q_buffer*)p_buffer.data;
    fprintf(stderr, "Buffer %p freed\n", buffer);
    free(buffer);
  }
}

#endif
