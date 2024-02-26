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
  q_value head;
  q_value tail;
} q_pair;

typedef struct {
  q_value* base;
  q_value* top;
} q_stack;

typedef struct {
  void* ret;
  q_pair* pair_top;
} q_frame;

typedef struct {
  q_frame* base;
  q_frame* top;
} q_rets;

typedef struct {
  q_pair* base;
  q_pair* top;
  uint64_t total;
} q_pairs;

typedef struct {
  uint64_t size;
  uint8_t data[];
} q_buffer;

typedef void (*q_function)(q_stack *s, q_rets *r, q_pairs *p, void **next);

#define Q_NUMBER(n) ((q_value) { .type = Q_TYPE_NUMBER, .data = n } )
#define Q_PAIR(p) ((q_value) { .type = Q_TYPE_PAIR, .data = (uint64_t) (p) } )
#define Q_LAMBDA(f) ((q_value) { .type = Q_TYPE_LAMBDA, .data = (uint64_t)(f) } )
#define Q_BUFFER(p) ((q_value) { .type = Q_TYPE_BUFFER, .data = (uint64_t)(p) } )

#define Q_STORE(s, n, v) { q_check_stack_in_bounds(s, n); (s)->top[-(n)] = v; }
#define Q_FETCH(s, n, v) { q_check_stack_in_bounds(s, n); *(v) = (s)->top[-(n)]; }
// #define Q_RESIZE(s, n) (s)->top = (s)->base + n;
#define Q_POP(s, n) { q_check_stack_underflow(s, n); (s)->top -= n; }
#define Q_PUSH(s, n) { q_check_stack_overflow(s, n); (s)->top += n; }
#define Q_BRANCH(s, a, b, next) { if((s)->top[0].data) { Q_POP(s, 1); *(next) = a; } else { Q_POP(s, 1); *(next) = b; } }

#define Q_STACK_SIZE 1024
#define Q_PAIRS_SIZE 1024

static inline void q_fatal(const char *msg)
{
  printf("%s", msg);
  exit(-22);
}

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
  r->base = calloc(Q_STACK_SIZE, sizeof(q_frame));
  r->top = r->base;
}

static inline void q_init_pairs(q_pairs *p)
{
  p->base = calloc(Q_PAIRS_SIZE, sizeof(q_pair));
  p->top = p->base;
  p->total = 0;
}

static inline void q_push_ret(q_rets *r, q_pairs *p, void *ret)
{
  *r->top = (q_frame) { .ret = ret, .pair_top = p->top };
  r->top++;
  if (r->top - r->base >= Q_STACK_SIZE)
    exit(-1);
}


static inline void q_pop_ret(q_stack *s, q_rets *r, int paramcount, void **next)
{
  if (r->top - r->base == 0)
    exit(-1);

  r->top--;
  q_frame frame = *r->top;

  *next = frame.ret;

  // pop frame copy return value to the top of previous stack frame
  q_value ret_value;
  Q_FETCH(s, 0, &ret_value);
  Q_POP(s, paramcount);
  Q_STORE(s, 0, ret_value);
}

static inline void q_debug_print(q_value x)
{
#ifdef Q_DEBUG
  printf("%x:%ld\n", x.type, x.data);
#endif
}

static inline void q_dump_stack(q_stack *s)
{
  for(q_value *x = s->base; x <= s->top; x++)
  {
    q_debug_print(*x);
  }
}



static inline void q_print_value(q_value x);

static inline void q_print_pair(q_value x)
{
  q_pair pair = *((q_pair*) x.data);

  q_print_value(pair.head);
  printf(" . ");
  q_print_value(pair.tail);
}

static inline void q_print_value(q_value x) {
  switch (x.type)
  {
  case Q_TYPE_NUMBER:
    {
      printf("%ld", x.data);
    } break;
  case Q_TYPE_LAMBDA:
    {
      printf("<lambda %ld>", x.data);
    } break;
  case Q_TYPE_PAIR:
    {
      printf("(");
      q_print_pair(x);
      printf(")");
    } break;
  }
}

static inline void q_print(q_stack* s)
{
  q_value x;
  Q_FETCH(s, 0, &x);
  
  q_print_value(x);
  printf("\n");

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

static inline void q_compact_pairs(q_stack *s, q_pairs *p)
{
  q_fatal("todo");
}

static inline void q_make_pair(q_pairs *p, q_stack *s)
{
  q_value head, tail;
  Q_FETCH(s, 0, &head);
  Q_FETCH(s, 1, &tail);

  if (p->total == Q_PAIRS_SIZE)
    q_fatal("Out of memory");

  if (p->top - p->base == Q_PAIRS_SIZE)
    q_compact_pairs(s, p);
  
  *p->top = (q_pair) { .head=head, .tail=tail };

  Q_POP(s, 2);
  Q_PUSH(s, 1);

  Q_STORE(s, 0, Q_PAIR(p->top));

  p->top++;
  p->total++;
}

static inline void q_call(q_stack *s, void** next)
{
  q_value f;
  Q_FETCH(s, 0, &f);

  if (f.type == Q_TYPE_LAMBDA)
  {
#ifdef Q_DEBUG
    printf("Calling %lx. s=%ld\n", f.data, s->top - s->base);
#endif
    *next = (void*)f.data;
    Q_POP(s, 1);
//    q_dump_stack(s);
  }
  else
  {
    fprintf(stderr, "Tried to call value of type %x. Aborting\n", f.type);
    exit(-1);
  }
}

static inline void q_call_tail(q_stack *s, q_pairs *p, q_rets *r, uint64_t argcount, uint64_t paramcount, void** next)
{
  q_value temp;
  q_call(s, next);

  if (paramcount > 0)
  {
    for (int64_t i = 0; i < argcount; i++)
    {
      Q_FETCH(s, argcount - i - 1, &temp);
      Q_STORE(s, argcount + paramcount - i - 1, temp);
    }
    Q_POP(s, paramcount);
  }
  
  (r->top - 1)->pair_top = p->top; // increase range of pairs of present frame
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

  Q_STORE(s, 0, Q_NUMBER((uint64_t) (((int64_t) a.data) + ((int64_t) b.data))));
}

static inline void q_mul(q_stack *s)
{
  q_value a, b;

  Q_FETCH(s, 0, &a);
  Q_FETCH(s, 1, &b);

  Q_POP(s, 2);
  Q_PUSH(s, 1);

  Q_STORE(s, 0, Q_NUMBER((uint64_t) (((int64_t) a.data) * ((int64_t) b.data))));
}

static inline void q_sub(q_stack *s)
{
  q_value a, b;

  Q_FETCH(s, 0, &a);
  Q_FETCH(s, 1, &b);

  Q_POP(s, 2);
  Q_PUSH(s, 1);

  Q_STORE(s, 0, Q_NUMBER((uint64_t) (((int64_t) a.data) - ((int64_t) b.data))));
}

static inline void q_div(q_stack *s)
{
  q_value a, b;

  Q_FETCH(s, 0, &a);
  Q_FETCH(s, 1, &b);

  Q_POP(s, 2);
  Q_PUSH(s, 1);

  Q_STORE(s, 0, Q_NUMBER((uint64_t) (((int64_t) a.data) / ((int64_t) b.data))));
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

static inline void q_drop(q_stack *s, q_pairs *p, q_rets *r) 
{
  Q_POP(s, 1);
  p->top = (r->top - 1)->pair_top; // pop pairs due to return value being dropped
}

#endif
