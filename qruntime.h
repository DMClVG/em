#ifndef Q_RUNTIME 
#define Q_RUNTIME
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#define Q_TYPE_NUMBER 1
#define Q_TYPE_PAIR 2
#define Q_TYPE_BUFFER 4
#define Q_TYPE_SYMBOL 5
#define Q_TYPE_BOOL 6
#define Q_TYPE_NULL 7
#define Q_TYPE_LAMBDA 8

typedef struct {
  uint64_t meta;
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
  q_value* env;
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

typedef struct {
  q_stack stack;
  q_rets rets;
  q_pairs pairs;
  //q_stack env_stack; 
  q_value* env;
} q_run;

typedef void (*q_function)(q_run* q, void **next);

#define Q_NUMBER(n) ((q_value) { .meta = Q_TYPE_NUMBER, .data = n } )
#define Q_PAIR(p) ((q_value) { .meta = Q_TYPE_PAIR, .data = (uint64_t) (p) } )
#define Q_LAMBDA(f) ((q_value) { .meta = Q_TYPE_LAMBDA, .data = (uint64_t)(f) } )
#define Q_CLOSURE(env, f) ((q_value) { .meta = (uint64_t)(env), .data = (uint64_t)(f) } )
#define Q_BUFFER(p) ((q_value) { .meta = Q_TYPE_BUFFER, .data = (uint64_t)(p) } )
#define Q_SYMBOL(s) ((q_value) { .meta = Q_TYPE_SYMBOL, .data = (uint64_t)(s) } )
#define Q_BOOL(x) ((q_value) { .meta = Q_TYPE_BOOL, .data = x })

#define Q_NULL ((q_value) { .meta = Q_TYPE_NULL, .data = 0 })
#define Q_TRUE Q_BOOL(1)
#define Q_FALSE Q_BOOL(0)

#define Q_STORE(q, n, v) { q_stack *s = &q->stack; q_check_stack_in_bounds(s, n); (s)->top[-(n)] = v; }
#define Q_FETCH(q, n, v) { q_stack *s = &q->stack; q_check_stack_in_bounds(s, n); *(v) = (s)->top[-(n)]; }
// #define Q_RESIZE(s, n) (s)->top = (s)->base + n;
#define Q_POP(q, n) { q_stack *s = &q->stack; q_check_stack_underflow(s, n); (s)->top -= n; }
#define Q_PUSH(q, n) { q_stack *s = &q->stack; q_check_stack_overflow(s, n); (s)->top += n; }
#define Q_BRANCH(q, a, b, next) { q_stack *s = &q->stack; if((s)->top[0].data) { Q_POP(q, 1); *(next) = a; } else { Q_POP(q, 1); *(next) = b; } }
#define Q_ENV(q, n, v) { q_value *env = q->env; *(v) = env[n]; }

#define Q_STACK_SIZE 1024
#define Q_PAIRS_SIZE 1024

static inline void q_fatal(const char *msg)
{
  printf("%s\n", msg);
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

static inline void q_init(q_run *q)
{
  q_init_stack(&q->stack);
  q_init_pairs(&q->pairs);
  q_init_rets(&q->rets);
  q->env = NULL;
}

static inline void q_push_ret(q_run *q, void *ret)
{
  q_pairs *p = &q->pairs;
  q_rets *r = &q->rets;

  *r->top = (q_frame) { .env = q->env, .ret = ret, .pair_top = p->top };
  r->top++;
  if (r->top - r->base >= Q_STACK_SIZE)
    exit(-1);
}


static inline void q_pop_ret(q_run *q, int paramcount, void **next)
{
  q_rets *r = &q->rets;

  if (r->top - r->base == 0)
    exit(-1);

  r->top--;
  q_frame frame = *r->top;

  if(next != NULL)
  {
    *next = frame.ret;
    q->env = frame.env;
  }

  // pop frame copy return value to the top of previous stack frame
  q_value ret_value;
  Q_FETCH(q, 0, &ret_value);
  Q_POP(q, paramcount);
  Q_STORE(q, 0, ret_value);
}


static inline void q_pop_keep_ret(q_run *q, int frame_size)
{
  q_value ret_value;
  Q_FETCH(q, 0, &ret_value);
  Q_POP(q, frame_size);
  Q_STORE(q, 0, ret_value);
}


static inline void q_debug_print(q_value x)
{
#ifdef Q_DEBUG
  printf("%x:%ld\n", x.meta x.data);
#endif
}

static inline void q_dump_stack(q_run *q)
{
  q_stack *s = &q->stack;
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
  if (pair.tail.meta == Q_TYPE_PAIR)
  {
    printf(" ");
    q_print_pair(pair.tail);
  } 
  else if (pair.tail.meta != Q_TYPE_NULL)
  {
    printf(" . ");
    q_print_value(pair.tail);
  }
}

static inline void q_print_value(q_value x) {
  switch (x.meta)
  {
  case Q_TYPE_NUMBER:
    {
      printf("%ld", x.data);
    } break;
  case Q_TYPE_BOOL:
    {
      if(x.data)
        printf("#t");
      else
        printf("#f");

    } break;
  case Q_TYPE_LAMBDA:
    {
      printf("<lambda 0x%lx>", x.data);
    } break;
  case Q_TYPE_SYMBOL:
    {
      printf("%s", (const char*) x.data);
    } break;
  case Q_TYPE_NULL:
    {
      printf("()");
    } break;
  case Q_TYPE_PAIR:
    {
      printf("(");
      q_print_pair(x);
      printf(")");
    } break;
  }
}

static inline void q_display(q_run *q)
{
  q_value x;
  Q_FETCH(q, 0, &x);
  
  q_print_value(x);

  Q_POP(q, 1);
  Q_PUSH(q, 1);
  Q_STORE(q, 0, Q_NUMBER(0));
}

static inline void q_newline(q_run *q)
{
  printf("\n");
  Q_PUSH(q, 1);
  Q_STORE(q, 0, Q_NUMBER(0));
}

static inline void q_make_lambda(q_run *q, void* f)
{

  Q_PUSH(q, 1);
  Q_STORE(q, 0, Q_LAMBDA(f));
}


static inline void q_make_closure(q_run *q, int parencount, int freecount, void* f)
{
  q_value *new_env = calloc(freecount, sizeof(q_value));
  if (q->env == NULL)
    parencount = 0;

  for(int i = 0; i < freecount - parencount; i++)
  {
    Q_FETCH(q, i, &new_env[i]);
  }
  for(int i = 0; i < parencount; i++)
  {
    new_env[freecount - 1 + i] = q->env[i];
  }
  Q_PUSH(q, 1);
  Q_STORE(q, 0, Q_CLOSURE(new_env, f));
}

static inline void q_compact_pairs(q_run *q)
{
  q_fatal("todo");
}

static inline void q_make_pair(q_run *q)
{
  q_pairs *p = &q->pairs;

  q_value head, tail;
  Q_FETCH(q, 0, &head);
  Q_FETCH(q, 1, &tail);

  if (p->total == Q_PAIRS_SIZE)
    q_fatal("Out of memory");

  if (p->top - p->base == Q_PAIRS_SIZE)
    q_compact_pairs(q);
  
  *p->top = (q_pair) { .head=head, .tail=tail };

  Q_POP(q, 2);
  Q_PUSH(q, 1);

  Q_STORE(q, 0, Q_PAIR(p->top));

  p->top++;
  p->total++;
}

static inline void q_call(q_run *q, void** next)
{
  q_value f;
  Q_FETCH(q, 0, &f);

  if (f.meta >= Q_TYPE_LAMBDA)
  {
#ifdef Q_DEBUG
    q_stack *s = &q->stack;
    printf("Calling %lx. s=%ld\n", f.data, s->top - s->base);
#endif
    *next = (void*)f.data;
    q->env = (q_value*)f.meta;
    Q_POP(q, 1);
  }
  else
  {
    fprintf(stderr, "Tried to call value of type %lx. Aborting\n", f.meta);
    exit(-1);
  }
}

static inline void q_call_tail(q_run *q, uint64_t argcount, uint64_t paramcount, void** next)
{
  q_pairs *p = &q->pairs;
  q_rets *r = &q->rets;

  q_value temp;
  q_call(q, next);

  if (paramcount > 0)
  {
    for (int64_t i = 0; i < argcount; i++)
    {
      Q_FETCH(q, argcount - i - 1, &temp);
      Q_STORE(q, argcount + paramcount - i - 1, temp);
    }
    Q_POP(q, paramcount);
  }
  
  (r->top - 1)->pair_top = p->top; // increase range of pairs of present frame
}

static inline void q_exit(q_run *q)
{
  exit(0);
}

static inline void q_add(q_run *q)
{
  q_value a, b;

  Q_FETCH(q, 0, &a);
  Q_FETCH(q, 1, &b);

  Q_POP(q, 2);
  Q_PUSH(q, 1);

  Q_STORE(q, 0, Q_NUMBER((uint64_t) (((int64_t) a.data) + ((int64_t) b.data))));
}

static inline void q_mul(q_run *q)
{
  q_value a, b;

  Q_FETCH(q, 0, &a);
  Q_FETCH(q, 1, &b);

  Q_POP(q, 2);
  Q_PUSH(q, 1);

  Q_STORE(q, 0, Q_NUMBER((uint64_t) (((int64_t) a.data) * ((int64_t) b.data))));
}

static inline void q_sub(q_run *q)
{
  q_value a, b;

  Q_FETCH(q, 0, &a);
  Q_FETCH(q, 1, &b);

  Q_POP(q, 2);
  Q_PUSH(q, 1);

  Q_STORE(q, 0, Q_NUMBER((uint64_t) (((int64_t) a.data) - ((int64_t) b.data))));
}

static inline void q_div(q_run *q)
{
  q_value a, b;

  Q_FETCH(q, 0, &a);
  Q_FETCH(q, 1, &b);

  Q_POP(q, 2);
  Q_PUSH(q, 1);

  Q_STORE(q, 0, Q_NUMBER((uint64_t) (((int64_t) a.data) / ((int64_t) b.data))));
}

static inline void q_is_equal(q_run *q)
{
  q_value a, b;

  Q_FETCH(q, 0, &a);
  Q_FETCH(q, 1, &b);

  Q_POP(q, 2);
  Q_PUSH(q, 1);

  Q_STORE(q, 0, Q_BOOL(a.meta == b.meta && a.data == b.data));
}


static inline void q_and(q_run *q)
{
  q_value a, b;

  Q_FETCH(q, 0, &a);
  Q_FETCH(q, 1, &b);

  Q_POP(q, 2);
  Q_PUSH(q, 1);

  Q_STORE(q, 0, Q_BOOL(a.data && b.data));
}

static inline void q_or(q_run *q)
{
  q_value a, b;

  Q_FETCH(q, 0, &a);
  Q_FETCH(q, 1, &b);

  Q_POP(q, 2);
  Q_PUSH(q, 1);

  Q_STORE(q, 0, Q_BOOL(a.data || b.data));
}


static inline void q_not(q_run *q)
{
  q_value x;

  Q_FETCH(q, 0, &x);

  Q_POP(q, 1);
  Q_PUSH(q, 1);

  Q_STORE(q, 0, Q_BOOL(!x.data));
}

static inline void q_is_greater(q_run *q)
{
  q_value a, b;

  Q_FETCH(q, 0, &a);
  Q_FETCH(q, 1, &b);

  Q_POP(q, 2);
  Q_PUSH(q, 1);

  Q_STORE(q, 0, Q_BOOL((int64_t)a.data > (int64_t)b.data));
}

static inline void q_is_lesser(q_run *q)
{
  q_value a, b;

  Q_FETCH(q, 0, &a);
  Q_FETCH(q, 1, &b);

  Q_POP(q, 2);
  Q_PUSH(q, 1);

  Q_STORE(q, 0, Q_BOOL((int64_t)a.data < (int64_t)b.data));
}

static inline void q_is_lesser_or_eq(q_run *q)
{
  q_value a, b;

  Q_FETCH(q, 0, &a);
  Q_FETCH(q, 1, &b);

  Q_POP(q, 2);
  Q_PUSH(q, 1);

  Q_STORE(q, 0, Q_BOOL((int64_t)a.data <= (int64_t)b.data));
}

static inline void q_is_greater_or_eq(q_run *q)
{
  q_value a, b;

  Q_FETCH(q, 0, &a);
  Q_FETCH(q, 1, &b);

  Q_POP(q, 2);
  Q_PUSH(q, 1);

  Q_STORE(q, 0, Q_BOOL((int64_t)a.data >= (int64_t)b.data));
}


static inline void q_is_pair(q_run *q)
{
  q_value x;

  Q_FETCH(q, 0, &x);
  Q_STORE(q, 0, Q_BOOL(x.meta == Q_TYPE_PAIR));
}

static inline void q_is_null(q_run *q)
{
  q_value x;

  Q_FETCH(q, 0, &x);
  Q_STORE(q, 0, Q_BOOL(x.meta == Q_TYPE_NULL));
}

static inline void q_is_procedure(q_run *q)
{
  q_value x;

  Q_FETCH(q, 0, &x);
  Q_STORE(q, 0, Q_BOOL(x.meta >= Q_TYPE_LAMBDA));
}

static inline void q_is_number(q_run *q)
{
  q_value x;

  Q_FETCH(q, 0, &x);
  Q_STORE(q, 0, Q_BOOL(x.meta == Q_TYPE_NUMBER));
}

static inline void q_is_symbol(q_run *q)
{
  q_value x;

  Q_FETCH(q, 0, &x);
  Q_STORE(q, 0, Q_BOOL(x.meta == Q_TYPE_SYMBOL));
}

static inline void q_is_boolean(q_run *q)
{
  q_value x;

  Q_FETCH(q, 0, &x);
  Q_STORE(q, 0, Q_BOOL(x.meta == Q_TYPE_BOOL));
}

static inline void q_drop(q_run *q) 
{
  q_pairs *p = &q->pairs;
  q_rets *r = &q->rets;

  Q_POP(q, 1);
  p->top = (r->top - 1)->pair_top; // pop pairs due to return value being dropped
}

static inline void q_car(q_run *q)
{
  q_value x;
  Q_FETCH(q, 0, &x);
  Q_POP(q, 1);

  if (x.meta != Q_TYPE_PAIR)
    q_fatal("car: expected pair");
  q_pair pair = *((q_pair*)x.data);

  Q_PUSH(q, 1);
  Q_STORE(q, 0, pair.head); 
}

static inline void q_cdr(q_run *q)
{
  q_value x;
  Q_FETCH(q, 0, &x);
  Q_POP(q, 1);

  if (x.meta != Q_TYPE_PAIR)
    q_fatal("cdr: expected pair");
  q_pair pair = *((q_pair*)x.data);

  Q_PUSH(q, 1);
  Q_STORE(q, 0, pair.tail); 
}

#endif
