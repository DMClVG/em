 

typedef struct {
  uint8_t type;
  uint8_t value;
} q_value;

typedef struct {
  q_value x;
  q_value y;
} q_pair;

typedef struct {
  q_value* base;
  q_value* top;
} q_stack;

#define Q_STACK_SIZE 1024

void start_p(q_stack* stack)
{
}


int main()
{
  q_value a;
  q_value b;

  return 0;
}
