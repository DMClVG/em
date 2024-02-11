#include "qruntime.h"
#include "qmain.h"

void f_0(q_stack s);
void f_1(q_stack s);
void f_2(q_stack s);
void f_3(q_stack s);

void f_0(q_stack s)
{

  {
	  Q_RESIZE(s, 0);
  }
  void *next = (void*) f_0;

  *(&s) = q_exit(s);
	goto *next;
}

void f_1(q_stack s)
{

  {
	  Q_RESIZE(s, 0);
  }

  void *next = (void*) f_2;
	*(&s) = q_make_lambda(s, f_3);
	goto *next;
}

void f_2(q_stack s)
{
  {
  	q_value t_f = Q_FETCH(s, 0);
  
  	Q_RESIZE(s, 1);
  	Q_STORE(s, 0, t_f);
  }

	q_call(s);
}

void f_3(q_stack s)
{

  {
	  Q_RESIZE(s, 1);
	  Q_STORE(s, 0, Q_NUMBER(69420));
  }

	f_1(q_print(s));
}

int main() { Q_MAIN(f_1); return 0; } 
