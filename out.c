#include "qruntime.h"

int main() {
	q_stack s;
	s.base = calloc(Q_STACK_SIZE, sizeof(q_value));
	s.top = s.base;
	void *next = NULL;
	goto f_0;

f_0:
{

	Q_POP(&s, 0);
	Q_PUSH(&s, 1);
	Q_STORE(&s, 0, Q_NUMBER(128));

	q_alloc(&s);
	goto f_1;
}

f_1:
{
	q_value t_m = Q_FETCH(&s, 0);

	Q_POP(&s, 1);
	Q_PUSH(&s, 4);
	Q_STORE(&s, 3, t_m);
	Q_STORE(&s, 2, Q_NUMBER(69));
	Q_STORE(&s, 1, Q_NUMBER(12));
	Q_STORE(&s, 0, t_m);

	q_store(&s);
	goto f_2;
}

f_2:
{
	q_value t_m = Q_FETCH(&s, 0);

	Q_POP(&s, 1);
	Q_PUSH(&s, 2);
	Q_STORE(&s, 1, Q_NUMBER(12));
	Q_STORE(&s, 0, t_m);

	q_load(&s);
	goto f_3;
}

f_3:
{
	q_value t_x = Q_FETCH(&s, 0);

	Q_POP(&s, 1);
	Q_PUSH(&s, 1);
	Q_STORE(&s, 0, t_x);

	q_print(&s);
	goto f_4;
}

f_4:
{

	Q_POP(&s, 0);
	Q_PUSH(&s, 0);

	q_exit(&s);
}

	return 0;
}

