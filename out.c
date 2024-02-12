#include "qruntime.h"

int main() {
	q_stack s;
	s.base = calloc(Q_STACK_SIZE, sizeof(q_value));
	s.top = s.base;
	void *next = NULL;
	printf("Starting program..\n");
	goto f_1;

f_0:
{

	Q_RESIZE(&s, 0);

	q_exit(&s);
	goto f_0;
}

f_1:
{

	Q_RESIZE(&s, 0);

	q_make_lambda(&s, &&f_3);
	goto f_2;
}

f_2:
{
	q_value t_f = Q_FETCH(&s, 0);

	Q_RESIZE(&s, 1);
	Q_STORE(&s, 0, t_f);

	q_call(&s, &next);
	goto *next;
}

f_3:
{

	Q_RESIZE(&s, 1);
	Q_STORE(&s, 0, Q_NUMBER(69420));

	q_print(&s);
	goto f_1;
}

	return 0;
}

