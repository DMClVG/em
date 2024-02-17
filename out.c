#include "qruntime.h"

int main() {
	q_stack s;
	s.base = calloc(Q_STACK_SIZE, sizeof(q_value));
	s.top = s.base;

	q_stack o;
	o.base = calloc(Q_STACK_SIZE, sizeof(q_value));
	o.top = o.base;

	void *next = NULL;
	goto f_0;

f_0:
{


	Q_POP(&s, 0);
	Q_PUSH(&s, 1);
	Q_STORE(&s, 0, Q_NUMBER(128));

	Q_POP(&o, 0);
	Q_PUSH(&o, 0);

	q_alloc(&s, &o);
	goto f_1;
}

f_1:
{

	q_value o_m;
	Q_FETCH(&o, 0, &o_m);

	Q_POP(&s, 0);
	Q_PUSH(&s, 2);
	Q_STORE(&s, 1, Q_NUMBER(69));
	Q_STORE(&s, 0, Q_NUMBER(12));

	Q_POP(&o, 1);
	Q_PUSH(&o, 1);
	Q_STORE(&o, 0, o_m);

	q_store(&s, &o);
	goto f_2;
}

f_2:
{

	q_value o_m;
	Q_FETCH(&o, 0, &o_m);

	Q_POP(&s, 0);
	Q_PUSH(&s, 1);
	Q_STORE(&s, 0, Q_NUMBER(12));

	Q_POP(&o, 1);
	Q_PUSH(&o, 1);
	Q_STORE(&o, 0, o_m);

	q_load(&s, &o);
	goto f_3;
}

f_3:
{
	q_value t_x;
	Q_FETCH(&s, 0, &t_x);

	q_value o_m;
	Q_FETCH(&o, 0, &o_m);

	Q_POP(&s, 1);
	Q_PUSH(&s, 1);
	Q_STORE(&s, 0, t_x);

	Q_POP(&o, 1);
	Q_PUSH(&o, 1);
	Q_STORE(&o, 0, o_m);

	q_print(&s);
	goto f_4;
}

f_4:
{

	q_value o_m;
	Q_FETCH(&o, 0, &o_m);

	Q_POP(&s, 0);
	Q_PUSH(&s, 0);

	Q_POP(&o, 1);
	Q_PUSH(&o, 1);
	Q_STORE(&o, 0, o_m);

	q_drop(&o);
	goto f_5;
}

f_5:
{


	Q_POP(&s, 0);
	Q_PUSH(&s, 0);

	Q_POP(&o, 0);
	Q_PUSH(&o, 0);

	q_exit(&s);
}

	return 0;
}

