#include "qruntime.h"
#include "qmain.h"

void f_0(q_stack s)
{
	Q_RESIZE(s, 2);
	Q_STORE(s, 1, Q_NUMBER(13123));
	Q_STORE(s, 0, Q_NUMBER(0));

	q_print(s);
}

void f_1(q_stack s)
{
	q_value t_b = Q_FETCH(s, 2);
	q_value t_a = Q_FETCH(s, 1);
	q_value t_k = Q_FETCH(s, 0);


	Q_RESIZE(s, 2);
	Q_STORE(s, 1, t_a);
	Q_STORE(s, 0, t_k);


	q_print(s);
}

q_function Q_GFT[] = {
	f_0,
	f_1
};


