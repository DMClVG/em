#include "qruntime.h"
#include "qmain.h"

void f_0(q_stack s)
{


	Q_RESIZE(s, 1);
	Q_STORE(s, 0, Q_NUMBER(0));


	q_exit(s);
}

void f_1(q_stack s)
{


	Q_RESIZE(s, 3);
	Q_STORE(s, 2, Q_NUMBER(7));
	Q_STORE(s, 1, Q_NUMBER(-8));
	Q_STORE(s, 0, Q_NUMBER(2));


	q_add(s);
}

void f_2(q_stack s)
{
	q_value t_x = Q_FETCH(s, 0);


	Q_RESIZE(s, 4);
	Q_STORE(s, 3, t_x);
	Q_STORE(s, 2, t_x);
	Q_STORE(s, 1, Q_NUMBER(0));
	Q_STORE(s, 0, Q_NUMBER(3));


	q_branch(s);
}

void f_3(q_stack s)
{
	q_value t_x = Q_FETCH(s, 0);


	Q_RESIZE(s, 2);
	Q_STORE(s, 1, t_x);
	Q_STORE(s, 0, Q_NUMBER(0));


	q_print(s);
}

q_function Q_GFT[] = {
	f_0,
	f_1,
	f_2,
	f_3
};



int main() { return q_main(1); } 
