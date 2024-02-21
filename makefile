P=out
LDFLAGS=
CFLAGS=-g -Wall -fstack-protector -O0 -fno-strict-aliasing -fno-semantic-interposition 
CC=gcc
OBJECTS=a.o b.o

$(P): $(OBJECTS)

%.c: %.scm
	cat $> | ./repl qlisp.scm > $@

clean: 
	rm -f $(OBJECTS)
	rm -f $(P)
