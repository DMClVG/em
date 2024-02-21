P=out
LDFLAGS=
CFLAGS=-g -Wall -fstack-protector -O0 -fno-strict-aliasing -fno-semantic-interposition 
CC=gcc
OBJECTS=out.o

$(P): $(OBJECTS)

out.c: qlisp.scm
	./repl qlisp.scm > $@

clean: 
	rm -f out.c
	rm -f $(OBJECTS)
	rm -f $(P)
