P=program
LDFLAGS=
CFLAGS=-g -Wall -fstack-protector -O0 -fno-strict-aliasing -fno-semantic-interposition 
CC=gcc
OBJECTS=a.o b.o main.o
DEPS=qruntime.h

$(P): $(OBJECTS)
	$(CC) -o $(P) $(OBJECTS)

%.c: %.scm qlisp.scm $(DEPS)
	cat $< | ./repl qlisp.scm $(basename $@) > $@

clean: 
	rm -f $(OBJECTS)
	rm -f $(P)
