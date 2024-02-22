P=program
LDFLAGS=
CFLAGS=-g -Wall -fstack-protector -O0 -fno-strict-aliasing -fno-semantic-interposition 
CC=gcc
OBJECTS=a.o

$(P): $(OBJECTS)
	$(CC) -o $(P) $(OBJECTS)

%.c: %.scm qlisp.scm
	cat $< | ./repl qlisp.scm > $@

clean: 
	rm -f $(OBJECTS)
	rm -f $(P)
