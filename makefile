P=out
LDFLAGS=
CFLAGS=-g -Wall -fstack-protector -O0
CC=gcc
OBJECTS=out.o

$(P): $(OBJECTS)

#out.c: q.scm
#	./repl q.scm > $@

#clean: 
#	rm -f out.c
#	rm -f $(OBJECTS)
#	rm -f $(P)
