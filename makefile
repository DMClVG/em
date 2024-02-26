P=program
LDFLAGS=
CFLAGS=-g -Wall -fstack-protector -O0 -fno-strict-aliasing -fno-semantic-interposition # -DQ_DEBUG
CC=gcc
OBJECTS=a.o b.o
MAIN=main.o
SYMBOLS=symbols.o
DEPS=qruntime.h

$(P): $(OBJECTS) $(DEPS) $(MAIN) $(SYMBOLS)
	$(CC) -o $(P) $(OBJECTS) $(MAIN) $(SYMBOLS)

%.c: %.scm qlisp.scm $(DEPS)
	cat $< | ./repl qlisp.scm --build $(basename $@) > $@

symbols.c: $(OBJECTS)
	cat $(OBJECTS:.o=.scm) | ./repl qlisp.scm --extract-symbols ee > $@

clean: 
	rm -f $(OBJECTS)
	rm -f $(MAIN)
	rm -f $(SYMBOLS)
	rm -f $(P)
