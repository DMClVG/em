P=program
LDFLAGS=
CFLAGS=-g -Wall -fstack-protector -O0 -fno-strict-aliasing -fno-semantic-interposition # -DQ_DEBUG
CC=gcc
SCRIPTS=a.fs b.fs
MAIN=main.o
SYMBOLS=symbols.fs
DEPS=qruntime.fs

$(P): $(SCRIPTS) $(SYMBOLS)

%.fs: %.scm qlisp.scm $(DEPS)
	cat $< | ./repl qlisp.scm --build $(basename $@) > $@

symbols.fs: $(SCRIPTS)
	cat $(SCRIPTS:.fs=.scm) | ./repl qlisp.scm --extract-symbols ee > $@

clean: 
	rm -f $(OBJECTS)
	rm -f $(MAIN)
	rm -f $(SYMBOLS)
	rm -f $(P)
