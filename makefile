P=program
SCRIPTS=main.fs person.fs std.fs
SYMBOLS=symbols.fs
DEPS=$(wildcard q/*.scm)
LISP=q/qlisp.scm

$(P): $(SCRIPTS) $(SYMBOLS)

%.fs: %.scm $(DEPS)
	cat $< | racket $(LISP) --build $(basename $@) > $@

symbols.fs: $(SCRIPTS)
	cat $(SCRIPTS:.fs=.scm) | racket $(LISP) --extract-symbols ee > $@

clean:
	rm -f $(SCRIPTS)
	rm -f $(SYMBOLS)
	rm -f $(P)
