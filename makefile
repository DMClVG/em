P=program
SCRIPTS=a.fs b.fs oop.fs typed.fs
SYMBOLS=symbols.fs
DEPS=qlisp.scm backend.scm env.scm

$(P): $(SCRIPTS) $(SYMBOLS)

%.fs: %.scm $(DEPS)
	cat $< | racket qlisp.scm --build $(basename $@) > $@

symbols.fs: $(SCRIPTS)
	cat $(SCRIPTS:.fs=.scm) | racket qlisp.scm --extract-symbols ee > $@

clean:
	rm -f $(SCRIPTS)
	rm -f $(SYMBOLS)
	rm -f $(P)
