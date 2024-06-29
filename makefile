P=program
SCRIPTS=a.fs
SYMBOLS=symbols.fs
DEPS=qruntime.fs

$(P): $(SCRIPTS) $(SYMBOLS)

%.fs: %.scm qlisp.scm $(DEPS)
	cat $< | racket qlisp.scm --build $(basename $@) > $@

symbols.fs: $(SCRIPTS)
	cat $(SCRIPTS:.fs=.scm) | racket qlisp.scm --extract-symbols ee > $@

clean: 
	rm -f $(SCRIPTS)
	rm -f $(SYMBOLS)
	rm -f $(P)
