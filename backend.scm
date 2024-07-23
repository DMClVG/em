#lang racket
(provide to-c stitch-program dedupe)
(provide stitch-symbol-constants)
(require "utils.scm")

(define (delimited-list ls del)
  (if (null? ls)
    '()
    (if (null? (cdr ls))
      (cons (car ls) '())
      (cons (car ls) (cons del (delimited-list (cdr ls) del))))))

(define (quote-to-c x symbols) ;; TODO: remove this
  (cond
    ((pair? x)
     (call-with-values
       (lambda ()
         (quote-to-c (car x) symbols))
       (lambda (left symbols2)
         (call-with-values
           (lambda () (quote-to-c (cdr x) symbols2))
           (lambda (right symbols3)
             (values
               (string-append "Q_PAIR(&((q_pair) { .head="left", .tail="right" }))")
               symbols3))))))
    ((null? x) (values "Q_NULL" symbols))
    ((symbol? x)
     (values
       (string-append "Q_SYMBOL("(symbol->cdef x)")")
       (cons x symbols)))
    (else (values "" symbols))))

(define (padleft s char n)
  (string-append
    (list->string (repeat char (- n (string-length s))))
    s))

(define (repeat x n)
  (let loop ((n n))
    (if (<= n 0)
      '()
      (cons x (loop (- n 1))))))

(define (define->cdefine name)
  (string-append "d-" (symbol->cidentifier name)))

(define (string-join ls del)
  (apply string-append (delimited-list ls del)))

(define (function-signature name)
  (string-append ": "name))

(define (symbol->cidentifier sym)
  (string-join
    (map
      (lambda (x)
        (let ((c (char->integer x)))
          (cond
            ((equal? 95 c)
             "__")
            ((or
               (and
                 (>= c 65)
                 (<= c 90))
               (and
                 (>= c 97)
                 (<= c 122))
               (and
                 (>= c 48)
                 (<= c 57)))
             (list->string (list (integer->char c))))

            (else
              (let ((hexchar (number->hex c)))
                (if (> (string-length hexchar) 4)
                  (string-append "_U"(padleft hexchar #\0 8))
                  (string-append "_u"(padleft hexchar #\0 4))))))))

      (string->list (symbol->string sym)))
    ""))

(define (number->hex n)
  (list->string
    (reverse
      (let loop ((n n))
        (cons
          (list-ref '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F) (modulo n 16))
          (if (equal? (floor (/ n 16)) 0)
            '()
            (loop (floor (/ n 16)))))))))
(define (lambda->label id)
  (string-append "f-" (number->string id)))
(define (symbol->cdef sym)
  (string-append "s-" (symbol->cidentifier sym)))

(define (op-to-c-call op)
  (case op
    ('+ "q+")
    ('- "q-")
    ('* "q*")
    ('/ "q/")
    ('equal? "q-equal?")
    ('eq? "q-eq?")
    ('= "q=")
    ('and "q-and")
    ('or "q-or")
    ('not "q-not")
    ('cons "q-pair")
    ('car "q-car")
    ('cdr "q-cdr")
    ('display "q-display")
    ('newline "q-newline")
    ('boolean? "q-boolean?")
    ('null? "q-null?")
    ('procedure? "q-procedure?")
    ('pair? "q-pair?")
    ('number? "q-number?")
    ('symbol? "q-symbol?")
    ('< "q<")
    ('> "q>")
    ('<= "q<=")
    ('>= "q>=")))

(define (stitch-lambda module id l)
  (string-append
    (function-signature (lambda->label id))"\n"
    (string-join (reverse l) "\n")
    " ;"))

(define (stitch-lambdas module lambdas)
  (string-join
    (let loop ((lambdas (reverse lambdas)) (id 0))
         (if (null? lambdas)
          '()
           (cons (stitch-lambda module id (car lambdas)) (loop (cdr lambdas) (+ 1 id)))))
    "\n\n"))


(define (stitch-defines defines)
  (string-join
    (let loop ((ls defines))
       (if (null? ls)
        '()
        (cons (string-append "variable "  (define->cdefine (car ls)) "\n: " (symbol->string (car ls)) " " (define->cdefine (car ls)) " @ check-lambda execute ;") (loop (cdr ls)))))
    "\n"))


(define (stitch-imports imports)
  (string-join
    (let loop ((ls imports))
       (if (null? ls)
        '()
        (cons
          (string-append "")
          (loop (cdr ls)))))
    "\n"))

(define (stitch-extern-defines defines)
  (string-join
    (let loop ((ls defines))
       (if (null? ls)
        '()
        (cons "" (loop (cdr ls)))))
    "\n"))

(define (filter p ls)
  (if (pair? ls)
    (if (p (car ls))
      (cons (car ls) (filter p (cdr ls)))
      (filter p (cdr ls)))
    '()))

(define (difference a b)
   (filter (lambda (x) (not (member x b))) a))

(define (top-level-function-signature module)
  (function-signature (string-append module"-toplevel")))

(define (stitch-symbols symbols)
  (string-join
    (map
      (lambda (sym)
        "")
      symbols)
    "\n"))


(define (stitch-quotes-definitions quotes)
  (string-join
    (let loop ((quotes quotes) (n 0))
      (if (null? quotes)
        '()
        (cons
         (string-append "variable qt-"(number->string n))
         (loop
           (cdr quotes)
           (+ n 1)))))
    "\n"))

(define (quote-init n id qt ls)
  (cond
    ((pair? qt)
     (call-with-values
       (lambda () (quote-init n id (car qt) ls))
       (lambda (ls2 n2)
         (call-with-values
           (lambda () (quote-init n2 id (cdr qt) ls2))
           (lambda (ls3 n3)
             (values
               (append
                 ls3
                 (list
                   (string-append "q-pair")
                   ))

               (+ 1 n3)))))))

    ((symbol? qt)
     (values
       (append
         ls
         (list
          (string-append (symbol->cdef qt))))

       (+ n 1)))
    ((number? qt)
     (values
       (append
         ls
         (list (string-append (number->string qt) " q-number")))

       (+ n 1)))
    ((null? qt)
     (values
       (append
         ls
         (list "q-null"))

       (+ n 1)))
    ((boolean? qt)
     (values
         (append
           ls
           (list
             (string-append (if qt "true q-bool" "false q-bool"))))
         (+ n 1)))
    (else (error "quote not supported"))))

(define (initialize-quote id qt ls)
  (call-with-values
    (lambda ()
      (quote-init
        0
        id
        qt
        '()))
    (lambda (ls n)
      (append
        ls
        (list (string-append id " !"))))))

(define (stitch-quotes-initialization quotes)
  (string-join
    (let loop ((quotes quotes) (n (- (length quotes) 1)))
      (if (null? quotes)
        '()
         (cons
          (string-join
            (initialize-quote
              (string-append "qt-"(number->string n))
              (car quotes)
              '())
           "\n")
          (loop (cdr quotes) (- n 1)))))
    "\n"))

(define (stitch-program module lambdas defines fetches imports symbols quotes debug?)
  (string-join
    `(
      ;; ,(if debug? (string-append ": rl postpone include " module ".fs ;"))
      "\\ defines"
      ,(stitch-defines (dedupe defines))
      ""
      "\\ extern"
      ,(stitch-extern-defines (difference (dedupe fetches) defines))
      ""
      "\\ imports"
      ,(stitch-imports (dedupe imports))
      ""
      "\\ symbols"
      ,(stitch-symbols (dedupe symbols))
      ""
      "\\ quotes"
      ,(stitch-quotes-definitions quotes)
      ""
      "\\ lambdas"
      ,(stitch-lambdas module lambdas)
      ""
      "\\ toplevel"
      ,(string-append (top-level-function-signature module) "")
      ,(stitch-quotes-initialization quotes)
      ,(string-append (lambda->label (- (length lambdas) 1)) " ;")
      )
    "\n"))

;;(begin
;;  "tests"
;;  (trace
;;    (syntax-lambda
;;      '(a b c)
;;      '((print (+ (+ a c) b))
;;        (print 0))
;;      '())
;;
;;    (evaluate-thunk '((1) (+ 1 2)) '() '())))

(define (read-all port)
  (let ((read-value (read port)))
    (if (eof-object? read-value)
      '()
      (cons read-value (read-all port)))))

(define (command-line)
  (let ((lst '()))
    (with-input-from-file "/proc/self/cmdline"
      (lambda ()
       (do ((c (read-char) (read-char))
            (s ""))
        ((eof-object? c)
         (reverse lst))
        (if (char=? c #\null)
         (begin
          (set! lst (cons s lst))
          (set! s ""))
         (set! s (string-append s (string c)))))))))

(define (stitch-symbol-constants symbols)
  (string-join
    (map
      (lambda (sym)
        (string-append "s\" "(symbol->string sym)"\" q-symbol constant "(symbol->cdef sym)))
      symbols)
    "\n"))

(define (to-c ir)

  (let next ((op ir) (code '()) (lambdas '()) (defines '()) (fetches '()) (imports '()) (symbols '()) (quotes '()) (paramcount 0))
    (if (null? op)
      (values
        (cons
          (cons
            (string-append "1 " (number->string paramcount)" shove-back")   ;; copy result to the top of parent's stackframe
            code)
          lambdas)
        defines
        fetches
        imports
        symbols
        quotes)

      (case (car op)

        ((closure)
         (let
           ((paramcount2 (list-ref op 1))
            (freecount2 (list-ref op 2))
            (thunk (list-ref op 3))
            (cont (list-ref op 4)))

           (call-with-values
             (lambda () (next thunk '() lambdas defines fetches imports symbols quotes paramcount2))

             (lambda (lambdas2 defines2 fetches2 imports2 symbols2 quotes2)
               (next
                 cont
                 (cons
                   (string-append
                     "['] "
                     (lambda->label (- (length lambdas2) 1))
                     " q-lambda")
                   code)
                 lambdas2
                 defines2
                 fetches2
                 imports2
                 symbols2
                 quotes2
                 paramcount)))))

        ((branch)
         (let
           ((iftrue (list-ref op 1))
            (iffalse (list-ref op 2)))

           (call-with-values
             (lambda () (next iftrue '() lambdas defines fetches imports symbols quotes paramcount))

             (lambda (lambdas2 defines2 fetches2 imports2 symbols2 quotes2)
               (call-with-values
                 (lambda () (next iffalse '() lambdas2 defines2 fetches2 imports2 symbols2 quotes2 paramcount))

                 (lambda (lambdas3 defines3 fetches3 imports3 symbols3 quotes3)
                   (values
                     (cons (cons
                             (string-append
                               "q? if "
                               (lambda->label (- (length lambdas2) 1)) ; if true
                               " else "
                               (lambda->label (- (length lambdas3) 1)) ; if false
                               " then")
                             code) lambdas3)
                     defines3
                     fetches3
                     imports3
                     symbols3
                     quotes3)))))))

        ((call)
         (let
           ((argcount (list-ref op 1))
            (cont (list-ref op 2)))

           (if (null? cont) ;; at tail position?
              ;;#f
             (values
               (cons
                 (cons
                   ""
                   (cons
		    (string-append (number->string argcount)" "(number->string paramcount) " shove-back")
		    code))
                 lambdas)
               defines
               fetches
               imports
               symbols
               quotes)

             (call-with-values
               (lambda () (next cont '() lambdas defines fetches imports symbols quotes paramcount))

               (lambda (lambdas2 defines2 fetches2 imports2 symbols2 quotes2)
                 (values
                   (cons
                     (cons
                       (string-append (lambda->label (- (length lambdas2) 1)))
                        code)
                     lambdas2)
                   defines2
                   fetches2
                   imports2
                   symbols2
                   quotes2))))))

        ((import)
         (let
           ((module (list-ref op 1))
            (cont (list-ref op 2)))

           (call-with-values
            (lambda () (next cont '() lambdas defines fetches imports symbols quotes paramcount))

            (lambda (lambdas2 defines2 fetches2 imports2 symbols2 quotes2)
              (values
                (cons
                  (cons
                    (string-append (lambda->label (- (length lambdas2) 1)))
                    (cons
                      (string-append (symbol->cidentifier module)"-toplevel")
                      code))
                  lambdas2)
                defines2
                fetches2
                (cons (string-append (symbol->cidentifier module) "-toplevel") imports2)
                symbols2
                quotes2)))))

        ((+ - * / = eq? equal? cons car cdr display newline and or not boolean? null? pair? procedure? number? symbol? >= <= > <) ;; ops
         (next
           (list-ref op 1) ;; cont
           (cons (op-to-c-call (car op)) code)
           lambdas
           defines
           fetches
           imports
           symbols
           quotes
           paramcount))

        ((native)
         (let
           ((name (list-ref op 1))
            (cont (list-ref op 2)))

           (next
             (list-ref op 2) ;; cont
             (cons
               (string-append "q_make_lambda(q, "name");")
               code)
             lambdas
             defines
             fetches
             (cons name imports)
             symbols
             quotes
             paramcount)))

        ((define)
         (let
           ((name (list-ref op 1))
            (cont (list-ref op 2)))

           (next
             cont
             (cons (string-append "dup " (define->cdefine name) " !") code)
             lambdas
             (cons name defines)
             fetches
             imports
             symbols
             quotes
             paramcount)))

        ((number)
         (let
           ((x (list-ref op 1))
            (cont (list-ref op 2)))
           (next
             cont
             (cons (string-append (number->string (list-ref op 1)) " q-number") code)
             lambdas
             defines
             fetches
             imports
             symbols
             quotes
             paramcount)))

        ((boolean)
         (let
           ((x (list-ref op 1))
            (cont (list-ref op 2)))
           (next
             cont
             (cons
               (string-append
                 (if x
                   "true q-bool"
                   "false q-bool"))
               code)
             lambdas
             defines
             fetches
             imports
             symbols
             quotes
             paramcount)))

        ((fetch)
         (let
            ((name (list-ref op 1))
             (cont (list-ref op 2)))
           (next
             cont
             (cons
               (string-append (symbol->string name))
               code)
             lambdas
             defines
             (cons name fetches)
             imports
             symbols
             quotes
             paramcount)))

        ((drop)
         (let
           ((cont (list-ref op 1)))
           (next
             cont
             (cons
               "drop"
               code)
             lambdas
             defines
             fetches
             imports
             symbols
             quotes
             paramcount)))

        ((push-frame)
         (let
           ((cont (list-ref op 1)))
           (next
             cont
             code
             lambdas
             defines
             fetches
             imports
             symbols
             quotes
             paramcount)))

        ((pop-frame)
         (let
           ((n (list-ref op 1))
            (cont (list-ref op 2)))
           (next
             cont
             (cons
               (string-append "1 "(number->string n)" shove-back")
               code)
             lambdas
             defines
             fetches
             imports
             symbols
             quotes
             paramcount)))

        ((quote)
          (let
            ((value (list-ref op 1))
             (cont (list-ref op 2)))
            (let
              ((quoted-value (call-with-values (lambda () (quote-to-c value symbols)) list)))
              (next
                  cont
                  (cons
                     (string-append "qt-"(number->string (length quotes))" @")
                     code)
                  lambdas
                  defines
                  fetches
                  imports
                  (list-ref quoted-value 1)
                  (cons value quotes)
                  paramcount))))

        ((free)
         (let
           ((n (list-ref op 1))
            (cont (list-ref op 2)))
          (next
            cont
            (cons
              "Q_STORE(q, 0, temp);"
              (cons
                "Q_PUSH(q, 1);"
                (cons
                  (string-append "Q_ENV(q, " (number->string n) ", &temp);")
                  code)))
            lambdas
            defines
            fetches
            imports
            symbols
            quotes
            paramcount)))

        ((pick)
         (let
            ((n (list-ref op 1))
             (cont (list-ref op 2)))
           (next
             cont
             (cons
               (string-append (number->string n) " pick")
                   code)
             lambdas
             defines
             fetches
             imports
             symbols
             quotes
             paramcount)))

        (else (error "bad operation" op))))))
