(define (function-signature name) 
  (string-append "void "name"(q_run *q, void **next)"))

(define (syntax-define head expr env cont)
  (cond 
    ((pair? head) 
     (syntax-define (car head) (list (append `(lambda ,(cdr head)) expr)) env cont)) ;; lambda
    ((symbol? head) 
     (evaluate-expr (car expr) env `(define ,head ,cont))))) ;; normal

(define (syntax-print expr env cont)
  (evaluate-expr expr env `(print ,cont)))

(define (syntax-binary op a b env cont)
  (evaluate-expr b env
    (evaluate-expr a (env-offset env) `(,op ,cont))))

(define (env-offset env)
  (map (lambda (x) (cons (car x) (+ (cdr x) 1))) env))

(define (evaluate-thunk thunk env cont)
  (if (null? thunk)
    cont
    (evaluate-expr 
      (car thunk) 
      env 
      (if (pair? (cdr thunk)) ; at tail?
        `(drop ,(evaluate-thunk (cdr thunk) env cont))
        (evaluate-thunk (cdr thunk) env cont)))))

(define (evaluate-many exprs env cont)
  (if (null? exprs)
    cont
    (evaluate-expr (car exprs) env (evaluate-many (cdr exprs) (env-offset env) cont))))

(define (syntax-lambda params body cont)
  (let ((env (params->env params)))
    `(lambda 
          ,(length params) 
          ,(evaluate-thunk body env '())
          ,cont)))

(define (syntax-import module env cont)
  `(import ,module ,cont))

(define (syntax-if condition iftrue iffalse env cont)
  (evaluate-expr 
    condition 
    env 
    `(branch ,(evaluate-expr iftrue env cont) ,(evaluate-expr iffalse env cont))))

(define (syntax-quote x cont)
  (unless (symbol? x) (error "todo"))
  `(symbol ,x ,cont))

(define (repeat x n)
  (let loop ((n n))
    (if (<= n 0)
      '()
      (cons x (loop (- n 1))))))

(define (padleft s char n)
  (string-append 
    (list->string (repeat char (- n (length s)))) 
    s))

;; define expressions
(define (evaluate-expr expr env cont)
  (if (pair? expr)
    (if (or (equal? (car expr) #_quote) (equal? (car expr) 'quote))
      (syntax-quote (list-ref expr 1) cont) 
      (case (car expr)
        ('define (syntax-define (list-ref expr 1) (list-tail expr 2) env cont)) 
        ('if (syntax-if (list-ref expr 1) (list-ref expr 2) (list-ref expr 3) env cont))

        ('lambda (syntax-lambda (list-ref expr 1) (list-tail expr 2) cont))
        ('begin (evaluate-thunk (list-tail expr 1) env cont))

        ('+ (syntax-binary '+ (list-ref expr 1) (list-ref expr 2) env cont))
        ('- (syntax-binary '- (list-ref expr 1) (list-ref expr 2) env cont))
        ('* (syntax-binary '* (list-ref expr 1) (list-ref expr 2) env cont))
        ('/ (syntax-binary '/ (list-ref expr 1) (list-ref expr 2) env cont))
        ('equal? (syntax-binary 'equal? (list-ref expr 1) (list-ref expr 2) env cont))

        ('print (syntax-print (list-ref expr 1) env cont))
        ('import (syntax-import (list-ref expr 1) env cont))

        ((pair cons) (syntax-binary 'pair (list-ref expr 1) (list-ref expr 2) env cont))

        (else (evaluate-many (reverse expr) env `(call ,(length (cdr expr)) ,cont))))) 
          
    (immediate expr env cont)))

(define (params->env params)
  (let next-param ((params params) (res '()))
    (if (pair? params)
      (next-param (cdr params) (cons (cons (car params) (length res)) res))
      res)))

(define (trace . vals)
  (for-each 
    (lambda (x)
      (display x)
      (newline))
    vals))

(define (todo)
  (error "todo"))

(define (define->cdefine name)
  (string-append "d_" (symbol->cidentifier name)))

(define (env-variable-pointer env name cont)
  (let ((match (assoc name env)))
    (if match
      `(pick ,(cdr match) ,cont)
      `(fetch ,name ,cont))))

(define (immediate x env cont)
  (cond
    ((number? x) `(number ,x ,cont))
    ((symbol? x) (env-variable-pointer env x cont))
    (else (error "bad immediate"))))

(define (number->hex n)
  (list->string
    (reverse 
      (let loop ((n n))
        (cons 
          (list-ref '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F) (modulo n 16))
          (if (equal? (floor (/ n 16)) 0)
            '()
            (loop (floor (/ n 16)))))))))

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
                (if (> (length hexchar) 4)
                  (string-append "_U"(padleft hexchar #\0 8))
                  (string-append "_u"(padleft hexchar #\0 4))))))))

      (string->list (symbol->string sym))) 
    ""))
    

(define (lambda->label id)
  (string-append "f_" (number->string id)))

(define (symbol->cdef sym)
  (string-append "s_" (symbol->cidentifier sym)))


(define (op-to-c-call op)
  (case op
    ('+ "q_add(q);")
    ('- "q_sub(q);")
    ('* "q_mul(q);")
    ('/ "q_div(q);")
    ('equal? "q_is_equal(q);")
    ('pair "q_make_pair(q);")))

(define (to-c ir)
  (let next ((op ir) (code '()) (lambdas '()) (defines '()) (fetches '()) (imports '()) (symbols '()) (paramcount 0))
    (if (null? op) 
      (values 
        (cons 
          (cons 
            (string-append "q_pop_ret(q, "(number->string paramcount)", next);")  
            code) 
          lambdas) 
        defines 
        fetches      
        imports
        symbols)
        

      (case (car op)
        ('lambda 
         (let 
           ((paramcount2 (list-ref op 1))
            (thunk (list-ref op 2))
            (cont (list-ref op 3)))

           (call-with-values 
             (lambda () (next thunk '() lambdas defines fetches imports symbols paramcount2))

             (lambda (lambdas2 defines2 fetches2 imports2 symbols2)
               (next 
                 cont
                 (cons 
                   (string-append 
                     "q_make_lambda(q, &" 
                     (lambda->label (- (length lambdas2) 1))
                     ");") 
                   code) 
                 lambdas2 
                 defines2
                 fetches2
                 imports2
                 symbols2
                 paramcount)))))


        ('branch 
         (let 
           ((iftrue (list-ref op 1))
            (iffalse (list-ref op 2)))

           (call-with-values 
             (lambda () (next iftrue '() lambdas defines fetches imports symbols paramcount))

             (lambda (lambdas2 defines2 fetches2 imports2 symbols2)
               (call-with-values 
                 (lambda () (next iffalse '() lambdas2 defines2 fetches2 imports2 symbols2 paramcount))

                 (lambda (lambdas3 defines3 fetches3 imports3 symbols3)
                   (values
                     (cons (cons 
                             (string-append 
                               "Q_BRANCH(q, &" 
                               (lambda->label (- (length lambdas2) 1)) ; if true
                               ", &"
                               (lambda->label (- (length lambdas3) 1)) ; if false
                               ", next);") 
                             code) lambdas3) 
                     defines3
                     fetches3
                     imports3
                     symbols3)))))))

        ('call
         (let 
           ((argcount (list-ref op 1))
            (cont (list-ref op 2)))

           (if (null? cont) ;; at tail position?
              ;;#f
             (values
               (cons 
                 (cons 
                   (string-append "q_call_tail(q, " (number->string argcount) ", " (number->string paramcount) ", next);") 
                   code) 
                 lambdas)
               defines
               fetches
               imports
               symbols)
             
             (call-with-values 
               (lambda () (next cont '() lambdas defines fetches imports symbols paramcount))

               (lambda (lambdas2 defines2 fetches2 imports2 symbols2)
                 (values 
                   (cons 
                     (cons 
                       "q_call(q, next);" 
                       (cons
                         (string-append "q_push_ret(q, &" (lambda->label (- (length lambdas2) 1)) ");")
                         code))
                     lambdas2) 
                   defines2
                   fetches2
                   imports2
                   symbols2))))))

        ('import
         (let 
           ((module (list-ref op 1))
            (cont (list-ref op 2)))

           (call-with-values 
            (lambda () (next cont '() lambdas defines fetches imports symbols paramcount))

            (lambda (lambdas2 defines2 fetches2 imports2 symbols2)
              (values 
                (cons 
                  (cons 
                    (string-append "*next = &"(symbol->cidentifier module)"_toplevel;")
                    (cons
                      (string-append "q_push_ret(q, &" (lambda->label (- (length lambdas2) 1)) ");")
                      code))
                  lambdas2) 
                defines2
                fetches2
                (cons module imports2)
                symbols2)))))

        ((+ - * / equal? pair) ;; binary
         (next
           (list-ref op 1) ;; cont
           (cons (op-to-c-call (car op)) code)
           lambdas
           defines
           fetches
           imports
           symbols
           paramcount))

        ('define 
         (let 
           ((name (list-ref op 1))
            (cont (list-ref op 2)))

           (next 
             cont 
             (cons (string-append "Q_FETCH(q, 0, &" (define->cdefine name) ");") code)
             lambdas 
             (cons name defines)
             fetches
             imports
             symbols
             paramcount)))

        ('number
         (let
           ((x (list-ref op 1))
            (cont (list-ref op 2)))
           (next
             cont
             (cons 
               (string-append "Q_STORE(q, 0, Q_NUMBER(" (number->string (list-ref op 1)) "));") 
               (cons "Q_PUSH(q, 1);" code))
             lambdas
             defines
             fetches
             imports
             symbols
             paramcount)))

        ('print
          (let 
            ((cont (list-ref op 1)))
            (next
              cont
              (cons "q_print(q);" code)
              lambdas
              defines
              fetches
              imports
              symbols
              paramcount)))
             
        ('fetch
         (let 
            ((name (list-ref op 1))
             (cont (list-ref op 2)))
           (next
             cont
             (cons 
               (string-append "Q_STORE(q, 0, " (define->cdefine name) ");")
               (cons "Q_PUSH(q, 1);" code))
             lambdas
             defines
             (cons name fetches)
             imports
             symbols
             paramcount)))

        ('drop
         (let 
           ((cont (list-ref op 1)))
           (next
             cont
             (cons 
               "q_drop(q);"
               code)
             lambdas
             defines
             fetches
             imports
             symbols
             paramcount)))

        ('symbol
          (let 
            ((sym (list-ref op 1))
             (cont (list-ref op 2)))
            (next
              cont
              (cons 
                (string-append "Q_STORE(q, 0, Q_SYMBOL("(symbol->cdef sym)"));") 
                (cons 
                  "Q_PUSH(q, 1);" 
                  code))
              lambdas
              defines
              fetches
              imports
              (cons sym symbols)
              paramcount)))
              

        ('pick
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
                   (string-append "Q_FETCH(q, " (number->string n) ", &temp);") 
                   code)))
             lambdas
             defines
             fetches
             imports
             symbols
             paramcount)))

        (else (error "bad operation"))))))
       
(define (delimited-list ls del)
  (if (null? ls) 
    '()
    (if (null? (cdr ls)) 
      (cons (car ls) '())
      (cons (car ls) (cons del (delimited-list (cdr ls) del))))))

(define (string-join ls del)
  (apply string-append (delimited-list ls del)))

(define (dedupe e)
  (if (null? e) '()
      (cons (car e) (dedupe (filter (lambda (x) (not (equal? x (car e)))) 
                                    (cdr e))))))

(define (stitch-lambda module id l)
  (string-append 
    "static " (function-signature (lambda->label id))" {\n"
    "q_value temp;\n"
    (string-join (reverse l) "\n")
    "\n}"))

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
        (cons (string-append "q_value " (define->cdefine (car ls)) ";") (loop (cdr ls)))))
    "\n"))


(define (stitch-imports imports)
  (string-join
    (let loop ((ls imports))
       (if (null? ls)
        '()
        (cons (string-append (top-level-function-signature (symbol->cidentifier (car ls)))";") (loop (cdr ls)))))
    "\n"))

(define (stitch-extern-defines defines)
  (string-join
    (let loop ((ls defines))
       (if (null? ls)
        '()
        (cons (string-append "extern q_value " (define->cdefine (car ls)) ";") (loop (cdr ls)))))
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
  (function-signature (string-append module"_toplevel")))
  
(define (stitch-symbols symbols)
  (string-join
    (map 
      (lambda (sym)
        (string-append "extern const char *"(symbol->cdef sym)";"))
      symbols)
    "\n"))

(define (stitch-program module lambdas defines fetches imports symbols)
  (string-join 
    `(
      "#include \"qruntime.h\""
      ""
      "// defines"
      ,(stitch-defines (dedupe defines))
      ""
      "// extern"
      ,(stitch-extern-defines (difference (dedupe fetches) defines))
      ""
      "// imports"
      ,(stitch-imports (dedupe imports))
      ""
      "// symbols"
      ,(stitch-symbols (dedupe symbols))
      ""
      "// lambdas"
      ,(stitch-lambdas module lambdas)
      ""
      "// toplevel"
      ,(string-append (top-level-function-signature module) " {") 
      ,(string-append (lambda->label (- (length lambdas) 1))"(q, next);")
      "}")
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
  (let ((lst ()))
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
        (string-append "const char *"(symbol->cdef sym)" = \""(symbol->string sym)"\";"))
      symbols)
    "\n"))

(define (cli cmd module)
  (cond
    ((equal? cmd "--extract-symbols")
     (call-with-values 
      (lambda () (to-c (evaluate-thunk (read-all *stdin*) '() '())))
      (lambda (lambdas defines fetches imports symbols)
        (display (stitch-symbol-constants (dedupe symbols))))))

    ((equal? cmd "--build")
     (display (stitch-program module (to-c (evaluate-thunk (read-all *stdin*) '() '())))))))

(apply cli (list-tail (command-line) 2))
