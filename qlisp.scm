
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

;; define expressions
(define (evaluate-expr expr env cont)
    (if (pair? expr)
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
        (else (evaluate-many (reverse expr) env `(call ,(length (cdr expr)) ,cont)))) 
          
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
  (string-append "d_" (symbol->string name)))

(define (env-variable-pointer env name cont)
  (let ((match (assoc name env)))
    (if match
      `(pick ,(cdr match) ,cont)
      `(fetch ,name ,cont))))

(define (immediate x env cont)
  (cond
    ((number? x) `(number ,x ,cont))
    ((symbol? x) (env-variable-pointer env x cont))
    (else (error "nope"))))



(define (lambda->label id)
  (string-append "f_" (number->string id)))


(define (op-to-c-call op)
  (case op
    ('+ "q_add(s);")
    ('- "q_sub(s);")
    ('* "q_mul(s);")
    ('/ "q_div(s);")
    ('equal? "q_is_equal(s);")))

(define (to-c ir)
  (let next ((op ir) (code '()) (lambdas '()) (defines '()) (fetches '()) (imports '()) (paramcount 0))
    (if (null? op) 
      (values 
        (cons 
          (cons 
            (string-append "q_pop_ret(s, r, "(number->string paramcount)", next);")  
            code) 
          lambdas) 
        defines 
        fetches      
        imports)
        

      (case (car op)
        ('lambda 
         (let 
           ((paramcount2 (list-ref op 1))
            (thunk (list-ref op 2))
            (cont (list-ref op 3)))

           (call-with-values 
             (lambda () (next thunk '() lambdas defines fetches imports paramcount2))

             (lambda (lambdas2 defines2 fetches2 imports2)
               (next 
                 cont
                 (cons 
                   (string-append 
                     "q_make_lambda(s, &" 
                     (lambda->label (- (length lambdas2) 1))
                     ");") 
                   code) 
                 lambdas2 
                 defines2
                 fetches2
                 imports2
                 paramcount)))))


        ('branch 
         (let 
           ((iftrue (list-ref op 1))
            (iffalse (list-ref op 2)))

           (call-with-values 
             (lambda () (next iftrue '() lambdas defines fetches imports paramcount))

             (lambda (lambdas2 defines2 fetches2 imports2)
               (call-with-values 
                 (lambda () (next iffalse '() lambdas2 defines2 fetches2 imports2 paramcount))

                 (lambda (lambdas3 defines3 fetches3 imports3)
                   (values
                     (cons (cons 
                             (string-append 
                               "Q_BRANCH(s, &" 
                               (lambda->label (- (length lambdas2) 1)) ; if true
                               ", &"
                               (lambda->label (- (length lambdas3) 1)) ; if false
                               ", next);") 
                             code) lambdas3) 
                     defines3
                     fetches3
                     imports3)))))))

        ('call
         (let 
           ((argcount (list-ref op 1))
            (cont (list-ref op 2)))

           (if (null? cont) ;; at tail position?
              ;;#f
             (values
               (cons 
                 (cons 
                   (string-append "q_call_tail(s, " (number->string argcount) ", " (number->string paramcount) ", next);") 
                   code) 
                 lambdas)
               defines
               fetches
               imports)
             
             (call-with-values 
               (lambda () (next cont '() lambdas defines fetches imports paramcount))

               (lambda (lambdas2 defines2 fetches2 imports2)
                 (values 
                   (cons 
                     (cons 
                       "q_call(s, next);" 
                       (cons
                         (string-append "q_push_ret(r, &" (lambda->label (- (length lambdas2) 1)) ");")
                         code))
                     lambdas2) 
                   defines2
                   fetches2
                   imports2))))))


        ('import
         (let 
           ((module (list-ref op 1))
            (cont (list-ref op 2)))

           (call-with-values 
            (lambda () (next cont '() lambdas defines fetches imports paramcount))

            (lambda (lambdas2 defines2 fetches2 imports2)
              (values 
                (cons 
                  (cons 
                    (string-append "*next = &"(symbol->string module)"_toplevel;")
                    (cons
                      (string-append "q_push_ret(r, &" (lambda->label (- (length lambdas2) 1)) ");")
                      code))
                  lambdas2) 
                defines2
                fetches2
                (cons module imports2))))))

        ((+ - * / equal?)
         (next
           (list-ref op 1) ;; cont
           (cons (op-to-c-call (car op)) code)
           lambdas
           defines
           fetches
           imports
           paramcount))

        ('define 
         (let 
           ((name (list-ref op 1))
            (cont (list-ref op 2)))

           (next 
             cont 
             (cons (string-append "Q_FETCH(s, 0, &" (define->cdefine name) ");") code)
             lambdas 
             (cons name defines)
             fetches
             imports
             paramcount)))

        ('number
         (let
           ((x (list-ref op 1))
            (cont (list-ref op 2)))
           (next
             cont
             (cons 
               (string-append "Q_STORE(s, 0, Q_NUMBER(" (number->string (list-ref op 1)) "));") 
               (cons "Q_PUSH(s, 1);" code))
             lambdas
             defines
             fetches
             imports
             paramcount)))

        ('print
          (let 
            ((cont (list-ref op 1)))
            (next
              cont
              (cons "q_print(s);" code)
              lambdas
              defines
              fetches
              imports
              paramcount)))
             
        ('+
          (let 
            ((cont (list-ref op 1)))
            (next
              cont
              (cons "q_add(s);" code)
              lambdas
              defines
              fetches
              imports
              paramcount)))

        ('fetch
         (let 
            ((name (list-ref op 1))
             (cont (list-ref op 2)))
           (next
             cont
             (cons 
               (string-append "Q_STORE(s, 0, " (define->cdefine name) ");")
               (cons "Q_PUSH(s, 1);" code))
             lambdas
             defines
             (cons name fetches)
             imports
             paramcount)))

        ('drop
         (let 
           ((cont (list-ref op 1)))
           (next
             cont
             (cons 
               "Q_POP(s, 1);"
               code)
             lambdas
             defines
             fetches
             imports
             paramcount)))

        ('pick
         (let 
            ((n (list-ref op 1))
             (cont (list-ref op 2)))
           (next
             cont
             (cons 
               "Q_STORE(s, 0, temp);"
               (cons
                 "Q_PUSH(s, 1);" 
                 (cons 
                   (string-append "Q_FETCH(s, " (number->string n) ", &temp);") 
                   code)))
             lambdas
             defines
             fetches
             imports
             paramcount)))

        (else (error "nuh-uh"))))))
       
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
    "static void " (lambda->label id)"(q_stack *s, q_rets *r, void **next) {\n"
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
        (cons (string-append "void "(symbol->string (car ls))"_toplevel(q_stack* s, q_rets *r, void** next);") (loop (cdr ls)))))
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

  
(define (stitch-program module lambdas defines fetches imports)
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
      "// lambdas"
      ,(stitch-lambdas module lambdas)
      ""
      "// toplevel"
      ,(string-append "void " module"_toplevel(q_stack *s, q_rets *r, void** next) {") 
      ,(string-append (lambda->label (- (length lambdas) 1))"(s, r, next);")
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
    
(display (stitch-program (list-ref (command-line) 2) (to-c (evaluate-thunk (read-all *stdin*) '() '()))))
