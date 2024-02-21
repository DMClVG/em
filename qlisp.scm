
(define
  source
  '(
    (define main 
      (lambda (a b c)
        (print (+ a b))))))

(define (syntax-define name expr env cont)
  (evaluate-expr expr env `(define ,name ,cont)))

(define (syntax-print expr env cont)
  (evaluate-expr expr env `(print ,cont)))

(define (syntax-+ a b env cont)
  (evaluate-expr b env
    (evaluate-expr a (env-offset env) `(+ ,cont))))

(define (env-offset env)
  (map (lambda (x) (cons (car x) (+ (cdr x) 1))) env))

(define (evaluate-thunk thunk env cont)
  (if (null? thunk)
    cont
    (evaluate-expr 
      (car thunk) 
      env 
      (if (pair? (cdr thunk)) 
        `(drop ,(evaluate-thunk (cdr thunk) env cont))
        (evaluate-thunk (cdr thunk) env cont)))))

(define (evaluate-many exprs env cont)
  (if (null? exprs)
    cont
    (evaluate-expr (car exprs) env (evaluate-many (cdr exprs) env cont))))

(define (syntax-lambda params body cont)
  (let ((env (params->env params)))
    `(lambda 
          ,(length params) 
          ,(evaluate-thunk body env '())
          ,cont)))

(define (evaluate-expr expr env cont)
    (if (pair? expr)
      (case (car expr)
        ('define (syntax-define (list-ref expr 1) (list-ref expr 2) env cont)) 
        ('lambda (syntax-lambda (list-ref expr 1) (list-tail expr 2) cont))
        ('+ (syntax-+ (list-ref expr 1) (list-ref expr 2) env cont))
        ('print (syntax-print (list-ref expr 1) env cont))
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


;;(define (tail? body)
;;  (equal? (length body) 1))

(define (lambda->label id)
  (string-append "f_" (number->string id)))

(define (to-c ir)
  (let next ((op ir) (code '()) (lambdas '()) (defines '()) (fetches '()))
    (if (null? op) 
      (values 
        (cons (cons 
                "q_pop_ret(&r, &next);"  
                code) lambdas) 
        defines 
        fetches)      

      (case (car op)
        ('lambda 
         (let 
           ((paramcount (list-ref op 1))
            (thunk (list-ref op 2))
            (cont (list-ref op 3)))

           (call-with-values 
             (lambda () (next thunk '() lambdas defines fetches))

             (lambda (lambdas2 defines2 fetches2)
               (next 
                 cont
                 (cons 
                   (string-append 
                     "q_make_lambda(&s, &&" 
                     (lambda->label (- (length lambdas2) 1))
                     ");") 
                   code) 
                 lambdas2 
                 defines2
                 fetches2)))))

        ('call
         (let 
           ((argcount (list-ref op 1))
            (cont (list-ref op 2)))

           (call-with-values 
             (lambda () (next cont '() lambdas defines fetches))

             (lambda (lambdas2 defines2 fetches2)
               (values 
                 (cons 
                   (cons 
                     "q_call(&s, &next);" 
                     (cons
                       (string-append "q_push_ret(&r, &&" (lambda->label (- (length lambdas2) 1)) ");")
                       code))
                   lambdas2) 
                 defines2
                 fetches2)))))

        ('define 
         (let 
           ((name (list-ref op 1))
            (cont (list-ref op 2)))

           (next 
             cont 
             (cons (string-append "Q_FETCH(&s, 0, &" (define->cdefine name) ");") code)
             lambdas 
             (cons name defines)
             fetches)))

        ('number
         (let
           ((x (list-ref op 1))
            (cont (list-ref op 2)))
           (next
             cont
             (cons 
               (string-append "Q_STORE(&s, 0, Q_NUMBER(" (number->string (list-ref op 1)) "));") 
               (cons "Q_PUSH(&s, 1);" code))
             lambdas
             defines
             fetches)))

        ('print
          (let 
            ((cont (list-ref op 1)))
            (next
              cont
              (cons "q_print(&s);" code)
              lambdas
              defines
              fetches)))
             
        ('+
          (let 
            ((cont (list-ref op 1)))
            (next
              cont
              (cons "q_add(&s);" code)
              lambdas
              defines
              fetches)))

        ('fetch
         (let 
            ((name (list-ref op 1))
             (cont (list-ref op 2)))
           (next
             cont
             (cons 
               (string-append "Q_STORE(&s, 0, " (define->cdefine name) ");")
               (cons "Q_PUSH(&s, 1);" code))
             lambdas
             defines
             (cons name fetches))))

        ('drop
         (let 
           ((cont (list-ref op 1)))
           (next
             cont
             (cons 
               "Q_POP(&s, 1);"
               code)
             lambdas
             defines
             fetches)))

        ('pick
         (let 
            ((n (list-ref op 1))
             (cont (list-ref op 2)))
           (next
             cont
             (cons 
               "Q_STORE(&s, 0, temp);"
               (cons
                 "Q_PUSH(&s, 1);" 
                 (cons 
                   (string-append "Q_FETCH(&s, " (number->string n) ", &temp);") 
                   code)))
             lambdas
             defines
             fetches)))

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

(define (stitch-lambda id l)
  (string-append 
    (lambda->label id) ": {\n"
    (string-join (reverse l) "\n")
    "\ngoto *next;\n}"))

(define (stitch-lambdas lambdas)
  (let loop ((lambdas (reverse lambdas)) (id 0))
     (if (null? lambdas)
      ""
      (begin
        (string-append
          (stitch-lambda id (car lambdas))
          "\n"
          (loop (cdr lambdas) (+ 1 id)))))))

(define (stitch-defines defines)
  (string-join
    (let loop ((ls defines))
       (if (null? ls)
        '()
        (cons (string-append "q_value " (define->cdefine (car ls)) ";") (loop (cdr ls)))))
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
  
(define (stitch-program lambdas defines fetches)
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
      "int main() {" 
      "q_stack s;"
      "q_rets r;"
      "void* next;"
      "q_value temp;"
      "q_init_rets(&r);"
      "q_init_stack(&s);"
      "q_push_ret(&r, &&end);"
      "next = NULL;"
      ,(string-append "goto " (lambda->label (- (length lambdas) 1)) ";")
      ,(stitch-lambdas lambdas)
      "end:"
      "return 0;"
      "\n}")
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
    
(display (stitch-program (to-c (evaluate-thunk (read-all *stdin*) '() '()))))

;;(display 
;;  (stitch-program (to-c (evaluate-thunk '(
;;                                          (define sum 
;;                                             (lambda () 
;;                                               (lambda (a b c) 
;;                                                 (print (+ (+ b a) c))))) 
;;
;;                                          ((sum) 1 2 3)
;;                                          ((sum ) 22 33 44)) 
;;                                        '() '()))))
  ;(trace (stitch-program 
     ;   (to-c (syntax-define 'f '((+ 1 2) 3 2) '())))) 
  ;;(trace  
   ;;        (syntax-define 'f '((+ 1 2) 3 2) '())))) 
