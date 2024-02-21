
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
    (evaluate-expr a env `(+ ,cont))))

(define (evaluate-thunk thunk env cont)
  (if (null? thunk)
    cont
    (evaluate-expr (car thunk) env (evaluate-thunk (cdr thunk) env cont))))

(define (syntax-lambda params body cont)
  (let ((env (params->env params)))
    `(lambda 
          ,(length params) 
          ,(evaluate-thunk body env '())
          ,cont)))

;;(define (evaluate-thunk thunk env))

(define (evaluate- args env cont)
  `())

(define (evaluate-expr expr env cont)
    (if (pair? expr)
      (case (car expr)
        ('define (syntax-define (list-ref expr 1) (list-ref expr 2) env cont)) 
        ('lambda (syntax-lambda (list-ref expr 1) (list-tail expr 2) cont))
        ('+ (syntax-+ (list-ref expr 1) (list-ref expr 2) env cont))
        ('print (syntax-print (list-ref expr 1) env cont))
        (else (evaluate-thunk (reverse expr) env `(call ,(length (cdr expr)) ,cont)))) 
          
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

(define
  ir
  '(

    (lambda 3
         1
         (+ 1 2) 
         (print 0)
         (print 1)
         (fetch main)
         (print 2))

    (define main 0))) 

(define definitions '())
(define lambdas '())

(define (todo)
  (error "todo"))

(define (evaluate-print x)
  (string-append "q_print(&s, " (number->string x) ");"))

(define (evaluate-+ a b)
  (string-append "q_add(&s, " (number->string a) , (number->string b) ");"))

(define (define->cdefine name)
  (string-append "d_" (symbol->string name)))

(define (make-lambda paramcount body)
  (declare-lambda)
  (string-append "q_make_lambda(&s, &&" (symbol->string label) ");"))

(define (declare-definition definitions name)
  (append (define->cdefine name) definitions))

(define (make-define name x)
  (declare-definition name)
  (string-append "Q_FETCH(&s, " (number->string x) ", &" (define->cdefine name) "); Q_POP(&s, 1);"))

(define (env-variable-pointer env name cont)
  (let ((match (assoc name env)))
    (if match
      `(arg ,(cdr match) ,cont)
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
      (values (cons code lambdas) defines fetches)      

      (case (car op)
        ('lambda 
         (let 
           ((paramcount (list-ref op 1))
            (thunk (list-ref op 2))
            (cont (list-ref op 3)))

           (call-with-values 
             (lambda () (to-c thunk))

             (lambda (lambdas2 defines2 fetches2)
               (next 
                 cont
                 (cons 
                   (string-append 
                     "q_lambda(&s, &&" 
                     (lambda->label (+ (length lambdas) (length lambdas2)))
                     ");") 
                   code) 
                 (append lambdas lambdas2) 
                 (append defines defines2)
                 (append fetches fetches2))))))

        ('call
         (let 
           ((argcount (list-ref op 1))
            (cont (list-ref op 2)))

           (call-with-values 
             (lambda () (to-c cont))

             (lambda (lambdas2 defines2 fetches2)
               (next 
                '()
                 (cons 
                   (string-append 
                     "q_call(&s, &r, &&" 
                     (lambda->label (- (length lambdas2) 1))
                     ");") 
                   code) 
                 (append lambdas lambdas2) 
                 (append defines defines2)
                 (append fetches fetches2))))))

        ('define 
         (let 
           ((name (list-ref op 1))
            (cont (list-ref op 2)))

           (next 
             cont 
             (cons "Q_POP(&s, 1);" (cons (string-append "Q_FETCH(&s, 0, &" (define->cdefine name) ");") code))
             lambdas 
             (cons name defines)
             fetches)))

        ('arg 
         (let 
           ((n (list-ref op 1))
            (cont (list-ref op 2)))
           (next 
             cont 
             (cons (string-append "Q_ARG(&a, &s, " (number->string n) ");") code)
             lambdas 
             defines
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
    "\nQ_RET(&r, &next);\ngoto *next; \n}"))

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
  (trace lambdas)
  (trace defines)
  (trace fetches)
  (string-join 
    `(
      "#include <qruntime.h>"
      ""
      "// defines"
      ,(stitch-defines (dedupe defines))
      ""
      "// extern"
      ,(stitch-extern-defines (difference (dedupe fetches) defines))
      ""
      "int main() {" 
      "q_stack s;"
      "q_stack r;"
      "q_stack a;"
      "void* next;"
      "next = NULL;"
      ,(stitch-lambdas lambdas)
      "return 0;"
      "\n}")
    "\n"))

(begin 
  "tests"
  (trace 
    (syntax-lambda 
      '(a b c) 
      '((print (+ (+ a c) b)) 
        (print 0))
      '())

    (evaluate-thunk '((1) (+ 1 2)) '() '())

    (stitch-program (to-c (evaluate-thunk '(
                                            (define sum 
                                              (lambda (a b c) 
                                                (lambda (a b c) 
                                                  (print (+ (+ b b) c))))) 

                                            (sum 1 2 3)
                                            (sum 22 33 11)) '() '())))))

  ;(trace (stitch-program 
      ;   (to-c (syntax-define 'f '((+ 1 2) 3 2) '())))) 
  ;;(trace  
   ;;        (syntax-define 'f '((+ 1 2) 3 2) '())))) 
