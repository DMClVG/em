
(define
  source
  '(
    (define main 
      (lambda (a b c)
        (print (+ a b))))))

(define (syntax-define name expr env)
  (append
    (evaluate-expr expr env)
    `((define ,name))))

(define (syntax-print x env)
  (append
    (evaluate-expr x env)
    `((print))))

;;(define (evaluate-thunk thunk env))

(define (evaluate-expr expr env)
  (if (pair? expr)
    (case (car expr)
      ('define (syntax-define (list-ref expr 1) (list-ref expr 2) env ())) 
      ('lambda (syntax-lambda (list-ref expr 1) (list-tail expr 2)))
      ('+ (syntax-+ (list-ref expr 1) (list-ref expr 2) env))
      ('print (syntax-print (list-ref expr 1) env))
      (else (apply append (map (evaluate-expr-with-env env) (reverse expr)))))
    (immediate expr env)))


(define (call? expr)
  (case (car expr)
    ('define #f)
    ('lambda #f)
    ('+ #f)
    ('print #f)
    (else #t)))

(define (evaluate-expr-with-env env)
  (lambda (expr) (evaluate-expr expr env)))

(define (params->env params)
  (let next-param ((params params) (res '()))
    (if (pair? params)
      (next-param (cdr params) (cons (cons (car params) (length res)) res))
      res)))

(define (evaluate-thunk ))

(define (syntax-lambda params body)
  (let ((env (params->env params)))
    `((lambda 
        ,(length params) 
        ,(let loop ((exprs body))
           (if (call? (car exprs))
             (append
                (evaluate-expr (car exprs) env)
               `((call ,(- (length (car exprs)) 1) ,(evaluate-expr ()))))
             (evaluate-expr (car exprs) env)))))))

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

(define (syntax-+ a b env)
  (append
    (evaluate-expr b env)
    (evaluate-expr a env)
    '((+))))

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

(define (env-variable-pointer env name)
  (let ((match (assoc name env)))
    (if match
      `(arg ,(cdr match))
      `(fetch ,name))))

(define (immediate x env)
  (cond
    ((number? x) `((number ,x)))
    ((symbol? x) `(,(env-variable-pointer env x)))
    (else (error "nope"))))


;;(define (tail? body)
;;  (equal? (length body) 1))

(define (lambda->label id)
  (string-append "f_" (number->string id)))

(define (to-c ir)
  (let next ((ir ir) (code '()) (lambdas '()) (defines '()) (fetches '()))
    (if (null? ir) 
      (values (cons code lambdas) defines fetches)      

      (let ((op (car ir)))
        (case (car op)
          ('lambda 
           (call-with-values 
             (lambda () (to-c (list-ref op 2)))
             (lambda (lambdas2 defines2 fetches2)
               (next 
                 (cdr ir) 
                 (cons 
                   (string-append 
                     "q_lambda(&s, &&" 
                     (lambda->label (- (+ (length lambdas) (length lambdas2)) 1))
                     ");") 
                   code) 
                 (append lambdas lambdas2) 
                 (append defines defines2)
                 (append fetches fetches2)))))

          ('call
           (call-with-values 
             (lambda () (to-c (list-ref op 1)))
             (lambda (lambdas2 defines2 fetches2)
               (next 
                 (cdr ir) 
                 (cons 
                   (string-append 
                     "q_call(&s, &r, &&" 
                     (lambda->label (- (+ (length lambdas) (length lambdas2)) 1))
                     ");") 
                   code) 
                 (append lambdas lambdas2) 
                 (append defines defines2)
                 (append fetches fetches2)))))

           ;;(next
           ;; code
           ;; (cons (to-c (list-ref op 2))))
             
          ('arg 
           (next 
             (cdr ir)
             (cons (string-append "Q_ARG(&a, &s, " (number->string (list-ref op 1)) ");") code)
             lambdas 
             defines
             fetches))

          ('+ 
           (next 
             (cdr ir)
             (cons "q_add(&s);" code)
             lambdas 
             defines
             fetches))

          ('print 
           (next 
             (cdr ir)
             (cons "q_print(&s);" code)
             lambdas 
             defines
             fetches))

          ('define 
           (let ((name (list-ref op 1)))
             (next 
               (cdr ir)
               (cons "Q_POP(&s, 1);" (cons (string-append "Q_FETCH(&s, &" (define->cdefine name) ");") code))
               lambdas 
               (cons name defines)
               fetches)))

          ('number
           (next
             (cdr ir)
             (cons 
               (string-append "Q_STORE(&s, 0, Q_NUMBER(" (number->string (list-ref op 1)) "));") 
               (cons "Q_PUSH(&s, 1);" code))
             lambdas
             defines
             fetches))


          ('fetch
           (let ((name (list-ref op 1)))
             (next
               (cdr ir)
               (cons 
                 (string-append "Q_STORE(&s, 0, " (define->cdefine name) ");")
                 (cons "Q_PUSH(&s, 1);" code))
               lambdas
               defines
               (cons name fetches))))

          (else (error "nuh-uh")))))))
       
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
  (trace (syntax-lambda '(a b c) '((print (+ (+ a c) b))))) 
  ;(trace (stitch-program 
        ;   (to-c (syntax-define 'f '((+ 1 2) 3 2) '())))) 
  (trace  
           (syntax-define 'f '((+ 1 2) 3 2) '()))) 
