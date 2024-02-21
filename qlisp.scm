(define
  source
  '(
    (define main 
      (lambda (a b c)
        (print (+ a b))))))

(define (syntax-define name expr)
  (append
    (evaluate-expr expr)
    `(define ,name 0)))

(define (evaluate-expr expr)
  (if (pair? expr)
    (case (car expr)
      ('define (make-define (list-ref expr 1) (list-ref expr 2)))
      ('lambda (make-lambda (list-ref expr 1) (list-tail expr 2)))
      ('+ (evaluate-+ (list-ref 1) (list-ref 2)))
      ('print (evaluate-print (list-ref expr 1)))
      ('fetch (evaluate-fetch (list-ref expr 1)))
      (error "nope"))
    (immediate expr)))

(define (syntax-lambda params body)
  `(lambda ,(length params) ,(apply append (map evaluate-expr body))))

(define (trace x)
  (display x)
  (newline)
  x)

(trace (syntax-lambda '(a b c) '(print (+ a b)))) 
     

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

(define (immediate x)
  (cond
    ((number? x) (string-append "Q_PUSH(&s, 1); Q_STORE(&s, 0, Q_NUMBER(" (number->string x) "));"))
    (else (error "nope"))))


(define (tail? body)
  (equal? (length body) 1))

(define (syntax-lambda params body)
  (string-join
   (let next-expr ((body body))
     (cond 
       ((null? body) "")
       ((tail-expr? body)
        () 
        (next-expr (cdr body)))
         
       (else 
         (evaluate-expr (car))
         (next-expr (cdr body)))))))

(define (compile-to-c source)

  (kkk))
                   
  
