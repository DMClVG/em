(define 
 code 
 '(
   (define (factorial n)
     (if (equal? n 0) 
       1
       (* n (factorial (- n 1)))))

   (factorial 4)))


    
(define (todo)
  (error "todo"))

(define (assert-not-nil x)
  (when (nil? x) (error "assertion failed")))

(define (syntax-define-function name params body)
  (syntax-define-value name (syntax-lambda params body)))

(define* (chain state :rest ops)
  (let* loop 
    ((state state) 
     (top (length (get-program-code state))) 
     (ops ops)) 

    (if (nil? ops)
     (values state top)
     (loop (cons (+ 1 top) (car ops)) (+ 1 top) (cdr ops)))))

;; DEFINITIONS

(define (allocate-definition-slot state name)
  (let (defs (get-definitions state))
    (cons `(,name ,(length defs)) defs)))

(define (get-definition-slot state name)
  (cdr (assoc name (get-definitions state))))

(define (store-in-definition-slot slot)
  (values
    `(() store! (v t) (,(+ (* slot 2) 1) v t) (d p) (d p))
    `(() store! (t) (,(+ (* slot 2) 0) t) (d p) (d p))))

(define (syntax-define-value state name expr)
  (let* ((state (allocate-definition-slot state name)))
    (list 
      state 
      (evaluate-expression state expr)
      (store-in-definition-slot (get-definition-slot state name)))))

;; LAMBDAS

(define (make-lambda state tag)
  `((,tag) lambda () () () ()))

(define (evaluate-lambda-body params)
     ()) 

(define (syntax-lambda state params body)
  (make-lambda (evaluate-lambda-body state params body)))

;; EXPRESSION

(define (evaluate-expression state env expr)
  (case (car expr)
    ('lambda (syntax-lambda state (list-ref expr 1) (list-tail expr 2)))

    ('if (syntax-if (list-ref expr 1) (list-ref expr 2) (list-ref expr 3)))

    ('* (syntax-binary '* (cdr expr)))
    ('- (syntax-minus - (cdr expr)))

    ('equal? (syntax-equal? (cdr expr)))

    (else 
      ())))

;; MAIN

(define (compile-scheme code)
  (let ()
    ()))

(define (trace x)
  (display x)
  (newline)
  x)

(trace (store-in-definition-slot '() 0))
(compile-scheme code)
