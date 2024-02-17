(define 
 code 
 '(
   (define (print-7)
     (print 7))

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

(define (store-environment))

(define* (chain :rest functions)
  ())

(define (make-symbol name)
  (chain
    (())))


(define (environment-load)
  (todo))

(define (environment-store)
  (chain 
    (environment-write-symbol)
    (environment-write-value)))

(define (syntax-define-value name expr)
  (chain 
    (evaluate-expression expr)
    (make-symbol name)
    (store-in-environment)))

(define (syntax-lambda params body)
  ())

(define (evaluate-expression expr)
  ())

(define (compile-scheme code)
  (let ()
    ()))


(compile-scheme code)
