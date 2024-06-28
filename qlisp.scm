#lang racket

(define (todo)
  (error "todo"))

(define (trace . vals)
  (for-each 
   (lambda (x)
     (display x)
     (newline))
   vals))

(struct context (env frees))

(define (append-ir ctx ir) '())

(define (syntax-binary op a b ctx)
  (append-ir (evaluate-expr b (evaluate-expr a ctx)) `(op)))

(define (syntax-unary op expr ctx)
  (append-ir (evaluate-expr expr ctx) `(,op)))

(define (syntax-nullary op ctx)
  (append-ir ctx `(,op)))


(define (syntax-define head expr ctx)
  (cond 
    ((pair? head) 
     (syntax-define (first head) (list (append `(lambda ,(rest head)) expr)) ctx)) ;; lambda
    ((symbol? head) 
     (append-ir (evaluate-expr (first expr) ctx) `(define ,head))))) ;; normal


(define (evaluate-thunk thunk ctx)
  (if (null? thunk)
      ctx
      (evaluate-expr 
       (car thunk) 
       (if (pair? (cdr thunk)) ; at tail?
           `(drop ,(evaluate-thunk (cdr thunk) ctx))
           (evaluate-thunk (cdr thunk) ctx)))))

(define (evaluate-many exprs ctx)
  (foldl (lambda (ctx expr) (evaluate-expr expr ctx)) ctx) exprs)

(define (make-closure ctx body-ctx)
  ((append-ir (evaluate-many (context-frees body-ctx) ctx) `(closure))))

(define (make-lambda-body-context parent-env params)
  (context '() '()))

(define (syntax-lambda params body ctx)
  (let* [(body-ctx (evaluate-thunk body (make-lambda-body-context ('env ctx) params)))]
    (make-closure body-ctx)))

(define (syntax-let bindings body ctx)
  (evaluate-many 
   (map cadr bindings)  
   `(push-frame
     ,(evaluate-thunk 
       body 
       (append-names-to-env env (map car bindings)) 
       `(pop-frame ,(length bindings) ,cont)))))

(define (syntax-import module ctx)
  `(import ,module ,cont))

(define (syntax-if condition iftrue iffalse env cont)
  (evaluate-expr 
   condition 
   env 
   `(branch ,(evaluate-expr iftrue env cont) ,(evaluate-expr iffalse env cont))))

(define (syntax-quote x cont)
  `(quote ,x ,cont))

(define (evaluate-call callee args ctx)
  (evaluate-many (append args (list callee)) ctx `(call ,(length (cdr expr)) ,cont)))

(define (evaluate-expr expr ctx)
  (if (pair? expr)
      (if (or #f (equal? (car expr) 'quote))
          ;; quotes
          (syntax-quote (list-ref expr 1) cont) 

          (let ((f (first expr)))
            (case f
              ;; special forms
              ('define (syntax-define (list-ref expr 1) (list-tail expr 2) ctx)) 
              ('if (syntax-if (list-ref expr 1) (list-ref expr 2) (list-ref expr 3) ctx))
              ('lambda (syntax-lambda (list-ref expr 1) (list-tail expr 2) ctx))
              ('begin (evaluate-thunk (list-tail expr 1) ctx))
              ('import (syntax-import (list-ref expr 1) ctx))
              ('let (syntax-let (list-ref expr 1) (list-tail expr 2) ctx))

              ;; built-in calls
              ((+ - * / >= <= > < equal? and or not)
               (syntax-binary (second expr) (third expr) expr ctx))

              ((null? pair? procedure? boolean? number? symbol? display car cdr cons)
               (syntax-unary f (list-ref expr 1) ctx))
        
              ((newline) (syntax-nullary f ctx))

              ;; calls
              (else (evaluate-call (first expr) (rest expr) ctx)))) 

          ;; immediates/variables
          (leaf-expr expr ctx))))
  
(define (leaf-expr x ctx)
  (cond
    ((number? x) 
     (ir-append ctx `(number ,x)))
    ((boolean? x) 
     (ir-append ctx `(boolean ,x)))
    ((symbol? x) 
     (env-name x ctx))
    (else 
     (error "bad expression"))))

  


