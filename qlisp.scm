#lang racket
(require struct-update)

(define (todo)
  (error "todo"))

(define (trace . vals)
  (for-each 
   (lambda (x)
     (display x)
     (newline))
   vals))

(struct context (env free-binds ops) #:transparent)
(define-struct-updaters context)

(struct environment (binds parent) #:transparent)
(define-struct-updaters environment)

;;(define (append-ir ctx ir) '())
(define (push-ir ctx op) (context-ops-set ctx (append op (list (context-ops ctx)))))

(define (syntax-binary op a b ctx)
  (evaluate-expr a (evaluate-expr b (push-ir ctx `(op)))))

(define (syntax-unary op expr ctx)
  (evaluate-expr expr (push-ir ctx `(,op))))

(define (syntax-nullary op ctx)
  (push-ir ctx `(,op)))

(define (evaluate-many exprs ctx)
  (foldr (lambda (expr ctx) (evaluate-expr expr ctx)) ctx exprs))

(define (syntax-quote x cont)
  `(quote ,x ,cont))

(define (call-op argc) `(call ,argc))

(define (evaluate-call callee args ctx)
  (evaluate-many
   (append args `(,callee)) ;; arguments
   (push-ir ctx  (call-op (length args))) ;; call-op
   ))


(define (fetch-free-vars free-binds ctx)
  (foldr (lambda (free-var ctx) (fetch-name free-var ctx)) ctx free-binds)
  )


(define (params->env params parent)
  (environment (let next-param ((params params) (res '()))
    (if (pair? params)
      (next-param (cdr params) (cons (cons (car params) (length res)) res))
      res)) parent))

(define (evaluate-lambda params body ctx)
  (let* ((env (context-env ctx))
         (param-count (length params))
         (body-env (params->env params env))
         (body-ctx (evaluate-thunk body (context body-env '() '()))))

    (fetch-free-vars (context-free-binds body-ctx)
                     (push-ir
                      (context env (context-free-binds ctx) (context-ops body-ctx))
                      `(lambda ,param-count)))))

(define (evaluate-expr expr ctx)
  (if (pair? expr)
      (if (or #f (equal? (car expr) 'quote))
          ;; quotes
          (todo) 

          (let ((f (first expr)))
            (case f
              ;; built-in calls
              ((+ - * / >= <= > < equal? and or not)
               (syntax-binary (second expr) (third expr) expr ctx))

              ((null? pair? procedure? boolean? number? symbol? display car cdr cons)
               (syntax-unary f (second expr 1) ctx))
        
              ((newline) (syntax-nullary f ctx))

              ('lambda (evaluate-lambda (second expr) (list-tail expr 3) ctx))

              ;; calls
              (else (evaluate-call (first expr) (rest expr) ctx)))))

      ;; immediates/variables
      (leaf-expr expr ctx)))

(define (get-bound name env)
  (cond
    ((eq? env #f) #f)
    ((assoc name (environment-binds env)) => (lambda (n) `(,env ,n)))
    (else (get-bound name (environment-parent env)))))

(define (local-bind? bind env) (eq? (first bind) env) )  

(define (insert-free bind ctx)
  (context-free-binds-update ctx (lambda (frees) (cons (first (second bind)) frees))))

(define (env-name name env ctx)
  (cond
    ((get-bound name env) =>
                          (lambda (bind) (if (local-bind? bind env)
                                             (push-ir ctx `(pick ,(second (second bind))))
                                             (push-ir (insert-free bind ctx) `(up ,(length (context-free-binds ctx)))))))
    (else (push-ir ctx `(fetch ,name)))))

(define (fetch-name x ctx)
  (env-name x (context-env ctx) ctx))

(define (leaf-expr x ctx)
  (cond
    ((number? x) 
     (push-ir ctx `(number ,x)))
    ((boolean? x) 
     (push-ir ctx `(boolean ,x)))
    ((symbol? x) 
     (fetch-name x ctx))
    (else 
     (error "bad expression " x))))

  

(define mock-ctx (context (environment '((a 0)) (environment '((b 0) (c 1)) #f)) '() '()


                          ))
