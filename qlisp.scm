#lang racket
(require struct-update)
(require "backend.scm")

(define (todo)
  (error "todo"))

(define (trace . vals)
  (for-each 
   (lambda (x)
     (display x)
     (newline))
   vals)
  (first vals))

(struct context (env free-binds ops) #:transparent)
(define-struct-updaters context)

(struct environment (binds parent) #:transparent)
(define-struct-updaters environment)

;;(define (append-ir ctx ir) '())
(define (push-ir ctx op) (context-ops-set ctx (append op (list (context-ops ctx)))))

(define (syntax-binary op a b ctx)
  (evaluate-expr a (pop-env-offset 1 (evaluate-expr b (push-env-offset (push-ir ctx `(,op)))))))

(define (syntax-unary op expr ctx)
  (evaluate-expr expr (push-ir ctx `(,op))))

(define (syntax-nullary op ctx)
  (push-ir ctx `(,op)))

(define (env-offset env n)
  (environment (map (lambda (bind) (list (first bind) (+ n (second bind)))) (environment-binds env)) (environment-parent env)))

(define (push-env-offset ctx)
  (context-env-update ctx (lambda (env) (env-offset env 1))))

(define (pop-env-offset n ctx)
  (context-env-update ctx (lambda (env) (env-offset env (- n)))))

(define (evaluate-many exprs ctx)
  (let loop ((exprs exprs) (ctx ctx))
    (if (null? exprs) ctx
        (evaluate-expr (first exprs) (pop-env-offset 1 (loop (rest exprs) (push-env-offset ctx)))))))

(define (call-op argc) `(call ,argc))

(define (evaluate-call callee args ctx)
  (evaluate-many
   (append args `(,callee)) ;; arguments
   (push-ir ctx  (call-op (length args))) ;; call-op
   ))


;;(define (fetch-free-vars free-binds ctx)
;;  (foldr (lambda (free-var ctx) (fetch-name free-var ctx)) ctx free-binds)
;;  )

(define (fetch-free-vars free-binds ctx)
  (let loop ((binds free-binds) (ctx ctx))
    (if (null? binds) ctx
        (fetch-name (first binds) (pop-env-offset 1 (loop (rest binds) (push-env-offset ctx)))))))


(define (evaluate-thunk thunk ctx)
  (if (null? thunk)
      ctx
      (evaluate-expr 
       (first thunk)  
       (if (pair? (rest thunk)) ; not at tail?
           (push-ir (evaluate-thunk (rest thunk) ctx) `(drop))
           (evaluate-thunk (cdr thunk) ctx)))))

(define (params->env params parent)
  (environment (let next-param ((params params) (res '()))
                 (if (pair? params)
                     (next-param (cdr params) (cons (list (car params) (length res)) res))
                     res)) parent))


;; (define (syntax-let bindings body env cont)
;;   (evaluate-many 
;;     (map cadr bindings) 
;;     env 
;;     `(push-frame
;;        ,(evaluate-thunk 
;;           body 
;;           (append-names-to-env env (map car bindings)) 
;;           `(pop-frame ,(length bindings) ,cont)))))

(define (syntax-import module ctx)
  (push-ir ctx `(import ,module)))

(define (syntax-if condition iftrue iffalse ctx)
  (evaluate-expr condition (context-ops-set ctx `(branch ,(context-ops (evaluate-expr iftrue ctx)) ,(context-ops (evaluate-expr iffalse ctx))))))

(define (syntax-quote x ctx)
  (push-ir ctx `(quote ,x)))

(define (syntax-lambda params body ctx)
  (let* ((env (context-env ctx))
         (param-count (length params))
         (body-env (params->env (reverse params) env))
         (body-ctx (evaluate-thunk body (context body-env '() '())))
         (free-count (length (context-free-binds body-ctx))))

    (fetch-free-vars (reverse (context-free-binds body-ctx))
                     (push-ir
                      (context env (context-free-binds ctx) (context-ops ctx))
                      `(closure ,param-count ,free-count ,(context-ops body-ctx))))))

(define (do-define name value ctx)
  (evaluate-expr value (push-ir ctx `(define ,name))))

(define (syntax-define name expr ctx)
  (if (list? name)
      (do-define (first name) `(lambda ,(rest name) ,@expr) ctx)
      (do-define name (first expr) ctx)))

(define (evaluate-expr expr ctx)
  (if (pair? expr)
      (if (or #f (equal? (first expr) 'quote))
          ;; quotes
          (syntax-quote (second expr) ctx) 

          (let ((f (first expr)))
            (case f
              ;; built-in calls
              ((+ - * / >= <= > < equal? and or not cons)
               (syntax-binary f (second expr) (third expr) ctx))

              ((null? pair? procedure? boolean? number? symbol? display car cdr)
               (syntax-unary f (second expr) ctx))
        
              ((newline) (syntax-nullary f ctx))

              ;; special forms
              ((define) (syntax-define (second expr) (list-tail expr 2) ctx))
              ((if) (syntax-if (second expr) (third expr) (fourth expr) ctx))
              ((lambda) (syntax-lambda (second expr) (list-tail expr 2) ctx))
              ((begin) (evaluate-thunk (rest expr) ctx))
              ((import) (syntax-import (second expr) ctx))

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
  (context-free-binds-update ctx (compose dedupe (lambda (frees) (append frees (list (first (second bind))))))))

(define (env-name name env ctx)
  (cond
    ((get-bound name env) =>
                          (lambda (bind) (if (local-bind? bind env)
                                             (push-ir ctx `(pick ,(second (second bind))))
                                             (let ((new-ctx (insert-free bind ctx))) (push-ir new-ctx `(up ,(index-of (context-free-binds new-ctx) name) ,(second (first (environment-binds env)))))))))
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


(define (read-all port)
  (let ((read-value (read port)))
    (if (eof-object? read-value)
        '()
        (cons read-value (read-all port)))))

(define mock-ctx
  (context (environment '((a 0) (d 1)) (environment '((b 0) (c 1)) #f)) '() '()))

(define mock-thunk `((asd a b c d) (display c) (newline)))
(define mock-params '(x y z))

(define empty-ctx (context (environment '() #f) '() '()))

(define (evala)
  (evaluate-thunk (read-all (open-input-file "a.scm")) empty-ctx))


(define (read-stdin)
  (read-all (current-input-port)))

(define (cli cmd module)
  (cond
    ((equal? cmd "--extract-symbols")
     (call-with-values 
      (lambda () (to-c (context-ops (evaluate-thunk (read-stdin) empty-ctx))))
      (lambda (lambdas defines fetches imports symbols quotes)
        (display (stitch-symbol-constants (dedupe symbols))))))

    ((equal? cmd "--build")
     (display (call-with-values (lambda () (to-c (context-ops (evaluate-thunk (read-stdin) empty-ctx)))) (lambda (lambdas defines fetches imports symbols quotes) (stitch-program module lambdas defines fetches imports symbols quotes #t)))))
    
    ((equal? cmd "--build-tree")
     (display (context-ops (evaluate-thunk (read-stdin) empty-ctx)))
     (newline))))

(let ((args (vector->list (current-command-line-arguments))))
  (when (not (zero? (length args)))
    (apply cli args))
  )

;;(call-with-input-file "a.scm"
;;  (lambda (p) (cli "--build" "a")))

