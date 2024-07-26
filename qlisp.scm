#lang racket
(require struct-update)
(require "backend.scm")
(require "env.scm")
(require "context.scm")
(require "utils.scm")

(struct bind (name index))


;;(define (append-ir ctx ir) '())

(define (syntax-binary op a b ctx)
  (evaluate-expr
   a

   (pop-env-offset
    1
    (evaluate-expr b (push-env-offset (push-ir ctx `(,op)))))))

(define (syntax-unary op expr ctx)
  (evaluate-expr expr (push-ir ctx `(,op))))

(define (syntax-nullary op ctx)
  (push-ir ctx `(,op)))

(define (env-offset env n)
  (environment (map (lambda (bind) (list (first bind) (+ n (second bind)))) (environment-binds env)) (environment-parent env)))

(define (env-append env names)
  (environment (append (map (lambda (name) (list name )) names) (environment-binds env)) (environment-parent env)))

(define (push-env-offset ctx)
  (context-env-update ctx (lambda (env) (env-offset env 1))))

(define (push-env-offset-by n ctx)
  (context-env-update ctx (lambda (env) (env-offset env n))))

(define (pop-env-offset n ctx)
  (context-env-update ctx (lambda (env) (env-offset env (- n)))))

(define (env-append-ctx names ctx)
  (context-env-update ctx (lambda (env) (env-append (env-offset env (length names)) names ))))

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


(define (evaluate-thunk thunk ctx)
  (if (null? thunk)
      ctx
      (evaluate-expr
       (first thunk)
       (if (pair? (rest thunk)) ; not at tail?
           (push-ir (evaluate-thunk (rest thunk) ctx) `(drop))
           (evaluate-thunk (cdr thunk) ctx)))))
(define (let->lambda names body)
  `((lambda ,(map first names) ,@body) ,@(map second names)))

(define (syntax-let names body ctx)
  (evaluate-expr (let->lambda names body) ctx))

(define (syntax-import module ctx)
  (push-ir ctx `(import ,module)))

(define (syntax-provide names ctx)
  (push-ir ctx `(provide ,names)))

(define (syntax-if condition iftrue iffalse ctx)
  (evaluate-expr
   condition

   (context-ops-set
    ctx
    `(branch
      ,(context-ops (evaluate-expr iftrue ctx))
      ,(context-ops (evaluate-expr iffalse ctx))))))

(define (syntax-quote x ctx)
  (push-ir ctx `(quote ,x)))

(define (syntax-lambda params body ctx)
  (let* ((env (context-env ctx))
         (param-count (length params))
         (body-env (params->env (reverse params) env))
         (body-ctx (evaluate-thunk body (context body-env '() '()))))
    (push-ir
     (context env (context-free-binds ctx) (context-ops ctx))
     `(closure ,param-count 0 ,(context-ops body-ctx)))))

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
              ;; built-in binary calls
              ((+ - * / >= <= > < = and or)
               (syntax-binary f (second expr) (third expr) ctx))

              ;; built-in unary calls
              ((not)
               (syntax-unary f (second expr) ctx))

              ;; special forms
              ((define) (syntax-define (second expr) (list-tail expr 2) ctx))
              ((if) (syntax-if (second expr) (third expr) (fourth expr) ctx))
              ((lambda) (syntax-lambda (second expr) (list-tail expr 2) ctx))
              ((begin) (evaluate-thunk (rest expr) ctx))
              ((require) (syntax-import (second expr) ctx))
	      ((provide) (syntax-provide (rest expr) ctx))
	      ((let) (syntax-let (second expr) (list-tail expr 2) ctx))

              ;; calls
              (else (evaluate-call (first expr) (rest expr) ctx)))))

      ;; immediates/variables
      (leaf-expr expr ctx)))

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
    ((string? x)
     (syntax-quote x ctx))
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

(define (eval-file path)
  (context-ops (evaluate-thunk (read-all (open-input-file path)) empty-ctx)))

(let ((args (vector->list (current-command-line-arguments))))
  (when (not (zero? (length args)))
    (apply cli args))
  )

;;(call-with-input-file "a.scm"
;;  (lambda (p) (cli "--build" "a")))
