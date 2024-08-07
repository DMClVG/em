#lang racket
(require struct-update)
(require "context.scm")
(require "utils.scm")
(provide
 params->env
 get-bound
 environment
 environment-binds
 environment-parent
 env-name
 )

(struct environment (binds parent) #:transparent)
(define-struct-updaters environment)

(define (params->env params parent)
  (environment (let next-param ((params params) (res '()))
                 (if (pair? params)
                     (next-param (cdr params) (cons (list (car params) (length res)) res))
                     res)) parent))

(define (get-bound name env)
  (cond
    ((assoc name (environment-binds env)) => (lambda (n) `(,env ,n)))
    (else #f)))


(define (env-name-found name bind env ctx)
  (push-ir ctx `(pick ,(second (second bind)))))

(define (env-name name env ctx)
  (cond
    ((get-bound name env) => (λ (bind) (env-name-found name bind env ctx)))
    (else (push-ir ctx `(fetch ,name)))))
