#lang racket
(require struct-update)
(provide
 context
 context-free-binds-update
 context-free-binds
 context-env-update
 context-ops-set
 context-ops
 context-env
 push-ir)

(struct context (env free-binds ops) #:transparent)
(define-struct-updaters context)

(define (push-ir ctx op) (context-ops-set ctx (append op (list (context-ops ctx)))))
