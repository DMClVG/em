#lang racket
(provide
 todo
 trace
 dedupe)

(define (dedupe e)
  (if (null? e) '()
      (cons (car e) (dedupe (filter (lambda (x) (not (equal? x (car e))))
                                    (cdr e))))))

(define (trace . vals)
  (for-each
   (lambda (x)
     (display x)
     (newline))
   vals)
  (first vals))


(define (todo)
  (error "todo"))
