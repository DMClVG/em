(use-modules 
  (srfi srfi-1)
  (srfi srfi-9)
  (rnrs bytevectors))

(define (trace x)
  (display x)
  (newline)
  x)
      

