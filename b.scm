(define (funnee a) 
  (print (+ a 33)))

(define (factorial n)
  (if (equal? n 0)
    1
    (* (factorial (- n 1)) n)))
