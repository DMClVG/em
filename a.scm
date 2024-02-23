(define (factorial n)
  (if (equal? n 0)
    1
    (* (factorial (- n 1)) n)))

(define (wrong x y)
  (if  0
    (print x)
    (wrong y x)))

(print (factorial 8))
;;(wrong 2 6)


