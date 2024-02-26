(import b)

(define (wrong x y)
  (if 0
    (print x)
    (begin
      (funnee x)
      (wrong y x))))

(define (range x a b)
  (if (equal? a b)
    x
    (range (cons b x) a (- b 1))))


(print (range 0 2 100))

;;(print (pair (pair (factorial 8) 0) 2))
;;(print (* -1 8))


