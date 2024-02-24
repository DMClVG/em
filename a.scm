(import b)

(define (wrong x y)
  (if 0
    (print x)
    (begin
      (funnee x)
      (wrong y x))))

;;(wrong 2 6)

(print (pair (pair (factorial 8) 0) 2))
;;(print (* -1 8))


