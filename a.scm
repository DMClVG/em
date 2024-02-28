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

(define (range? a b)
  '())

(print (range '() 2 100))

(print (cons (cons (factorial 8) 0) 2))
(print (* -1 8))

(define abc '(a . b))
(define bc '(b . c))
((lambda ()
   (print '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
   (print abc)))


