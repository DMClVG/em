(import b)

(define 
  superduper 
  (c-procedure "superduper"))

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
(print 
  ((lambda ()
     (print 
       '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
     (superduper (cdr abc)))))

(print (lambda () '()))
(print (funnee 12))
