;;(import b)
;;
;;(define 
;;  superduper 
;;  (c-procedure "superduper"))

;;(if (equal? (cons 'a 'b) (cons 'a 'b))
;;  (define (display x)
;;    (display x))
;;  '()

;;(define (print x)
;;  (display x)
;;  (newline))

(define (funnee a) 
  '(h a h a))
;;
;;(define (factorial n)
;;  (if (equal? n 0)
;;    1
;;    (* (factorial (- n 1)) n)))
;;
;;(define (wrong x y)
;;  (if 0
;;    (print x)
;;    (begin
;;      (funnee x)
;;      (wrong y x))))
;;
;;(define (range x a b)
;;  (if (equal? a b)
;;    x
;;    (range (cons b x) a (- b 1))))
;;
;;(define (range? a b)
;;  '())
;;
;;(print (range '() 2 100))
;;
;;(print (cons (cons (factorial 8) 0) 2))
;;(print (* -1 8))
;;
;;(define abc '(a . b))
;;(define bc '(b . c))
;;;;(print 
;;;;  ((lambda ()
;;;;     (print 
;;;;       '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
;;;;     (cdr abc))))
;;
;;(print (lambda () '()))
;;(print (funnee 12))
;;
;;(define (length ls)
;;  (define (length-tail n ls)
;;    (if (null? ls)
;;      n
;;      (length-tail (+ n 1) (cdr ls))))
;;  (length-tail 0 ls))
;;
;;(print (length '()))
;;(print (length (range '() 0 100)))
;;(print (equal? #t #t))
;;
;;(print (boolean? #f))
;;(print (pair? '(a b)))
;;(print (null? '()))
;;(print (symbol? (car '(c d))))
;;(print (number? 122))
;;(print (procedure? length))
;;
;;(print 'the-other-stuff)
;;(print (<= 0 0))
;;(print (< -12 0))
;;(print (< 1 -12))
;;(print (equal? -12 -12))
;;
;;(define (reverse ls)
;;
;;  (define (reverse-tail ls res)
;;    (if (null? ls)
;;      res
;;      (reverse-tail (cdr ls) (cons (car ls) res))))
;;  
;;  (reverse-tail ls '()))
;;
;;
;;(define (map f ls)
;;
;;  (define (map-tail f ls res)
;;    (if (null? ls)
;;      res
;;      (map-tail f (cdr ls) (cons (f (car ls)) res))))
;;
;; (reverse (map-tail f ls '())))
;;
;;(define (call f x)
;;  (f x))
;;(define (this-thing)
;;  (let ((other (cons 1 1)))
;;    (let ((alpha 12) 
;;          (sud 13))
;;      (display alpha)
;;      (newline))
;;    (display other)
;;    (newline))))
     ;; (print (lambda () (+ sud alpha))))
      ;;(print (map (lambda (x) (+ 0 alpha)) '(1 2 3 4 1 23 34 4 43 5325 235 3 43))))

;;(display (cons 1 (cons 2 '(yaya . baba))))
;;(newline)
;;(display '(1 2 (ya) 4))
;;(newline)
;;(display (funnee 2))
;;
;;(define (loop x)
;;  (display x)
;;  (newline)
;;  (loop (+ x 1)))

;;(loop 0)

(define (aba a b c) 
  (display (cons a ( cons b (cons c '())))))

(let ((avar 1)
      (mama 11))
  (display mama) 
  (newline)
  (display (+ mama avar))
  (newline))

(display (and #t #f))
(display (or #f #t))
(display (not #f))

(if (>= 2 3)
  (display 'whaaat)
  (display 'nooo))


;;(print (call symbol? 'babauioe))
