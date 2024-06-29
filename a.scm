
(define (print x) (display x) (newline))

;;(define (funnee a) 
;;  '(h a h a))
;;
;;(define (factorial n)
;; (if (equal? n 0)
;;    1
;;    (* (factorial (- n 1)) n)))
;;
;;(define (length ls)
;;  (define (length-tail n ls)
;;    (if (null? ls)
;;      n
;;      (length-tail (+ n 1) (cdr ls))))
;;  (length-tail 0 ls))
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
;;(define (count start end)
;;  (if (<= start end)
;;    (begin 
;;      (display start)
;;      (newline)
;;      (count (+ start 1) end))
;;    '()))
;;
;;(count 1 1000)
;;
;;(print (funny? 'adam))
;;(print (factorial 19))

;;(print (length '(a b c d e f g)))
;;(print (reverse '(a b c d e f g)))
(print 1)
