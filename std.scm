(provide
 list-length
 list-reverse
 string-length
 string-address

 inc!
 printf)

(define (inc! box)
  (set! box (+ 1 (ref box))))

(define (printf f x)
  (if (eq? 'number f)
      (display (number->string x))
      (if (eq? 'string f)
	  (display x)
	  '())))

(struct string (string-length string-address))
(struct buffer (buffer-length buffer-address))

(define (list-length ls)
  (define (length-tail n ls)
    (if (eq? ls 0)
        n
        (length-tail (+ n 1) (cdr ls))))
  (length-tail 0 ls))

(define (list-reverse ls)
  (define (reverse-tail ls res)
    (if (eq? ls 0)
        res
        (reverse-tail (cdr ls) (cons (car ls) res))))
  (reverse-tail ls '()))
