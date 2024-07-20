(import b)

(define (print x) (display x) (newline))

(define (funny a)
  '(h a h a))

(define (factorial n)
  (if (= n 0)
      1
      (* (factorial (- n 1)) n)))

(define (length ls)
  (define (length-tail n ls)
    (if (null? ls)
        n
        (length-tail (+ n 1) (cdr ls))))
  (length-tail 0 ls))

(define (reverse ls)

  (define (reverse-tail ls res)
    (if (null? ls)
        res
        (reverse-tail (cdr ls) (cons (car ls) res))))

  (reverse-tail ls '()))

(define (count start end)
  (if (<= start end)
      (begin
        (display start)
        (newline)
        (count (+ start 1) end))
      '()))

(count 1 1000)

(print (funny? 'sandy))
(print (factorial 19))

(print (length '(a b c d e f g)))
(print (reverse '(a b c d e f g)))

(define (const x) (lambda (_) x))

(define (map f ls)

  (define (map-tail f ls res)
    (if (null? ls)
      res
      (map-tail f (cdr ls) (cons (f (car ls)) res))))

 (reverse (map-tail f ls '())))


(print (define x (map (const 'ayayaya) '(a b c d e f g h))))
(print (length x))
