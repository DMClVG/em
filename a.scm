(require b)
(require oop)
(require typed)

(provide
 const
 compose
 funny
 factorial
 length
 count)

(define (hello x)
  (display 'hello-my-dear)
  (display x)
  (newline))

(define (const x)
  (define (const-function _ self) self)
  (object x const-function))

(define (compose f g)
  (define (compose-function x self)
    ((car self) ((car (cdr self)) x)))

  (object (cons f (cons g '())) compose-function))

(define (funny a)
  '(h a h a))

(define (factorial n)
  (if (= n 0)
      1
      (* (factorial (- n 1)) n)))

(define (length ls)
  (define (length-tail n ls)
    (if (eq? ls 0)
        n
        (length-tail (+ n 1) (cdr ls))))
  (length-tail 0 ls))

(define (reverse ls)

  (define (reverse-tail ls res)
    (if (eq? ls 0)
        res
        (reverse-tail (cdr ls) (cons (car ls) res))))

  (reverse-tail ls '()))

(define (count start end)
  (if (<= start end)
      (begin
        (display (number->string start))
        (newline)
        (count (+ start 1) end))
      '()))


(newline)
(print (number->string (length (cons 'a (cons 'b (cons 'c '()))))))

;;(print (compose (lambda (x) (+ x 2)) (lambda (x) (* x 5))))
(print (number->string 23132))

(info john)
(let ((hey (john 'name)))
  (display "hey ")
  (display hey)
  (display "!")
  (newline))

(define (curry f x)
  (define (curry-behavior y self)
    ((car self) (cdr self) y))
  (object (cons f x) curry-behavior))


(printf 'number 122)
(printf 'number ((const 8) 12))

((compose (curry printf 'number) factorial) 11)
;;((curry printf 'number) 21)
(printf 'number 11)
