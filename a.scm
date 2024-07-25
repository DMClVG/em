(require b)
(require oop)

(define (hello x)
  (display 'hello-my-dear)
  (display x)
  (newline))

(define (const x)
  (define (const-function _ self)
    self)
  (object x const-function))

(define (compose f g)
  (define (compose-function x self)
    ((car self) ((car (cdr self)) x)))

  (object (cons f (cons g '())) compose-function))

(define (print x)
  (display x)
  (newline))

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
        (display start)
        (newline)
        (count (+ start 1) end))
      '()))


;;(print (length '(a b c)))

;;(print (compose (lambda (x) (+ x 2)) (lambda (x) (* x 5))))
(print (number->string 23132))

(info john)


;;(print ((const 43) 3213))

;;(print ((const 8) 12))

;;(print ((compose 12 321) 11))
