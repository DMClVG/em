(require b)
(require oop)
(require typed)

(provide
 funny
 melissa
 factorial
 length
 count)

(struct string (length-of-string chars-of-string))
;; (struct cons   (cdr car))

(define (hello x)
  (display 'hello-my-dear)
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
        (display (number->string start))
        (newline)
        (count (+ start 1) end))
      '()))

(newline)

(display "'(a b c) has length: ")
(printf 'number (length '(a b c)))

(printf 'string "~~~INFO~~~")
(info john)
(printf 'string "~~~~~~~~~~")

(printf 'string "~~~INFO~~~")
(info melissa)
(printf 'string "~~~~~~~~~~")

(printf 'number (length-of-string "hi"))
(printf 'number (chars-of-string "hi"))
