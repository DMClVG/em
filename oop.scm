(provide
 info
 john
 melissa
 inc!
 person)

(define (info about)
  (display "Name: ")
  (display (about 'name))
  (newline)
  (display "Age: ")
  (display (number->string (about 'age)))
  (newline)
  (display "Personality: ")
  (display (about 'personality))
  (newline))

(define people-count (box 0))

(define (inc! box)
  (set! box (+ 1 (ref box))))

(define (first ls) (car ls))
(define (second ls) (car (cdr ls)))
(define (third ls) (car (cdr (cdr ls))))

(define (person name age personality)
  (inc! people-count)

  (define (person-behavior msg self)
    (case msg
      ('name (first self))
      ('age  (second self))
      ('personality (third self))
      ('info (info (object self person-behavior)))))

  (object
   (cons name (cons age (cons personality '())))
   person-behavior))

(define john (person "John" 23 "(¬_¬)ﾉ"))
(define melissa (person "Melissa" 27 "ヾ(@^▽^@)ノ"))
