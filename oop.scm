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

(define (person name age personality)

  (define (person-behavior msg self)
    (if (eq? msg 'name)
        (car self)
        (if (eq? msg 'age)
            (car (cdr self))
            (if (eq? msg 'personality)
                (car (cdr (cdr self)))
                (if (eq? msg 'info)
		    (info (object self person-behavior))
		    '())))))

  (object
   (cons name (cons age (cons personality '())))
   person-behavior))


(define john (person "John" 23 "(¬_¬)ﾉ"))
(define melissa (person "Melissa" 27 "ヾ(@^▽^@)ノ"))
