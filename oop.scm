
(define (person name age personality)

  (define (person-behavior msg self)
    (if (eq? msg 'name)
        (car self)
        (if (eq? msg 'age)
            (car (cdr self))
            (if (eq? msg 'personality)
                (car (cdr (cdr self)))
                '()))))
  (object (cons name (cons age (cons personality '()))) person-behavior))


(define john (person 'john 23 'joyful))

(display (john 'name))
(newline)
(display (john 'age))
(newline)
(display (john 'personality))
(newline)
