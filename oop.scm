(define (person name age personality)
  (lambda (msg)
    (if (eq? msg 'name)
        name
        '())))


(define john (person 'john 23 'joyful))

(display (john 'name))
(newline)
