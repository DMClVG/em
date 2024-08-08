(require std)
(require person)
(require read)

(define (factorial n)
  (if (= n 0)
      1
      (* (factorial (- n 1)) n)))

(display "'(a b c) has length: ")
(printf 'number (list-length '(a b c)))
(newline)

(printf 'string "~~~INFO~~~")
(newline)
(info john)
(printf 'string "~~~~~~~~~~")
(newline)

(printf 'string "~~~INFO~~~")
(newline)
(info melissa)
(printf 'string "~~~~~~~~~~")
(newline)


(define (print string)
  (printf 'string string))

(define (here)
  (printf 'string "Here"))

(define (tprint x)
  (define (tprint-list ls)
    (if ls
        (begin
         (let ((a (car ls))
               (b (cdr ls)))
           (here)

           (tprint a)

           (here)

           (if (eq? (type b) 'cons)
               (tprint-list b)
               (begin
                 (printf 'string " . ")
                 (tprint b)))))
        0))

  (here)
  (printf 'number x)
  (print (type x))

  (case (type x)
    ('cons
     (printf 'string "(")
     (tprint-list (value x))
     (printf 'string ")"))
    ('string
     (printf 'string "\"")
     (printf 'string (value x))
     (printf 'string "\""))
    ('symbol
     (here)
     (printf 'string (value x)))
    ('float
     (printf 'string "[FLOAT]"))))

(tprint (read "haha"))

;;(newline)
