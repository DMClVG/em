(provide
 info
 john
 melissa
 person
 name-of-person
 age-of-person
 personality-of-person
 inc!
 person)

(define (info about)
  (display "Name: ")
  (display (name-of-person about))
  (newline)
  (display "Age: ")
  (display (number->string (age-of-person about)))
  (newline)
  (display "Personality: ")
  (display (personality-of-person about))
  (newline))

(define people-count (box 0))

(define (inc! box)
  (set! box (+ 1 (ref box))))

(struct person
   (name-of-person
    age-of-person
    personality-of-person))

(define john (person "John" 23 "(¬_¬)ﾉ"))
(define melissa (person "Melissa" 27 "ヾ(@^▽^@)ノ"))
