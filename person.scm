(provide
 info

 john
 melissa

 person
 name-of-person
 age-of-person
 personality-of-person)

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

(struct person
   (name-of-person
    age-of-person
    personality-of-person))

(define john (person "John" 23 "(¬_¬)ﾉ"))
(define melissa (person "Melissa" 27 "ヾ(@^▽^@)ノ"))
