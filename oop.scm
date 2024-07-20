(define (person name age personality)
  (lambda (msg)
    (if (eq? msg 'name)
	name
	(if (eq? msg 'age)
	    age
	    (if (eq? msg 'personality)
		personality
		#f)))))

(define john (person 'john 23 'joyful))

(john 'name)
;;(newline)
