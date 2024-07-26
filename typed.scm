(provide
 print
 printf)

(define (print x)
  (display x)
  (newline))

(define (printf f x)
  (if (eq? 'number f)
      (print (number->string x))
      (if (eq? 'string f)
	  (print x)
	  '())))

(define (typed type datum)
  (cons type datum))

(define (type? t) (car t))
(define (datum? t) (cdr t))

(define (null? t) (eq? 'null (type? t)))
(define (boolean? t) (eq? 'boolean (type? t)))
(define (number? t) (eq? 'number (type? t)))
(define (string? t) (eq? 'string (type? t)))
(define (cons? t) (eq? 'cons (type? t)))

(define (display-a-list pair)
  (tdisplay (car pair))

  (if (cons? (cdr pair))

      (begin
	(display " ")
	(display-a-list (datum? (cdr pair))))

      (if (not (null? (cdr pair)))
	  (begin (display " . ")
		 (tdisplay (cdr pair)))
	  '()))
  )

(define (bool->string b) (if b "#t" "#f"))

(define (tnumber n) (typed 'number n))
(define (tcons a b) (typed 'cons (cons a b)))
(define (tbool b) (typed 'boolean b))
(define tnull (typed 'null '()))
(define (tstring s) (typed 'string s))

(define (tdisplay t)
  (if (cons? t)
      (begin
	(display "(")
	(display-a-list (datum? t))
	(display ")"))
      (if (number? t)
	  (display (number->string (datum? t)))
	  (if (string? t)
	      (display (datum? t))
	      (if (null? t)
		  (display "'()")
		  (if (boolean? t)
		      (display (bool->string (datum? t)))
		      (display "ERR")))))))
