(provide
 type
 value
 read)

(struct tcell (type value))

(define old-cons cons)



(define (string s)
  (tcell 'string s))

(define (symbol s)
  (tcell 'symbol s))

(define (float f)
  (tcell 'float f))

(define (read s)
  (tcell 'cons (cons (symbol 'a) (symbol 'b))))
