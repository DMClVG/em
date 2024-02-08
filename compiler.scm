(use-modules 
  (srfi srfi-1)
  (srfi srfi-9)
  (rnrs bytevectors))

(define (trace x)
  (display x)
  (newline)
  x)

(define bv-ref bytevector-u8-ref)
(define bv-set! bytevector-u8-set!)

(define P
 #(
   (loadi 0 0)
   (loadi 1 9)
   (loadi 20 10)

   (write 10)
   (goto 9)))

(define (interpret state code data)
  (let* (
         (pc (assoc-ref state 'pc))
         (op (vector-ref code pc)))
    (case (car op)
      ('loadi 
       (bv-set! data (list-ref op 2) (list-ref op 1))
       (assoc-set! state 'pc (+ 1 pc)))
       
      ('load 
       (bv-set! data (bv-ref data (list-ref op 2)) (list-ref op 1))
       (assoc-set! state 'pc (+ 1 pc)))
       
      ('add 
       (bv-set! data 
        (+ 
          (bv-ref data (list-ref op 1))
          (bv-ref data (list-ref op 2)))
        (bv-ref data (list-ref op 3)))
       (assoc-set! state 'pc (+ 1 pc)))

      ('write
       (trace (bv-ref data (list-ref op 1)))
       (assoc-set! state 'pc (+ 1 pc)))
       
      ('goto 
       (assoc-set! state 'pc (bv-ref data (list-ref op 1)))))
    (unless (equal? (assoc-ref state 'pc) 0)
      (interpret state code data))))
      
(interpret (acons 'pc 0 '()) P (make-bytevector 20 0)) 

