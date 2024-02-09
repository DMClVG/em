(define code 
  '(() . ((k a b) . (print k a)) 0 1 2)) 

(define P
  '(
     (() . (print 0 1))
     ((k a b) . (print a b c))))

(define (trace x)
  (display x)
  (newline)
  x)

;;(define (parse ls)
;;  (if (list? (car ls))
;;    (parse-stmt stmt)
;;    (parse-expr expr)))
;;
;;(define (parse-stmt stmt)
;;  (let ((params (car stmt))
;;        (expr (cdr stmt)))
;;
;;    (trace params)
;;    (trace expr)))
(define (param->cparam param)
  (string-append "q_value " (symbol->string param)))

(define (nil? x)
  (equal? x '()))

(define (delimited-list ls del)
  (if (nil? ls) 
    '()
    (if (nil? (cdr ls)) 
      (cons (car ls) '())
      (cons (car ls) (cons del (delimited-list (cdr ls) del))))))

(define (string-join ls del)
  (apply string-append (delimited-list ls del)))

(define (params->cparams params)
  (string-join (map 
                 param->cparam
                 params) ", "))

(define (proc-cdecl name params)
  (apply string-append `("void " ,name "(" ,(params->cparams params) ")")))


(define (setup-arguments args n)
  (if (nil? args)
    ""
    (apply string-append
      (setup-arguments (cdr args) (+ n 1))
      (let ((arg (car args)))
        `("\tq_value n_" ,(number->string n) " = " 
          ,(cond 
             ((number? arg) (number->string arg))
             ((symbol? arg) (symbol->string arg))
             (else (error "nuhuh"))) ";\n")))))

(define (enumerate-call-params params n)
  (if (nil? params)
    '()
    (cons (string-append "n_" (number->string n)) (enumerate-call-params (cdr params) (+ n 1)))))
   
(define (make-call expr n)
  (let ((params (string-join (enumerate-call-params (cdr expr) 0) ", ")))
    (case (car expr)
      ('apply (string-append "\tq_apply(" params ");"))
      ('print (string-append "\tq_print(" params ");"))
      (else (error (string-append "unrecognized operation " (symbol->string (car expr))))))))

(define (proc-body params expr)
  (let* (
         (op (car expr))
         (args (cdr expr))
         (argument-section (setup-arguments args 0))
         (call-section (make-call expr 0)))
    (string-append "{\n" argument-section "\n" call-section "\n}")))

(define (proc-to-c name proc)
  (let* ((params (car proc))
         (expr (cdr proc))
         (declaration (proc-cdecl name params)) 
         (body (proc-body params expr)))
    (string-append declaration "\n" body)))
           
(trace (proc-to-c "babby" '((a b c) . (apply a b c))))

