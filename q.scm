(define code 
  '(() . ((k a b) . (print k a)) 0 1 2)) 

(define P
  '(
     (() . (exit 0))
     (() . (+ 2 -8 7))
     ((x) . (if 3 0 x x))
     ((x) . (print 0 x))))

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
  (apply string-append `("void " ,name "(q_stack s)")))


(define (store-temp-arguments args n)
  (if (nil? args)
    ""
    (apply string-append
      (store-temp-arguments (cdr args) (+ n 1))
      (let ((arg (car args)))
        `("\tq_value t_" ,(symbol->string arg) " = Q_FETCH(s, " ,(number->string n) ");\n")))))

(define (edit-stack args n)
  (string-append 
    "\tQ_RESIZE(s, " (number->string (length args)) ");\n"
    (let loop ((args args) (n n))
      (if (nil? args)
        ""
        (apply string-append
          (loop (cdr args) (+ n 1))
          (let ((arg (car args)))
            `("\tQ_STORE(s, " ,(number->string n) ", " 
              ,(cond 
                 ((symbol? arg) (values "t_" (symbol->string arg)))
                 ((number? arg) (values "Q_NUMBER(" (number->string arg) ")"))
                 (else (error "bad argument"))) ");\n")))))))

(define (make-call expr n)
  (case (car expr)
    ('apply (string-append "\tq_apply(s);"))
    ('print (string-append "\tq_print(s);"))
    ('exit (string-append "\tq_exit(s);"))
    ('+ (string-append "\tq_add(s);"))
    ('- (string-append "\tq_sub(s);"))
    ('* (string-append "\tq_mul(s);"))
    ('/ (string-append "\tq_div(s);"))
    ('pair (string-append "\tq_pair(s);"))
    ('pair? (string-append "\tq_is_pair(s);"))
    ('if (string-append "\tq_branch(s);"))
    (else (error (string-append "unrecognized operation " (symbol->string (car expr)))))))

(define (proc-body params expr)
  (let* (
         (op (car expr))
         (args (cdr expr))
         (temp-arguments-section (store-temp-arguments params 0))
         (edit-stack-section (edit-stack args 0))
         (call-section (make-call expr 0)))
    (string-append "{\n" temp-arguments-section "\n\n" edit-stack-section "\n\n" call-section "\n}")))

(define (proc-to-c name proc)
  (let* ((params (car proc))
         (expr (cdr proc))
         (declaration (proc-cdecl name params)) 
         (body (proc-body params expr)))
    (string-append declaration "\n" body)))

(define (enumerate-functions p n)
  (if (nil? p) 
    '()
     (cons (string-append "\tf_" (number->string n)) (enumerate-functions (cdr p) (+ n 1)))))

(define (program-create-global-function-table p)
  (string-append "q_function Q_GFT[] = {\n" (string-join (enumerate-functions p 0) ",\n") "\n};\n\n"))

(define (program-define-main p start)
  (string-append "int main() { return q_main(" (number->string start) "); } "))

(define (program-to-c p start)
  (string-append
    "#include \"qruntime.h\"\n"
    "#include \"qmain.h\"\n\n"
    (string-join 
      (let loop ((procedures p) (n 0))
        (if (nil? procedures)
           '()
           (cons (proc-to-c (string-append "f_" (number->string n)) (car procedures)) (loop (cdr procedures) (+ n 1))))) "\n\n")
    "\n\n" 
    (program-create-global-function-table p)
    "\n\n"
    (program-define-main p start)))
           
;;(trace (proc-to-c "f_" '((a b c) . (apply 1 b c))))
(trace (program-to-c P 1))

