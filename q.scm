(define code 
  '(() . ((k a b) . (print k a)) 0 1 2)) 

;;(define P
;;  '(
;;     (() () exit (0))
;;     (() (-7 7) + (2))
;;     ((x) (x x) if (3 0))
;;     ((x) (x) print (0))))


(define P2
  '(
     (() () exit (0))
     (() () lambda (2 3))
     ((f) (f) call (0))
     (() (69420) print (1))
     
     (() . (200) make-buffer (0))
     ((b) . (b b 8 6) + (1))))


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
        `("\tq_value t_" ,(symbol->string arg) " = Q_FETCH(&s, " ,(number->string n) ");\n")))))

(define (edit-stack args n)
  (string-append 
    "\tQ_RESIZE(&s, " (number->string (length args)) ");\n"
    (let loop ((args args) (n n))
      (if (nil? args)
        ""
        (apply string-append
          (loop (cdr args) (+ n 1))
          (let ((arg (car args)))
            `("\tQ_STORE(&s, " ,(number->string n) ", " 
              ,(cond 
                 ((symbol? arg) (values "t_" (symbol->string arg)))
                 ((number? arg) (values "Q_NUMBER(" (number->string arg) ")"))
                 (else (error "bad argument"))) ");\n")))))))

(define (cont->function-id cont)
  (string-append "f_" (number->string cont)))

(define (goto label)
  (string-append "goto " label ";"))

(define (make-call op conts)
  (let ((conts (map cont->function-id conts))) 
   (case op
     ('print 
      (string-join `("q_print(&s);" ,(goto (car conts))) "\n\t"))
     ('exit 
      (string-join `("q_exit(&s);" ,(goto (car conts))) "\n\t"))

     ('+ 
      (string-join `("q_add(&s);" ,(goto (car conts))) "\n\t"))
     ('- 
      (string-join `("q_sub(&s);" ,(goto (car conts))) "\n\t"))
     ('* 
      (string-join `("q_mul(&s);" ,(goto (car conts))) "\n\t"))
     ('/ 
      (string-join `("q_div(&s);" ,(goto (car conts))) "\n\t"))

     ('lambda 
      (string-join `(,(string-append "q_make_lambda(&s, &&" (list-ref conts 1) ");") ,(goto (car conts))) "\n\t"))
     ('call 
      (string-join `("q_call(&s, &next);" ,(goto "*next")) "\n\t"))

     ('branch 
      (string-join `(,(string-append "Q_BRANCH(&s, " (list-ref conts 1) ", " (list-ref conts 2) ");") ,(goto (car conts))) "\n\t"))

     (else (error (string-append "unrecognized operation " (symbol->string (car expr))))))))

(define (proc-body params args op conts)
  (let* (
         (temp-arguments-section (store-temp-arguments params 0))
         (edit-stack-section (edit-stack args 0))
         (call-section (make-call op conts)))

    (string-append "{\n" temp-arguments-section "\n" edit-stack-section "\n\t" call-section "\n}")))

(define (proc-to-c label proc)
  (let* ((params (list-ref proc 0))
         (args (list-ref proc 1))
         (op (list-ref proc 2))
         (conts (list-ref proc 3))

         (body (proc-body params args op conts)))

    (string-append label ":\n" body)))

(define (program-to-c p start)
  (string-append
    "#include \"qruntime.h\"\n"
    "\n"
    "int main() {\n"
    "\tq_stack s;\n"
    "\ts.base = calloc(Q_STACK_SIZE, sizeof(q_value));\n"
    "\ts.top = s.base;\n"

    "\tvoid *next = NULL;\n"
    "\tprintf(\"Starting program..\\n\");\n"
    "\t" (goto (cont->function-id start)) "\n"
    "\n"
    (string-join 
      (let loop ((procedures p) (n 0))
        (if (nil? procedures)
           '()
           (cons (proc-to-c (string-append "f_" (number->string n)) (car procedures)) (loop (cdr procedures) (+ n 1))))) "\n\n")
    "\n\n\treturn 0;\n"
    "}\n"))
    
           
;;(trace (proc-to-c "helppp" '((x) (x x) + (0 3))))
;;(trace (proc-to-c "f_" '((a b c) . (apply 1 b c))))
(trace (program-to-c P2 1))

