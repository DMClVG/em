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
     (() (69420) print (1))))

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

(define (cont->function-id cont)
  (string-append "f_" (number->string cont)))

(define (make-call op conts)
  (let ((cont (cont->function-id (car conts))))
    (case op
      ('apply (string-append "\t" cont "(q_apply(s));"))
      ('print (string-append "\t" cont "(q_print(s));"))
      ('exit (string-append "\t" cont "(q_exit(s));"))
      ('+ (string-append "\t" cont "(q_add(s));"))
      ('- (string-append "\t" cont "(q_sub(s));"))
      ('* (string-append "\t" cont "(q_mul(s));"))
      ('/ (string-append "\t" cont "(q_div(s));"))
      ('lambda (string-append "\t" cont "(q_make_lambda(s, " (cont->function-id (car (cdr conts))) "));"))
      ('branch (string-append "\tQ_BRANCH(s, " cont ", " (cont->function-id (car (cdr conts))) ");"))
      ('call (string-append "\tq_call(s);"))

      (else (error (string-append "unrecognized operation " (symbol->string (car expr))))))))

(define (proc-body params args op conts)
  (let* (
         (temp-arguments-section (store-temp-arguments params 0))
         (edit-stack-section (edit-stack args 0))
         (call-section (make-call op conts)))
    (string-append "{\n" temp-arguments-section "\n" edit-stack-section "\n" call-section "\n}")))

(define (proc-to-c name proc)
  (let* ((params (list-ref proc 0))
         (args (list-ref proc 1))
         (op (list-ref proc 2))
         (conts (list-ref proc 3))

         (declaration (proc-cdecl name params)) 
         (body (proc-body params args op conts)))

    (string-append declaration "\n" body)))

(define (enumerate-functions p n)
  (if (nil? p) 
    '()
     (cons (string-append "f_" (number->string n)) (enumerate-functions (cdr p) (+ n 1)))))

(define (program-create-global-function-table p)
  (string-append "q_function Q_GFT[] = {\n" (string-join (enumerate-functions p 0) ",\n") "\n};\n\n"))

(define (to-top-decl x)
  (string-append "void " x "(q_stack s);\n"))

(define (top-defs p)
  (apply string-append (map to-top-decl (enumerate-functions p 0))))

(define (program-define-main p start)
  (string-append "int main() { Q_MAIN(" (cont->function-id start) "); return 0; } "))

(define (program-to-c p start)
  (string-append
    "#include \"qruntime.h\"\n"
    "#include \"qmain.h\"\n"
    "\n"
    (top-defs p)
    "\n"
    (string-join 
      (let loop ((procedures p) (n 0))
        (if (nil? procedures)
           '()
           (cons (proc-to-c (string-append "f_" (number->string n)) (car procedures)) (loop (cdr procedures) (+ n 1))))) "\n\n")
    "\n\n"
    (program-define-main p start)))
           
;;(trace (proc-to-c "helppp" '((x) (x x) + (0 3))))
;;(trace (proc-to-c "f_" '((a b c) . (apply 1 b c))))
(trace (program-to-c P2 1))

