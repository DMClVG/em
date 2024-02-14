(define P
  '(
     (() () exit (0))
     (() (-7 7) + (2))
     ((x) (x x) if (3 0))
     ((x) (x) print (0))))


(define P2
  '(
     (() () exit (0))
     (() () lambda (2 3))
     ((f) (f) call (0))
     (() (69420) print (1))))

(define s2
  '(
    (() (3) goto (1)) ; 0
    ((a) (a a) + (2)) ; 1
    ((c) (10 c c) > (3)) ; 2
    ((res) (res) branch (4 5)) ; 3
    ((c) (c) goto (1)) ; 4
    ((c) (c) print (6)) ; 5
    (() () exit ()))) ; 6


(define mem1
  '(
    (() (128) alloc (1))
    ((m) (m 12 69 m) store! (2))
    ((m) (m 12) load (3))
    
    ((x) (x) print (4))
    (() () exit ())))

(define (trace x)
  (display x)
  (newline)
  x)

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

(define (store-temp params n)
  (if (nil? params)
    ""
    (apply string-append
      (store-temp (cdr params) (+ n 1))
      (let ((param (car params)))
        `("\tq_value t_" ,(symbol->string param) " = Q_FETCH(&s, " ,(number->string n) ");\n")))))

(define (edit-stack params args n)
  (string-append 
    ;;"\tQ_RESIZE(&s, " (number->string (length args)) ");\n"
    "\tQ_POP(&s, " (number->string (length params)) ");\n"
    "\tQ_PUSH(&s, " (number->string (length args)) ");\n"
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

(define (cont->label cont)
  (string-append "f_" (number->string cont)))

(define (goto label)
  (string-append "goto " label ";"))

(define (make-call op conts)
  (let ((conts (map cont->label conts))) 
   (case op
     ('print 
      (string-join `("q_print(&s);" ,(goto (car conts))) "\n\t"))
     ('exit 
      "q_exit(&s);")

     ('+ 
      (string-join `("q_add(&s);" ,(goto (car conts))) "\n\t"))
     ('- 
      (string-join `("q_sub(&s);" ,(goto (car conts))) "\n\t"))
     ('* 
      (string-join `("q_mul(&s);" ,(goto (car conts))) "\n\t"))
     ('/ 
      (string-join `("q_div(&s);" ,(goto (car conts))) "\n\t"))
     ('equal?
      (string-join `("q_is_equal(&s);" ,(goto (car conts))) "\n\t"))
     ('>
      (string-join `("q_is_greater(&s);" ,(goto (car conts))) "\n\t"))

     ('lambda 
      (string-join `(,(string-append "q_make_lambda(&s, &&" (list-ref conts 1) ");") ,(goto (car conts))) "\n\t"))
     ('call 
      (string-join `("q_call(&s, &next);" ,(goto "*next")) "\n\t"))

     ('branch 
      (string-join `(,(string-append "Q_BRANCH(&s, " (list-ref conts 0) ", " (list-ref conts 1) ");")) "\n\t"))
     ('goto
      (goto (car conts)))

     ('alloc
      (string-join `("q_alloc(&s);" ,(goto (car conts))) "\n\t"))
     ('store!
      (string-join `("q_store(&s);" ,(goto (car conts))) "\n\t"))
     ('load
      (string-join `("q_load(&s);" ,(goto (car conts))) "\n\t"))
     ('size
      (string-join `("q_size(&s);" ,(goto (car conts))) "\n\t"))

     (else (error (string-append "unrecognized operation " (symbol->string op)))))))


(define (proc-body params args op conts)
  (let* (
         (temp-section (store-temp params 0))
         (edit-stack-section (edit-stack params args 0))
         (call-section (make-call op conts)))

    (string-append "{\n" temp-section "\n" edit-stack-section "\n\t" call-section "\n}")))

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
    "\t" (goto (cont->label start)) "\n"
    "\n"
    (string-join 
      (let loop ((procedures p) (n 0))
        (if (nil? procedures)
           '()
           (cons (proc-to-c (string-append "f_" (number->string n)) (car procedures)) (loop (cdr procedures) (+ n 1))))) "\n\n")
    "\n\n\treturn 0;\n"
    "}\n"))

(trace (program-to-c mem1 0))

