(define P
  '(
     (() () exit (0) () ())
     (() (-7 7) + (2) () ())
     ((x) (x x) if (3 0) () ())
     ((x) (x) print (0) () ())))


(define P2
  '(
     (() () exit (0) () ())
     (() () lambda (2 3) () ())
     ((f) (f) call (0) () ())
     (() (69420) print (1) () ())))

(define s2
  '(
    (() (5) goto (1) () ()) ; 0
    ((a) (a a) + (2) () ()) ; 1
    ((c) (100 c c) > (3) () ()) ; 2
    ((res) (res) branch (4 5) () ()) ; 3
    ((c) (c) goto (1) () ()) ; 4
    ((c) (c) print (6) () ()) ; 5
    (() () exit () () ()))) ; 6


(define mem1
  '(
    (() (128) alloc (1) () ())
    (() (12 69) store! (2) (m) (m))
    (() (12) load (3) (m) (m))
    
    ((x) (x) print (4) (m) (m))
    (() () drop (5) (m) (m))
    (() () exit () () ())))

(define (trace x)
  (display x)
  (newline)
  x)

(define (nil? x)
  (equal? x '()))

(define (count x ls)
  (if (nil? ls)
    0
    (if (equal? (car ls) x)
      (+ 1 (count x (cdr ls)))
      (count x (cdr ls)))))

(define (symlist->string ls)
  (string-join (map symbol->string ls) " "))

(define (validate-objects-stack-effect params args)
  (let* 
    next-param 
    ((params params))

    (when (not (nil? params))
      (if (equal? (count (car params) args) 1)
        (next-param (cdr params))
        (error "Invalid stack effect " (symlist->string params) (symlist->string args))))))

(define (delimited-list ls del)
  (if (nil? ls) 
    '()
    (if (nil? (cdr ls)) 
      (cons (car ls) '())
      (cons (car ls) (cons del (delimited-list (cdr ls) del))))))

(define (string-join ls del)
  (apply string-append (delimited-list ls del)))

(define (store-temp params n)
  (string-append 
    (if (nil? params) "" (values "\tq_value " (string-join (map (lambda (x) (string-append "t_" (symbol->string x))) params) ", ") ";\n"))

    (let loop ((params params) (n n))
      (if (nil? params)
        ""
        (apply string-append
          (loop (cdr params) (+ n 1))
          (let ((param (car params)))
            `("\tQ_FETCH(&s, " ,(number->string n) ", &t_" ,(symbol->string param) ");\n")))))))

(define (store-temp-objects params n)
  (string-append 
    (if (nil? params) "" (values "\tq_value " (string-join (map (lambda (x) (string-append "o_" (symbol->string x))) params) ", ") ";\n"))

    (let loop ((params params) (n n))
      (if (nil? params)
        ""
        (apply string-append
          (loop (cdr params) (+ n 1))
          (let ((param (car params)))
            `("\tQ_FETCH(&o, " ,(number->string n) ", &o_" ,(symbol->string param) ");\n")))))))

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

(define (edit-stack-objects params args n)
  (string-append 
    "\tQ_POP(&o, " (number->string (length params)) ");\n"
    "\tQ_PUSH(&o, " (number->string (length args)) ");\n"
    (let loop ((args args) (n n))
      (if (nil? args)
        ""
        (apply string-append
          (loop (cdr args) (+ n 1))
          (let ((arg (car args)))
            `("\tQ_STORE(&o, " ,(number->string n) ", " 
              ,(cond 
                 ((symbol? arg) (values "o_" (symbol->string arg)))
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
      (string-join `("q_alloc(&s, &o);" ,(goto (car conts))) "\n\t"))
     ('store!
      (string-join `("q_store(&s, &o);" ,(goto (car conts))) "\n\t"))
     ('load
      (string-join `("q_load(&s, &o);" ,(goto (car conts))) "\n\t"))
     ('size
      (string-join `("q_size(&s, &o);" ,(goto (car conts))) "\n\t"))
     ('drop
      (string-join `("q_drop(&o);" ,(goto (car conts))) "\n\t"))

     (else (error (string-append "unrecognized operation " (symbol->string op)))))))


(define (proc-body params args op conts o-params o-args)
  (validate-objects-stack-effect o-params o-args)

  (let* (
         (temp-section (store-temp params 0))
         (temp-objects-section (store-temp-objects o-params 0))
         (edit-stack-section (edit-stack params args 0))
         (edit-stack-objects-section (edit-stack-objects o-params o-args 0))
         (call-section (make-call op conts)))

    (string-append "{\n" temp-section "\n" temp-objects-section "\n" edit-stack-section "\n" edit-stack-objects-section "\n\t" call-section "\n}")))

(define (proc-to-c label proc)
  (let* ((params (list-ref proc 0))
         (args (list-ref proc 1))
         (op (list-ref proc 2))
         (conts (list-ref proc 3))
         (o-params (list-ref proc 4))
         (o-args (list-ref proc 5))

         (body (proc-body params args op conts o-params o-args)))

    (string-append label ":\n" body)))

(define (program-to-c p start)
  (string-append
    "#include \"qruntime.h\"\n"
    "\n"
    "int main() {\n"
    "\tq_stack s;\n"
    "\ts.base = calloc(Q_STACK_SIZE, sizeof(q_value));\n"
    "\ts.top = s.base;\n"
    "\n"
    "\tq_stack o;\n"
    "\to.base = calloc(Q_STACK_SIZE, sizeof(q_value));\n"
    "\to.top = o.base;\n"
    "\n"
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

