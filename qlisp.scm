(define (function-signature name) 
  (string-append ": "name))

(define (syntax-define head expr env cont)
  (cond 
    ((pair? head) 
     (syntax-define (car head) (list (append `(lambda ,(cdr head)) expr)) env cont)) ;; lambda
    ((symbol? head) 
     (evaluate-expr (car expr) env `(define ,head ,cont))))) ;; normal

(define (syntax-binary op a b env cont)
  (evaluate-expr a env
    (evaluate-expr b (env-offset env 1) `(,op ,cont))))

(define (syntax-unary op expr env cont)
  (evaluate-expr expr env `(,op ,cont)))

(define (syntax-nullary op env cont)
  `(,op ,cont))

(define (env-offset env n)
  (map 
    (lambda (x) 
      (if (number? (cdr x))
        (cons (car x) (+ (cdr x) n)) 
        x))
    env))

(define (append-names-to-env env names)
  (let ((offsetted-env (env-offset env (length names))))
    (append (let loop ((names names) (n 0)) 
        (if (null? names)
          '()
          (cons 
            (cons (car names) n) 
            (loop (cdr names) (+ n 1)))))
      offsetted-env)))

(define (evaluate-thunk thunk env cont)
  (if (null? thunk)
    cont
    (evaluate-expr 
      (car thunk) 
      env 
      (if (pair? (cdr thunk)) ; at tail?
        `(drop ,(evaluate-thunk (cdr thunk) env cont))
        (evaluate-thunk (cdr thunk) env cont)))))

(define (evaluate-many exprs env cont)
  (if (null? exprs)
    cont
    (evaluate-expr (car exprs) env (evaluate-many (cdr exprs) (env-offset env 1) cont))))

(define (env-make-free env)
  (map (lambda (var) 
         (if (pair? (cdr var))
           `(,(car var) ((car (cdr var)) (+ (car (cdr (cdr var))) (- (length env) (free-count env)))))
           `(,(car var) (free ,(cdr var)))))
       env))

(define (get-frees code)
  (if (pair? code)
    (case (car code)
      ;;('lambda (get-frees (list-ref code 2))) ;; only cont, not body
      ('free (list (list-ref code 1)))
      (else
        (apply append (map get-frees (cdr code)))))
    '()))

(define (free-count env)
  (if (null? env)
    0
    (+ (if (pair? (cdr (car env))) 1 0) (free-count (cdr env)))))

(define (syntax-lambda params body parent-env cont)
  (let* ((env (append (params->env (reverse params)) (env-make-free parent-env)))
         (body (evaluate-thunk body env '()))
         (frees (get-frees body)))
    (if (null? frees)
      `(lambda 
            ,(length params) 
            ,body 
            ,cont)
      `(closure 
         ,(length params) 
         ,body 
         ,cont
         ,(length parent-env)
         ,(free-count parent-env)
         ,(cdr )))))

(define (syntax-let bindings body env cont)
  (evaluate-many 
    (map cadr bindings) 
    env 
    `(push-frame
       ,(evaluate-thunk 
          body 
          (append-names-to-env env (map car bindings)) 
          `(pop-frame ,(length bindings) ,cont)))))

(define (syntax-import module env cont)
  `(import ,module ,cont))

(define (syntax-if condition iftrue iffalse env cont)
  (evaluate-expr 
    condition 
    env 
    `(branch ,(evaluate-expr iftrue env cont) ,(evaluate-expr iffalse env cont))))

(define (syntax-quote x cont)
  `(quote ,x ,cont))

(define (repeat x n)
  (let loop ((n n))
    (if (<= n 0)
      '()
      (cons x (loop (- n 1))))))

(define (padleft s char n)
  (string-append 
    (list->string (repeat char (- n (length s)))) 
    s))

;; define expressions
(define (evaluate-expr expr env cont)
  (if (pair? expr)
    (if (or (equal? (car expr) #_quote) (equal? (car expr) 'quote))
      (syntax-quote (list-ref expr 1) cont) 
      (case (car expr)
        ('define (syntax-define (list-ref expr 1) (list-tail expr 2) env cont)) 
        ('if (syntax-if (list-ref expr 1) (list-ref expr 2) (list-ref expr 3) env cont))

        ('lambda (syntax-lambda (list-ref expr 1) (list-tail expr 2) env cont))
        ('begin (evaluate-thunk (list-tail expr 1) env cont))

        ('+ (syntax-binary '+ (list-ref expr 1) (list-ref expr 2) env cont))
        ('- (syntax-binary '- (list-ref expr 1) (list-ref expr 2) env cont))
        ('* (syntax-binary '* (list-ref expr 1) (list-ref expr 2) env cont))
        ('/ (syntax-binary '/ (list-ref expr 1) (list-ref expr 2) env cont))
        ('>= (syntax-binary '>= (list-ref expr 1) (list-ref expr 2) env cont))
        ('<= (syntax-binary '<= (list-ref expr 1) (list-ref expr 2) env cont))
        ('> (syntax-binary '> (list-ref expr 1) (list-ref expr 2) env cont))
        ('< (syntax-binary '< (list-ref expr 1) (list-ref expr 2) env cont))

        ('equal? (syntax-binary 'equal? (list-ref expr 1) (list-ref expr 2) env cont))
        ('and (syntax-binary 'and (list-ref expr 1) (list-ref expr 2) env cont))
        ('or (syntax-binary 'or (list-ref expr 1) (list-ref expr 2) env cont))
        ('not (syntax-unary 'not (list-ref expr 1) env cont))

        ('null? (syntax-unary 'null? (list-ref expr 1) env cont))
        ('pair? (syntax-unary 'pair? (list-ref expr 1) env cont))
        ('procedure? (syntax-unary 'procedure? (list-ref expr 1) env cont))
        ('boolean? (syntax-unary 'boolean? (list-ref expr 1) env cont))
        ('number? (syntax-unary 'number? (list-ref expr 1) env cont))
        ('symbol? (syntax-unary 'symbol? (list-ref expr 1) env cont))

        ('display (syntax-unary 'display (list-ref expr 1) env cont))
        ('newline (syntax-nullary 'newline env cont))
        ('import (syntax-import (list-ref expr 1) env cont))

        ((pair cons) (syntax-binary 'pair (list-ref expr 1) (list-ref expr 2) env cont))
        ((car dit) (syntax-unary 'car (list-ref expr 1) env cont))
        ((cdr dot) (syntax-unary 'cdr (list-ref expr 1) env cont))

        ('let (syntax-let (list-ref expr 1) (list-tail expr 2) env cont))
        ('c-procedure `(native ,(list-ref expr 1) ,cont))


        (else (evaluate-many (append (cdr expr) (list (car expr))) env `(call ,(length (cdr expr)) ,cont))))) 
          
    (immediate expr env cont)))

(define (params->env params)
  (let next-param ((params params) (res '()))
    (if (pair? params)
      (next-param (cdr params) (cons (cons (car params) (length res)) res))
      res)))

(define (trace . vals)
  (for-each 
    (lambda (x)
      (display x)
      (newline))
    vals))

(define (todo)
  (error "todo"))

(define (define->cdefine name)
  (string-append "d-" (symbol->cidentifier name)))

(define (env-variable-pointer env name cont)
  (let ((match (assoc name env)))
    (if match
      (if (pair? (cdr match))
        `(free ,(list-ref (car (cdr match)) 1) ,cont)
        `(pick ,(cdr match) ,cont))
      `(fetch ,name ,cont))))

(define (immediate x env cont)
  (cond
    ((number? x) `(number ,x ,cont))
    ((boolean? x) `(boolean ,x ,cont))
    ((symbol? x) (env-variable-pointer env x cont))
    (else (error "bad immediate"))))

(define (number->hex n)
  (list->string
    (reverse 
      (let loop ((n n))
        (cons 
          (list-ref '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F) (modulo n 16))
          (if (equal? (floor (/ n 16)) 0)
            '()
            (loop (floor (/ n 16)))))))))

(define (symbol->cidentifier sym)
  (string-join
    (map 
      (lambda (x)
        (let ((c (char->integer x)))
          (cond
            ((equal? 95 c)
             "__")
            ((or
               (and 
                 (>= c 65)
                 (<= c 90))
               (and
                 (>= c 97)
                 (<= c 122))
               (and
                 (>= c 48)
                 (<= c 57)))
             (list->string (list (integer->char c))))

            (else
              (let ((hexchar (number->hex c)))
                (if (> (length hexchar) 4)
                  (string-append "_U"(padleft hexchar #\0 8))
                  (string-append "_u"(padleft hexchar #\0 4))))))))

      (string->list (symbol->string sym))) 
    ""))
    

(define (lambda->label id)
  (string-append "f-" (number->string id)))

(define (symbol->cdef sym)
  (string-append "s-" (symbol->cidentifier sym)))


(define (op-to-c-call op)
  (case op
    ('+ "q+")
    ('- "q-")
    ('* "q*")
    ('/ "q/")
    ('equal? "q=")
    ('and "q-and")
    ('or "q-or")
    ('not "q-not")
    ('pair "q-pair")
    ('car "q-car")
    ('cdr "q-cdr")
    ('display "q-display")
    ('newline "newline type q-null")
    ('boolean? "q-boolean?")
    ('null? "q-null?")
    ('procedure? "q-procedure?")
    ('pair? "q-pair?")
    ('number? "q-number?")
    ('symbol? "q-symbol?")
    ('< "q<")
    ('> "q>")
    ('<= "q<=")
    ('>= "q>=")))

(define (quote-to-c x symbols) ;; TODO: remove this
  (cond
    ((pair? x) 
     (call-with-values
       (lambda () 
         (quote-to-c (car x) symbols))
       (lambda (left symbols2) 
         (call-with-values
           (lambda () (quote-to-c (cdr x) symbols2))
           (lambda (right symbols3)
             (values 
               (string-append "Q_PAIR(&((q_pair) { .head="left", .tail="right" }))")
               symbols3))))))
    ((null? x) (values "Q_NULL" symbols))
    ((symbol? x) 
     (values 
       (string-append "Q_SYMBOL("(symbol->cdef x)")") 
       (cons x symbols)))
    (else (values "" symbols))))

(define (to-c ir)
  (let next ((op ir) (code '()) (lambdas '()) (defines '()) (fetches '()) (imports '()) (symbols '()) (quotes '()) (paramcount 0))
    (if (null? op) 
      (values 
        (cons 
          (cons 
            (string-append "1 " (number->string paramcount)" shove-back")   ;; copy result to the top of parent's stackframe
            code) 
          lambdas) 
        defines 
        fetches      
        imports
        symbols
        quotes)
        

      (case (car op)
        ((lambda closure) 
         (let 
           ((paramcount2 (list-ref op 1))
            (thunk (list-ref op 2))
            (cont (list-ref op 3)))

           (call-with-values 
             (lambda () (next thunk '() lambdas defines fetches imports symbols quotes paramcount2))

             (lambda (lambdas2 defines2 fetches2 imports2 symbols2 quotes2)
               (next 
                 cont
                 (cons 
                   (string-append 
                     "['] "
                     (lambda->label (- (length lambdas2) 1))
                     " q-lambda") 
                   code) 
                 lambdas2 
                 defines2
                 fetches2
                 imports2
                 symbols2
                 quotes2
                 paramcount)))))


        ('branch 
         (let 
           ((iftrue (list-ref op 1))
            (iffalse (list-ref op 2)))

           (call-with-values 
             (lambda () (next iftrue '() lambdas defines fetches imports symbols quotes paramcount))

             (lambda (lambdas2 defines2 fetches2 imports2 symbols2 quotes2)
               (call-with-values 
                 (lambda () (next iffalse '() lambdas2 defines2 fetches2 imports2 symbols2 quotes2 paramcount))

                 (lambda (lambdas3 defines3 fetches3 imports3 symbols3 quotes3)
                   (values
                     (cons (cons 
                             (string-append 
                               "q? if " 
                               (lambda->label (- (length lambdas2) 1)) ; if true
                               " else "
                               (lambda->label (- (length lambdas3) 1)) ; if false
                               " then") 
                             code) lambdas3) 
                     defines3
                     fetches3
                     imports3
                     symbols3
                     quotes3)))))))

        ('call
         (let 
           ((argcount (list-ref op 1))
            (cont (list-ref op 2)))

           (if (null? cont) ;; at tail position?
              ;;#f
             (values
               (cons 
                 (cons 
                   "check-lambda r> drop execute" 
                   ;;"q-call-tail" 
                   (cons (string-append (number->string  (+ argcount 1))" "(number->string paramcount) " shove-back") code)) 
                 lambdas)
               defines
               fetches
               imports
               symbols
               quotes)
             
             (call-with-values 
               (lambda () (next cont '() lambdas defines fetches imports symbols quotes paramcount))

               (lambda (lambdas2 defines2 fetches2 imports2 symbols2 quotes2)
                 (values 
                   (cons 
                     (cons
                       (string-append (lambda->label (- (length lambdas2) 1))) 
                        (cons 
                       "q-call" 
                          code))
                     lambdas2) 
                   defines2
                   fetches2
                   imports2
                   symbols2
                   quotes2))))))

        ('import
         (let 
           ((module (list-ref op 1))
            (cont (list-ref op 2)))

           (call-with-values 
            (lambda () (next cont '() lambdas defines fetches imports symbols quotes paramcount))

            (lambda (lambdas2 defines2 fetches2 imports2 symbols2 quotes2)
              (values 
                (cons 
                  (cons 
                    (string-append (lambda->label (- (length lambdas2) 1)))
                    (cons
                      (string-append (symbol->cidentifier module)"-toplevel")   
                      code))
                  lambdas2) 
                defines2
                fetches2
                (cons (string-append (symbol->cidentifier module) "-toplevel") imports2)
                symbols2
                quotes2)))))

        ((+ - * / equal? pair car cdr display newline and or not boolean? null? pair? procedure? number? symbol? >= <= > <) ;; ops
         (next
           (list-ref op 1) ;; cont
           (cons (op-to-c-call (car op)) code)
           lambdas
           defines
           fetches
           imports
           symbols
           quotes
           paramcount))

        ('native
         (let 
           ((name (list-ref op 1))
            (cont (list-ref op 2)))

           (next
             (list-ref op 2) ;; cont
             (cons 
               (string-append "q_make_lambda(q, "name");") 
               code)
             lambdas
             defines
             fetches
             (cons name imports)
             symbols
             quotes
             paramcount)))

        ('define 
         (let 
           ((name (list-ref op 1))
            (cont (list-ref op 2)))

           (next 
             cont 
             (cons (string-append "2dup " (define->cdefine name) " 2!") code)
             lambdas 
             (cons name defines)
             fetches
             imports
             symbols
             quotes
             paramcount)))

        ('number
         (let
           ((x (list-ref op 1))
            (cont (list-ref op 2)))
           (next
             cont
             (cons (string-append (number->string (list-ref op 1)) " q-num") code)
             lambdas
             defines
             fetches
             imports
             symbols
             quotes
             paramcount)))
             
        ('boolean
         (let
           ((x (list-ref op 1))
            (cont (list-ref op 2)))
           (next
             cont
             (cons 
               (string-append 
                 (if x 
                   "true q-bool" 
                   "false q-bool"))
               code)
             lambdas
             defines
             fetches
             imports
             symbols
             quotes
             paramcount)))

        ('fetch
         (let 
            ((name (list-ref op 1))
             (cont (list-ref op 2)))
           (next
             cont
             (cons 
               (string-append (define->cdefine name) " 2@")
               code)
             lambdas
             defines
             (cons name fetches)
             imports
             symbols
             quotes
             paramcount)))

        ('drop
         (let 
           ((cont (list-ref op 1)))
           (next
             cont
             (cons 
               "q-drop"
               code)
             lambdas
             defines
             fetches
             imports
             symbols
             quotes
             paramcount)))

        ('push-frame
         (let 
           ((cont (list-ref op 1)))
           (next
             cont
             code
             lambdas
             defines
             fetches
             imports
             symbols
             quotes
             paramcount)))

        ('pop-frame
         (let 
           ((n (list-ref op 1))
            (cont (list-ref op 2)))
           (next
             cont
             (cons 
               (string-append "1 "(number->string n)" shove-back")
               code)
             lambdas
             defines
             fetches
             imports
             symbols
             quotes
             paramcount)))

        ('quote
          (let 
            ((value (list-ref op 1))
             (cont (list-ref op 2)))
            (let 
              ((quoted-value (list (quote-to-c value symbols))))
              (next
                  cont
                  (cons 
                     (string-append "qt-"(number->string (length quotes))" 2@") 
                     code)
                  lambdas
                  defines
                  fetches
                  imports
                  (list-ref quoted-value 1)
                  (cons value quotes)
                  paramcount))))
              
        ('free
         (let 
           ((n (list-ref op 1))
            (cont (list-ref op 2)))
          (next
            cont
            (cons 
              "Q_STORE(q, 0, temp);"
              (cons
                "Q_PUSH(q, 1);" 
                (cons 
                  (string-append "Q_ENV(q, " (number->string n) ", &temp);") 
                  code)))
            lambdas
            defines
            fetches
            imports
            symbols
            quotes
            paramcount)))

        ('pick
         (let 
            ((n (list-ref op 1))
             (cont (list-ref op 2)))
           (next
             cont
             (cons 
               (string-append (number->string n) " q-pick") 
                   code)
             lambdas
             defines
             fetches
             imports
             symbols
             quotes
             paramcount)))

        (else (error "bad operation"))))))
       
(define (delimited-list ls del)
  (if (null? ls) 
    '()
    (if (null? (cdr ls)) 
      (cons (car ls) '())
      (cons (car ls) (cons del (delimited-list (cdr ls) del))))))

(define (string-join ls del)
  (apply string-append (delimited-list ls del)))

(define (dedupe e)
  (if (null? e) '()
      (cons (car e) (dedupe (filter (lambda (x) (not (equal? x (car e)))) 
                                    (cdr e))))))

(define (stitch-lambda module id l)
  (string-append 
    (function-signature (lambda->label id))"\n"
    (string-join (reverse l) "\n")
    " ;"))

(define (stitch-lambdas module lambdas)
  (string-join
    (let loop ((lambdas (reverse lambdas)) (id 0))
         (if (null? lambdas)
          '()
           (cons (stitch-lambda module id (car lambdas)) (loop (cdr lambdas) (+ 1 id)))))
    "\n\n"))
          

(define (stitch-defines defines)
  (string-join
    (let loop ((ls defines))
       (if (null? ls)
        '()
        (cons (string-append "2variable "  (define->cdefine (car ls))) (loop (cdr ls)))))
    "\n"))


(define (stitch-imports imports)
  (string-join
    (let loop ((ls imports))
       (if (null? ls)
        '()
        (cons 
          (string-append "") 
          (loop (cdr ls)))))
    "\n"))

(define (stitch-extern-defines defines)
  (string-join
    (let loop ((ls defines))
       (if (null? ls)
        '()
        (cons "" (loop (cdr ls)))))
    "\n"))

(define (filter p ls)
  (if (pair? ls)
    (if (p (car ls))
      (cons (car ls) (filter p (cdr ls)))
      (filter p (cdr ls)))
    '()))

(define (difference a b)
   (filter (lambda (x) (not (member x b))) a))

(define (top-level-function-signature module)
  (function-signature (string-append module"-toplevel")))
  
(define (stitch-symbols symbols)
  (string-join
    (map 
      (lambda (sym)
        "")
      symbols)
    "\n"))


(define (stitch-quotes-definitions quotes)
  (string-join
    (let loop ((quotes quotes) (n 0))
      (if (null? quotes)
        '()
        (cons 
         (string-append "2variable qt-"(number->string n))
         (loop
           (cdr quotes)
           (+ n 1)))))
    "\n"))

(define (quote-init n id qt ls)
  (cond 
    ((pair? qt) 
     (call-with-values 
       (lambda () (quote-init n id (car qt) ls))
       (lambda (ls2 n2)
         (call-with-values 
           (lambda () (quote-init n2 id (cdr qt) ls2))
           (lambda (ls3 n3)
             (values
               (append
                 ls3
                 (list
                   (string-append "q-pair")
                   ))
                 
               (+ 1 n3)))))))

    ((symbol? qt)
     (values 
       (append
         ls
         (list 
          (string-append (symbol->cdef qt))))
         
       (+ n 1)))
    ((number? qt)
     (values 
       (append 
         ls
         (list (string-append (number->string qt) " q-num")))
         
       (+ n 1)))
    ((null? qt)
     (values 
       (append 
         ls
         (list "q-null"))
         
       (+ n 1)))
    ((boolean? qt)
     (values 
         (append 
           ls
           (list
             (string-append (if qt "true q-bool" "false q-bool"))))
         (+ n 1)))
    (else (error "quote not supported"))))

(define (initialize-quote id qt ls)
  (call-with-values 
    (lambda ()
      (quote-init
        0
        id
        qt
        '())) 
    (lambda (ls n)
      (append 
        ls
        (list (string-append id " 2!"))))))

(define (stitch-quotes-initialization quotes)
  (string-join
    (let loop ((quotes quotes) (n (- (length quotes) 1)))
      (if (null? quotes)
        '()
         (cons
          (string-join 
            (initialize-quote 
              (string-append "qt-"(number->string n))
              (car quotes)
              '())
           "\n")
          (loop (cdr quotes) (- n 1)))))
    "\n"))

(define (stitch-program module lambdas defines fetches imports symbols quotes debug?)
  (string-join 
    `(
      ;; ,(if debug? (string-append ": rl postpone include " module ".fs ;"))
      "\\ defines"
      ,(stitch-defines (dedupe defines))
      ""
      "\\ extern"
      ,(stitch-extern-defines (difference (dedupe fetches) defines))
      ""
      "\\ imports"
      ,(stitch-imports (dedupe imports))
      ""
      "\\ symbols"
      ,(stitch-symbols (dedupe symbols))
      ""
      "\\ quotes"
      ,(stitch-quotes-definitions quotes)
      ""
      "\\ lambdas"
      ,(stitch-lambdas module lambdas)
      ""
      "\\ toplevel"
      ,(string-append (top-level-function-signature module) "") 
      ,(stitch-quotes-initialization quotes)
      ,(string-append (lambda->label (- (length lambdas) 1)) " ;")
      )
    "\n"))

;;(begin 
;;  "tests"
;;  (trace 
;;    (syntax-lambda 
;;      '(a b c) 
;;      '((print (+ (+ a c) b)) 
;;        (print 0))
;;      '())
;;
;;    (evaluate-thunk '((1) (+ 1 2)) '() '())))

(define (read-all port)
  (let ((read-value (read port)))
    (if (eof-object? read-value)
      '()
      (cons read-value (read-all port)))))

(define (command-line)
  (let ((lst ()))
    (with-input-from-file "/proc/self/cmdline"
      (lambda ()
       (do ((c (read-char) (read-char))
            (s ""))
        ((eof-object? c)
         (reverse lst))
        (if (char=? c #\null)
         (begin
          (set! lst (cons s lst))
          (set! s ""))
         (set! s (string-append s (string c)))))))))

(define (stitch-symbol-constants symbols)
  (string-join
    (map 
      (lambda (sym)
        (string-append "s\" "(symbol->string sym)"\" q-symbol 2constant "(symbol->cdef sym)))
      symbols)
    "\n"))

(define (cli cmd module)
  (cond
    ((equal? cmd "--extract-symbols")
     (call-with-values 
      (lambda () (to-c (evaluate-thunk (read-all *stdin*) '() '())))
      (lambda (lambdas defines fetches imports symbols quotes)
        (display (stitch-symbol-constants (dedupe symbols))))))

    ((equal? cmd "--build")
     (display (stitch-program module (to-c (evaluate-thunk (read-all *stdin*) '() '())) #t)))
    
    ((equal? cmd "--build-tree")
     (display (evaluate-thunk (read-all *stdin*) '() '()))
     (newline))))

(let ((args (list-tail (command-line) 2)))
  (if (not (zero? (length args)))
    (apply cli args))
    )

