(use-modules 
  (srfi srfi-1)
  (srfi srfi-9))

(define* (params->env params #:optional idx)
  (if (pair? params)
    (cons (cons (car params) (if idx idx 0)) 
          (params->env (cdr params) (if idx (+ 1 idx) 1)))
    '()))

(define (name->index expr env)
  (unless env (raise-exception 'undefined-name))
  (cond
    ((assoc expr env) => cdr)
    (else (raise-exception 'undefined-name))))

(define* (offset-env env offset)
  (map (lambda (x) (if x (cons (car x) (+ offset (cdr x))) #f)) env))

(define* (resolve-args args env lambdas code)
  (if (pair? args)

    (let ((arg (car args))
          (args (cdr args)))

      (cond 
       ((number? arg) 
        (resolve-args args (offset-env env 0) lambdas (cons arg code)))

       ((symbol? arg) 
        (resolve-args args (offset-env env 0) lambdas (cons 'pick (cons (name->index arg env) code))))

       ((pair? arg) 
        (let ((new-lambdas (make-lambda (car arg) (cdr arg) lambdas)))
           (resolve-args args (offset-env env 0) new-lambdas (cons (length new-lambdas) code)))) 

       (else (raise-exception 'bad-argument))))

    (cons (reverse code) lambdas)))

(define* (make-lambda params body lambdas)
  (eval-expr body (params->env params) lambdas))

(define (op-with-args op args env lambdas)
  (let ((new-lambdas (resolve-args args env lambdas '())))
    (cons (append (car new-lambdas) (list op)) (cdr new-lambdas))))
   
(define (eval-expr expr env lambdas) 
  (let ((args (reverse (cdr expr))))
    (case (car expr)
      ('apply
       (resolve-args args env lambdas '()))
      ('print 
       (op-with-args 'print args env lambdas))
      ('equal? 
       (op-with-args 'equal? args env lambdas))
      ('if 
       (op-with-args 'branch args env lambdas))
      ('+ 
       (op-with-args 'add args env lambdas))
      (else (raise-exception `(bad-expr ,expr)))))) 
      

(define (trace x)
  (display x)
  (newline)
  x)

(define-record-type <env>
  (make-env names lambdas)
  env?
  (lambdas env/lambdas)
  (names env/names))


;; (eval-args-and-exec '((print 0)) 'print)
(define (execute codepoint program state local)
 ;; (trace codepoint)

  (cond 
    ((number? codepoint) (cons codepoint local))
    ((symbol? codepoint)

     (case codepoint
       ('branch
        (cons (if (not (equal? (list-ref local 2) 0)) (list-ref local 0) (list-ref local 1)) (list-tail local 3)))

       ('print 
        (display (list-ref local 1))
        (newline)
        (cons (list-ref local 0) (cons 'nil (list-tail local 2))))

       ('add 
        (cons (list-ref local 0) (cons (+ (list-ref local 1) (list-ref local 2)) (list-tail local 3))))

       ('equal? 
        (cons (list-ref local 0) (cons (if (equal? (list-ref local 1) (list-ref local 2)) 1 0) (list-tail local 3))))

       ('pick 
        (cons (list-ref state (car local)) (cdr local)))

       (else (raise-exception `(bad-code ,codepoint)))))
    (else (raise-exception `(bad-code ,codepoint)))))
 

(define (interpret proc program state local)
 ;; (trace state)
 ;; (trace local)
  (if (pair? proc)

    (interpret (cdr proc) program state (execute (car proc) program state local))

    (let ((lambda-id (list-ref local 0)))
      ;; (trace `("entering" ,lambda-id))
      ;; (trace local)
      (when (not (equal? lambda-id 0))
        (interpret (list-ref program (- lambda-id 1)) program (cdr local) '()))))) 
        

;;(define code 
;;  (make-lambda 
;;   '() 
;;   '(apply ((k x) . (print ((a) . (print ((b) . (print 0 1)) a)) x)) 0 1)
;;    (make-env '() '())))
;;
;; (define program (reverse (make-lambda '() '(apply ((a b c) . (apply ((a b) . (+ ((x) . (print 0 x)) a b)) a b)) 3 2 1) '()))) 
;;(equal? ((x c k a b) . (if ((c k a b) . (apply k b)) ((c k a b) . (+ ((n c k a) . (apply c k a n)) b 1 c k a)) x c k a b)) a 0 c k a b)

(define (let-extract-values expr)
  (map identity (map (lambda (x) (car (cdr x))) (car (cdr expr)))))

(define (lambda-extract-params expr)
  (car (cdr expr)))

(define (let-extract-names expr)
  (map identity (map car (car (cdr expr)))))

(define (translate expr cont)
  (cond
    (pair? expr 
      (case (car expr)
        ;;('let `((apply (,(cons 'cont (let-extract-names expr) . (translate (list-ref expr 2) ) (cons cont ,(let-extract-values expr)))))))
        
        ;;('lambda `(,(lambda-extract-params expr) . ()))
        ('if '())
        (else (raise-exception 'badbadbad))))
    (number? expr)))

(define code2
  '((let ((c (lambda (c a b) (if (equal? a 0) (print a) (c (- a 1) (+ b 1))))))
      (c c 0 0))))

(define code3 '((lambda (x y) (print x) (print y)) 12 12211))

(define code 
 '(apply 
   ((c k a b) . (apply c c k a b)) 

   ((c k a b) . (equal? ((x c k a b) . (if ((c k a b) . (apply k b)) ((c k a b) . (+ ((n c k a) . (+ ((m c k n) . (apply c c k m n)) a -1 c k n)) b 1 c k a)) x c k a b)) a 0 c k a b)) 

   ((x) . (print 0 x)) 
   121200
   3112))

(define program (reverse (make-lambda '() code '()))) 
(define start (list-ref program (+ -1 (length program))))
;;(trace program) a

;(translate code3 '())


(define (resolve-variables expr env)
  (trace env)
  (cond
    ((number? expr) `(quote ,expr))
    ((pair? expr)
     (if (list? (car expr))
       (resolve-variables (cdr expr) (params->env (car expr)))
       (cons (car expr) (map (lambda (x) (resolve-variables x env)) (cdr expr)))))
    ((assoc expr env) => (lambda (x) (cdr x)))
    (else expr)))

code
(resolve-variables code '())

;; (interpret start program '() '())
(let-extract-values '(let ((a 0) (b 1) (c 2))))
(let-extract-names '(let ((a 0) (b 1) (c 2))))
(lambda-extract-params '(lambda (a b c)))

;;(trace code)
;;(trace compiled-lambdas)
;;(interpret (reverse code) '() (map reverse (car compiled-lambdas))) 

;;(define (make-lambda params body)
;;  (let ((buffer (list)))
;;    (for-each compile ast buffer)))

;;(display (compile (read (open-input-file "file.scm"))))
;;(newline)
