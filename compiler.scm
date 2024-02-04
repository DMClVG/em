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
        (resolve-args args (offset-env env 1) lambdas (cons arg code)))

       ((symbol? arg) 
        (resolve-args args (offset-env env 1) lambdas (cons 'pick (cons (name->index arg env) code))))

       ((pair? arg) 
        (let ((new-lambdas (make-lambda (car arg) (cdr arg) lambdas)))
           (resolve-args args (offset-env env 1) new-lambdas (cons (length new-lambdas) code)))) 

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
      ('+ 
       (op-with-args 'add args env lambdas))
      (else 'bad-expr)))) 
      

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
  (trace state)
  (trace local)
  (trace codepoint)

  (cond 
    ((number? codepoint) (cons codepoint local))
    ((symbol? codepoint)

     (case codepoint
       ('print 
        (display (list-ref local 1))
        (newline)
        (cons (list-ref local 0) (cons 'nil (list-tail local 2))))

       ('add 
        (cons (list-ref local 0) (cons (+ (list-ref local 1) (list-ref local 2)) (list-tail local 3))))

       ('pick 
        (cons (list-ref state (car local)) (cdr local)))

       (else (raise-exception `(bad-code ,codepoint)))))
    (else (raise-exception `(bad-code ,codepoint)))))
 

(define (interpret proc program state local)
  (if (pair? proc)

    (interpret (cdr proc) program state (execute (car proc) program state local))

    (let ((lambda-id (list-ref local 0)))
      (trace `("entering" ,lambda-id))
      (when (not (equal? lambda-id 0))
        (interpret (list-ref program (- lambda-id 1)) program (cdr local) '()))))) 
        

;;(define code 
;;  (make-lambda 
;;   '() 
;;   '(apply ((k x) . (print ((a) . (print ((b) . (print 0 1)) a)) x)) 0 1)
;;    (make-env '() '())))
;;
;; (define program (reverse (make-lambda '() '(apply ((a b c) . (apply ((a b) . (+ ((x) . (print 0 x)) a b)) a b)) 3 2 1) '()))) 
(define program (reverse (make-lambda '() '(apply ((a) . (apply a a)) ((x) . (apply x x))) '()))) 
(define start (list-ref program (+ -1 (length program))))
(display program)

;; (interpret start program '() '())

;;(trace code)
;;(trace compiled-lambdas)
;;(interpret (reverse code) '() (map reverse (car compiled-lambdas))) 

;;(define (make-lambda params body)
;;  (let ((buffer (list)))
;;    (for-each compile ast buffer)))

;;(display (compile (read (open-input-file "file.scm"))))
;;(newline)
