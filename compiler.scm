(use-modules 
  (srfi srfi-1)
  (srfi srfi-9))

(define +OP-PRINT+ 3)
(define +OP-ADD+ 4)
(define +OP-CALL+ 4)

(define* (eval-args args env lambdas)
  (fold (lambda (x prev) (append (eval-expr x env lambdas) prev)) '() (reverse args)))

(define* (offset-env env offset)
  (map (lambda (x) (if x (cons (car x) (+ offset (cdr x))) #f)) env))

(define* (resolve-args args env lambdas)
  (if (pair? args)
    (let ((arg (car args))
          (args (cdr args)))
      (cond 
       ((number? arg) (append (resolve-args args (offset-env env 1) lambdas) (list arg)))
       ((symbol? arg) (append (resolve-args args (offset-env env 1) lambdas) `(pick ,(name->index arg env))))
       ((pair? arg) (append (resolve-args args (offset-env env 1) lambdas) (eval-expr arg env lambdas)))
       (else (raise-exception 'bad-argument))))
    '()))

(define* (exec-with-args args env lambdas op #:key min-args max-args)
  (cons op (resolve-args (reverse args) env lambdas)))

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

(define* (make-lambda params body lambdas)
  (let ((id (length (car lambdas))))
    (set-car! lambdas (append (car lambdas) (list (eval-expr body (params->env params) lambdas))))
    (list id)))

(define (trace x)
  (display x)
  (newline)
  x)

(define-record-type <env>
  (make-env names lambdas)
  env?
  (lambdas env/lambdas)
  (names env/names))

(define (eval-expr expr env) 
  (cond 
   ((pair? expr)
    (case (car expr)
      ('lambda 

       (cond 
         ((equal? (length expr) 1) (raise-exception 'missing-parameters))
         ((equal? (length expr) 2) (raise-exception 'missing-body)))
        
       (let ((params (list-ref expr 1))
             (body (list-ref expr 2)))
         (make-lambda params body lambdas)))

      ('print 
       (exec-with-args (cdr expr) env lambdas 'print #:min-args 1))

      ('+ 
       (exec-with-args (cdr expr) env lambdas 'add #:min-args 0)) 

      (else 
        (append
         ;; '(call-lambda)
          (eval-expr (car expr) env lambdas)
          (resolve-args (reverse (cdr expr)) env lambdas)))))

;;   ((symbol? expr) 
;;    (trace env)
;;    (unless (assoc expr env))
;;    (list 'pick (name->index expr env)))

   ((number? expr)
    (list expr))))

;; (eval-args-and-exec '((print 0)) 'print)
(define (execute codepoint state lambdas)
  (trace state)
  (trace codepoint)

  (cond 
    ((number? codepoint) (cons codepoint state))
    ((symbol? codepoint)
     (case codepoint
        
       ('print 
        (display (list-ref state 1))
        (newline)
        (cons (list-ref state 0) (cons 'nil (cdr state))))

       ('pick 
        (cons (list-ref (cdr state) (car state)) (cdr state)))

       (else (raise-exception 'bad-code))))
    (else (raise-exception 'bad-code))))
 

(define (interpret codepoints state lambdas)
  (if (pair? codepoints)

    (interpret 
      (cdr codepoints) 
      (execute (car codepoints) state lambdas) 
      lambdas)

    (let ((lambda-id (list-ref state 0)))
      (trace `("entering" ,lambda-id))
      (when (not (equal? lambda-id 0))
        (interpret (list-ref lambdas lambda-id) (cdr state) lambdas))))) 
        

(define compiled-lambdas (cons '((exit)) '()))
(define code 
  (eval-expr '(apply (lambda (k x) (print (lambda (a) (print (lambda (b) (print 0 1)) a)) x)) 0 1) '() compiled-lambdas))

(trace code)
(trace compiled-lambdas)
;;(interpret (reverse code) '() (map reverse (car compiled-lambdas))) 

;;(define (make-lambda params body)
;;  (let ((buffer (list)))
;;    (for-each compile ast buffer)))

;;(display (compile (read (open-input-file "file.scm"))))
;;(newline)
