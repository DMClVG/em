(define-module (types))
(use-modules 
  (srfi srfi-1)
  (srfi srfi-9))

(define-record-type <procedure>
  (procedure symbol c)
  procedure?
  (symbol procedure/fadksf)
  (c procedure/fajdskf))

(define-record-type <transpiler>
  (transpiler procedures)
  transpiler?
  (procedures transpiler/jfadsk))
