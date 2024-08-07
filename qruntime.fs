\ Q runtime environment

variable this-module-old-current
variable this-module

: go execute ;

: module ( "name" -- )

  get-current this-module-old-current !

  vocabulary \ create vocabulary "name"

  also
  latestxt execute

  definitions \ switch current to "name"
;

: provide
  this-module-old-current @ set-current ;

: end-module
  previous
  0 this-module !
  0 this-module-old-current !
;

0 constant q-null

: 2alloc, 2 cells alloc throw dup >r 2! r> ;
: 1alloc, 1 cells alloc throw dup >r ! r> ;
: 2allot, 2, here 2 cells - ;

: q-string ( s # -- st ) 2allot, ;
: q-symbol ( s # -- sy ) 2allot, ;
: q-cons ( a b -- cons ) 2alloc, ;

: q-car cell + @ ;
: q-cdr @ ;

: q-unbox 2@ ;

\ predefined
\ :noname 1 alloc throw dup >r ! r> ; constant box
:noname 1alloc, ; constant box
:noname swap ! 0 ; constant set!
:noname @ ; constant ref
:noname 0 <# #s #> q-string ; constant number->string
:noname 2@ type q-null ; constant display
:noname newline type q-null ; constant newline
:noname = ; constant eq?
' q-cons constant cons
' q-car constant car
' q-cdr constant cdr
' noop constant identity
' noop constant symbol->string
:noname ( addr offset x -- nil )
  -rot cells + ! 0
; constant offset-set!
:noname ( addr offset -- x )
  cells + @
; constant offset-ref
:noname ( n -- addr )
  cells alloc throw
; constant make-memory

\ stack trickery
variable argnum
variable paramnum
variable stackp

: reorder-stack
  cells paramnum ! cells argnum !

  sp@ stackp !

  stackp @ \ src
  stackp @ paramnum @ + \ dest
  argnum @
  move
;

: update-stack-pointer
  stackp @ paramnum @ + sp! \ update pointer
;

: shove-back ( n d -- )
\ for moving back n values by d cells on stack
  reorder-stack
  update-stack-pointer
;
