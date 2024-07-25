\ Q runtime environment

0 constant q-null

: q-string ( s # -- st ) 2, here 2 cells - ;
: q-symbol ( s # -- sy ) 2, here 2 cells - ;
: q-cons ( a b -- cons ) 2, here 2 cells - ;

: q-create-object
  noname create 2,
  does> 2@ execute
;
: q-object ( d f -- value )
  q-create-object
  latestxt
;

: q-car cell + @ ;
: q-cdr @ ;

\ predefined
:noname , here cell - ; constant box
:noname swap ! 0 ; constant set!
:noname @ ; constant ref
:noname 0 <# #s #> q-string ; constant number->string
:noname 2@ type q-null ; constant display
:noname newline type q-null ; constant newline
:noname = ; constant eq?
' q-cons constant cons
' q-car constant car
' q-cdr constant cdr
' q-object constant object
' noop constant identity
' noop constant symbol->string

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
