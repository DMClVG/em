\ include debug.fs

rp@ constant rbase

: rdepth rbase rp@ - ;

: q-number ;
: q-null 0 ;
: q-lambda ( f -- value )
;
: q-string ( s # -- value )
  here 2 cells allot >r
  r@ 0 cells + !
  r@ 1 cells + !
  r>
  ;

: q-bool ;

: q-symbol ( s # -- s tag )
  here 2 cells allot >r
  r@ 0 cells + !
  r@ 1 cells + !
  r>
;

: q-pair ( a b -- p )
  here 2 cells allot >r
  r@ 1 cells + !
  r@ 0 cells + !
  r>
;

: symbol-string 2@ ;

: car @ ;
: cdr cell + @ ;

: q+ + ;
: q- - ;
: q* * ;
: q/ / ;
: q< < ;
: q> > ;
: q>= >= ;
: q<= <= ;
: q= = ;

: q-eq? = ;

: q-and and ;
: q-or or ;
: q-not not ;

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

: shove-back ( n d -- ) \ for tail call
  reorder-stack
  update-stack-pointer
;

: display 0 <# #s #>

: q-display display 0 ;
: q-newline newline type 0 ;
: q-car car ;
: q-cdr cdr ;

: q-dbg dup . ;

variable object-f
variable object-d

: make-object
  noname create
  object-f @ ,
  object-d @ ,

  does>
  2@ execute
;

: q-object ( d f -- value )
  object-f !
  object-d !

  make-object
  latestxt
;

: object ['] q-object ;
