\ include debug.fs

1 constant Q-PAIR-T
2 constant Q-NUMBER-T
3 constant Q-NULL-T
4 constant Q-SYMBOL-T
5 constant Q-LAMBDA-T
6 constant Q-BOOL-T
7 constant Q-OBJECT-T
8 constant Q-STRING-T

rp@ constant rbase

: q-unbox 2@ ;
: q-data cell + @ ;
: q-tag @ ;

: rdepth rbase rp@ - ;

: q-value ( data tag -- value )
  here 2 cells allot dup >r 2! r>
;

: q-number Q-NUMBER-T q-value ;
: q-null 0 Q-NULL-T q-value ;
: q-lambda ( f -- value )
  Q-LAMBDA-T q-value
;
: q-string ( s # -- value )
  here 2 cells allot >r
  r@ 0 cells + !
  r@ 1 cells + !
  r> Q-STRING-T q-value
  ;

: q-bool Q-BOOL-T q-value ;

: q-symbol ( s # -- s tag )
  here 2 cells allot >r
  r@ 0 cells + !
  r@ 1 cells + !
  r> Q-SYMBOL-T q-value
;

: q-pair ( a b -- p )
  here 2 cells allot >r
  r@ 1 cells + !
  r@ 0 cells + !
  r> Q-PAIR-T q-value
;

: symbol-string 2@ ;

: check-pair q-unbox Q-PAIR-T <> if ." is not a pair!" abort then ;
: check-num q-unbox Q-NUMBER-T <> if ." is not a number!" abort then ;
: check-bool q-unbox Q-BOOL-T <> if ." is not a boolean!" abort then ;
: check-lambda q-unbox Q-LAMBDA-T <> if ." is not a lambda!" abort then ;
: check-string q-unbox Q-STRING-T <> if ." is not a string!" abort then ;

: car @ ;
: cdr cell + @ ;

: setup-2num ( a b -- n m )
check-num >r check-num r> ;

: setup-2bool ( a b -- n m )
check-bool >r check-bool r> ;

: q? check-bool ;

: q+ setup-2num + q-number ;
: q- setup-2num - q-number ;
: q* setup-2num * q-number ;
: q/ setup-2num / q-number ;
: q< setup-2num < q-bool ;
: q> setup-2num > q-bool ;
: q>= setup-2num >= q-bool ;
: q<= setup-2num <= q-bool ;
: q= setup-2num = q-bool ;

: q-eq? >r q-unbox r> q-unbox rot = -rot = and q-bool ;

: q-and setup-2bool and q-bool ;
: q-or setup-2bool or q-bool ;
: q-not check-bool 0= q-bool ;

: q-oftype? = ;

: q-bool? q-tag Q-BOOL-T = q-bool ;
: q-null? q-tag Q-NULL-T = q-bool ;
: q-procedure? q-tag Q-LAMBDA-T = q-bool ;
: q-pair? q-tag Q-PAIR-T = q-bool ;
: q-number? q-tag Q-NUMBER-T = q-bool ;
: q-symbol? q-tag Q-BOOL-T = q-bool ;

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


defer display*

: display-pair
dup
car display*
cdr dup q-tag Q-PAIR-T = if
  q-data space recurse
else
  dup q-tag Q-NULL-T <> if
  ."  . " display*
  else drop then
then
;

: display-num 0 <# #s #> type ;
: display-symbol symbol-string type ;
: display-string symbol-string type ;
: display-null drop ." ()" ;
: display-bool if ." #t" else ." #f" then ;

: display-lambda drop ." <procedure>" ;

: display
q-unbox
case
Q-PAIR-T of ." (" display-pair ." )" endof
Q-NUMBER-T of display-num endof
Q-NULL-T of display-null endof
Q-SYMBOL-T of display-symbol endof
Q-BOOL-T of display-bool endof
Q-LAMBDA-T of display-lambda endof
Q-STRING-T of display-string endof
endcase
;

' display is display*

: q-display display q-null ;
: q-newline newline type q-null ;
: q-car check-pair car ;
: q-cdr check-pair cdr ;

: q-dbg dup q-unbox swap . . ;




variable object-f
variable object-d
variable #object-slots
variable object-slots

defer wut

: allot-slots here #object-slots @ cells allot ;

: create-self ( n -- self )
  noname
  create
  cells allot
  does>
  swap cells + @
 ;

: make-self create-self latestxt ;

: make-object
  noname create
  object-f @ ,
  object-d @ ,

  does>
  2@ execute
;

: q-object ( d f -- value )
  check-lambda object-f !
  object-d !

  make-object
  latestxt
  q-lambda
;

: object ['] q-object q-lambda ;
