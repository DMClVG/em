\ include debug.fs

1 constant Q-PAIR-T
2 constant Q-NUMBER-T
3 constant Q-NULL-T
4 constant Q-SYMBOL-T
5 constant Q-LAMBDA-T
6 constant Q-BOOL-T
7 constant Q-CLOSURE-T

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
: q-lambda ( f -- l tag )
  Q-LAMBDA-T q-value
;


\ : closure-allot closure-size cells allot ;
\ : closure-upvalue ( c n ) 1+ cells + @ ;
: closure-xt ( c ) @ ;
: closure-upvalues cell + ;
: q-upvalue ( idx delta -- up )
  1 + pick swap cells + @ ;

variable closure-ptr
variable closure-size-cells

: q-closure ( qqq f n -- c tag )
  here >r
  1+ cells dup closure-size-cells !
  allot r> closure-ptr !

  sp@
  closure-ptr @
  closure-size-cells @
  move

  sp@ closure-size-cells @ + sp!
  closure-ptr @ Q-CLOSURE-T q-value
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
: check-lambda q-unbox Q-CLOSURE-T <> if ." is not a lambda!" abort then ;


: setup-closure
  check-lambda
  dup closure-xt >r
  closure-upvalues r>
  ;

: car @ ;
: cdr cell + @ ;

: q-drop drop ;
: q-pick 1+ pick ; \ skip upvalue pointer
: q-call setup-closure execute ;

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
  stackp @ paramnum @ + cell + \ dest
  argnum @
  move
;

: update-stack-pointer
  stackp @ paramnum @ + cell + sp! \ update pointer
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
: display-null drop ." ()" ;
: display-bool if ." #t" else ." #f" then ;

: display
q-unbox
case
Q-PAIR-T of ." (" display-pair ." )" endof
Q-NUMBER-T of display-num endof
Q-NULL-T of display-null endof
Q-SYMBOL-T of display-symbol endof
Q-BOOL-T of display-bool endof
endcase
;

' display is display*

: q-display display q-null ;
: q-newline newline type q-null ;
: q-car check-pair car ;
: q-cdr check-pair cdr ;

: q-dbg dup q-unbox swap . . ;
