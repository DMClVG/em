\ include debug.fs

1 constant Q-PAIR-T
2 constant Q-NUM-T
3 constant Q-NULL-T
4 constant Q-SYMBOL-T
5 constant Q-LAMBDA-T
6 constant Q-BOOL-T
7 constant Q-CLOSURE-T

rp@ constant rbase

: q-unbox drop ; \ drop tag 
: q-tag nip ;

: rdrop r> drop ;
: rdepth rbase rp@ - ; 

: q-num Q-NUM-T ;
: q-null 0 Q-NULL-T ;
: q-lambda ( f -- l tag )
  Q-LAMBDA-T
;

: closure-size 2 * 1 + ;

: closure-allot closure-size cells allot ;
: closure-upvalue ( c n ) closure-size cells + 2@ ;
: closure-xt ( c ) @ ;
: closure-upvalues cell + ;
: q-upvalue 2 * 1 + pick swap 2 cells * + 2@ ;

variable closure-ptr
variable closure-size-cells

: q-closure ( qqq f n -- c tag )
  here >r
  closure-size cells dup closure-size-cells ! 
  allot r> closure-ptr ! 

  sp@
  closure-ptr @ 
  closure-size-cells @
  move 

  sp@ closure-size-cells @ + sp!
  closure-ptr @ Q-CLOSURE-T
;

: q-bool Q-BOOL-T ;

: q-symbol ( s # -- s tag )
  here 2 cells allot >r   
  r@ 0 cells + !
  r@ 1 cells + !
  r> Q-SYMBOL-T
;

: symbol-string 2@ ;

: q-pair ( a t b t -- p tag  )
  here 4 cells allot >r
  r@ 2 cells + 2! 
  r@ 0 cells + 2! 
  r> Q-PAIR-T
;


: check-pair Q-PAIR-T <> if ." is not a pair!" abort then ;
: check-num Q-NUM-T <> if ." is not a number!" abort then ;
: check-bool Q-BOOL-T <> if ." is not a boolean!" abort then ;
: check-lambda Q-CLOSURE-T <> if ." is not a lambda!" abort then ;


: setup-closure 
  check-lambda 
  dup closure-xt >r
  closure-upvalues r> 
  ;

: car 2@ ;
: cdr 2 cells + 2@ ;

: q-drop 2drop ;
: q-pick 2 * dup 2 + pick >r 2 + pick r> ;
: q-call setup-closure execute ;

: setup-2num ( a b -- n m )
check-num >r check-num r> ;

: setup-2bool ( a b -- n m )
check-bool >r check-bool r> ;

: q? check-bool ;

: q+ setup-2num + q-num ;
: q- setup-2num - q-num ;
: q* setup-2num * q-num ;
: q/ setup-2num / q-num ;
: q< setup-2num < q-bool ;
: q> setup-2num > q-bool ;
: q>= setup-2num >= q-bool ;
: q<= setup-2num <= q-bool ;

: q= rot = -rot = and q-bool ;    

: q-and setup-2bool and q-bool ;
: q-or setup-2bool or q-bool ;
: q-not check-bool 0= q-bool ;

: q-oftype? = nip ;

: q-bool? Q-BOOL-T q-oftype? q-bool ;
: q-null? Q-NULL-T  q-oftype? q-bool ;
: q-procedure? Q-LAMBDA-T q-oftype? q-bool ;
: q-pair? Q-PAIR-T q-oftype? q-bool ;
: q-number? Q-NUM-T q-oftype? q-bool ;
: q-symbol? Q-BOOL-T q-oftype? q-bool ;

variable argnum 
variable paramnum 
variable stackp

: reorder-stack
  2 cells * paramnum ! 2 cells * argnum !

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

: q-call-tail postpone check-lambda postpone r> postpone drop postpone execute ; immediate

: display-num 0 <# #s #> type ;

defer display*

: display-pair 
dup 
car display* 
cdr dup Q-PAIR-T = if 
  drop space recurse 
else
  dup Q-NULL-T <> if 
  ."  . " display*
  else q-drop then
then
;

: display-symbol symbol-string type ;
: display-null drop ." ()" ;
: display-bool if ." #t" else ." #f" then ;

: display 
case 
Q-PAIR-T of ." (" display-pair ." )" endof
Q-NUM-T of display-num endof
Q-NULL-T of display-null endof
Q-SYMBOL-T of display-symbol endof
Q-BOOL-T of display-bool endof
endcase
; 

' display is display*

: q-display 2dup display ;
: q-car check-pair car ;
: q-cdr check-pair cdr ;

