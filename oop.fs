\ defines
variable d-melissa
: melissa d-melissa @ ;
variable d-john
: john d-john @ ;
variable d-person
: person d-person @ ;
variable d-person_u002Dbehavior
: person-behavior d-person_u002Dbehavior @ ;
variable d-info
: info d-info @ ;

\ extern



\ imports


\ symbols





\ quotes
variable qt-0
variable qt-1
variable qt-2
variable qt-3
variable qt-4
variable qt-5
variable qt-6
variable qt-7
variable qt-8
variable qt-9
variable qt-10
variable qt-11
variable qt-12
variable qt-13
variable qt-14
variable qt-15

\ lambdas
: f-0
q-display
drop
q-newline
1 1 shove-back ;

: f-1
q-display
drop
q-newline
drop
qt-4 @
q-display
drop
qt-5 @
1 pick
execute
f-0 ;

: f-2
number->string
execute
f-1 ;

: f-3
q-display
drop
q-newline
drop
qt-2 @
q-display
drop
qt-3 @
1 pick
execute
f-2 ;

: f-4
qt-0 @
q-display
drop
qt-1 @
1 pick
execute
f-3 ;

: f-5
0 pick
q-car
1 2 shove-back ;

: f-6
0 pick
q-cdr
q-car
1 2 shove-back ;

: f-7
0 pick
q-cdr
q-cdr
q-car
1 2 shove-back ;

: f-8
info
2 2 shove-back
execute ;

: f-9
0 pick
person-behavior
object
execute
f-8 ;

: f-10
qt-10 @
1 2 shove-back ;

: f-11
1 pick
qt-9 @
q-eq?
if f-9 else f-10 then ;

: f-12
1 pick
qt-8 @
q-eq?
if f-7 else f-11 then ;

: f-13
1 pick
qt-7 @
q-eq?
if f-6 else f-12 then ;

: f-14
1 pick
qt-6 @
q-eq?
if f-5 else f-13 then ;

: f-15
['] f-14 q-lambda
dup d-person_u002Dbehavior !
drop
2 pick
2 pick
2 pick
qt-11 @
q-pair
q-pair
q-pair
person-behavior
object
3 3 shove-back
execute ;

: f-16
dup d-melissa !
1 0 shove-back ;

: f-17
dup d-john !
drop
qt-14 @
27 q-number
qt-15 @
person
execute
f-16 ;

: f-18
['] f-4 q-lambda
dup d-info !
drop
['] f-15 q-lambda
dup d-person !
drop
qt-12 @
23 q-number
qt-13 @
person
execute
f-17 ;

\ toplevel
: oop-toplevel
s" ヾ(@^▽^@)ノ" q-string
qt-15 !
s" Melissa" q-string
qt-14 !
s" (¬_¬)ﾉ" q-string
qt-13 !
s" John" q-string
qt-12 !
q-null
qt-11 !
q-null
qt-10 !
s-info
qt-9 !
s-personality
qt-8 !
s-age
qt-7 !
s-name
qt-6 !
s-personality
qt-5 !
s" Personality: " q-string
qt-4 !
s-age
qt-3 !
s" Age: " q-string
qt-2 !
s-name
qt-1 !
s" Name: " q-string
qt-0 !
f-18 ;