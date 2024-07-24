\ defines
variable d-info
: info d-info @ ;
variable d-john
: john d-john @ ;
variable d-person
: person d-person @ ;
variable d-person_u002Dbehavior
: person-behavior d-person_u002Dbehavior @ ;

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

\ lambdas
: f-0
0 pick
q-car
1 2 shove-back ;

: f-1
0 pick
q-cdr
q-car
1 2 shove-back ;

: f-2
0 pick
q-cdr
q-cdr
q-car
1 2 shove-back ;

: f-3
qt-3 @
1 2 shove-back ;

: f-4
1 pick
qt-2 @
q-eq?
q? if f-2 else f-3 then ;

: f-5
1 pick
qt-1 @
q-eq?
q? if f-1 else f-4 then ;

: f-6
1 pick
qt-0 @
q-eq?
q? if f-0 else f-5 then ;

: f-7
['] f-6 q-lambda
dup d-person_u002Dbehavior !
drop
2 pick
2 pick
2 pick
qt-4 @
q-pair
q-pair
q-pair
person-behavior
object
3 3 shove-back
check-lambda execute ;

: f-8
q-display
drop
q-newline
1 1 shove-back ;

: f-9
q-display
drop
q-newline
drop
qt-11 @
q-display
drop
qt-12 @
1 pick
check-lambda execute
f-8 ;

: f-10
q-display
drop
q-newline
drop
qt-9 @
q-display
drop
qt-10 @
1 pick
check-lambda execute
f-9 ;

: f-11
qt-7 @
q-display
drop
qt-8 @
john
check-lambda execute
f-10 ;

: f-12
dup d-john !
drop
['] f-11 q-lambda
dup d-info !
1 0 shove-back ;

: f-13
['] f-7 q-lambda
dup d-person !
drop
qt-5 @
23 q-number
qt-6 @
person
check-lambda execute
f-12 ;

\ toplevel
: oop-toplevel
s-personality
qt-12 !
s" Personality: " q-string
qt-11 !
s-age
qt-10 !
s" Age: " q-string
qt-9 !
s-name
qt-8 !
s" Name: " q-string
qt-7 !
s" (¬_¬)ﾉ" q-string
qt-6 !
s-john
qt-5 !
q-null
qt-4 !
q-null
qt-3 !
s-personality
qt-2 !
s-age
qt-1 !
s-name
qt-0 !
f-13 ;