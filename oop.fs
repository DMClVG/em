\ defines
variable d-john
variable d-person

\ extern


\ imports


\ symbols




\ quotes
variable qt-0
variable qt-1
variable qt-2
variable qt-3
variable qt-4

\ lambdas
: f-0
0 0 q-upvalue
1 1 shove-back ;

: f-1
qt-1 @
1 1 shove-back ;

: f-2
0 q-pick
qt-0 @
q-eq?
q? r> drop if f-0 else f-1 then ;

: f-3
['] f-2 0 q-closure
1 3 shove-back ;

: f-4
q-display
drop
q-newline
1 0 shove-back ;

: f-5
dup d-john !
drop
qt-4 @
d-john @
q-call
f-4 ;

: f-6
['] f-3 0 q-closure
dup d-person !
drop
qt-2 @
23 q-number
qt-3 @
d-person @
q-call
f-5 ;

\ toplevel
: oop-toplevel
s-name
qt-4 !
s-joyful
qt-3 !
s-john
qt-2 !
q-null
qt-1 !
s-name
qt-0 !
f-6 ;