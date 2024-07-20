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
variable qt-5

\ lambdas
: f-0
0 0 q-upvalue
1 1 shove-back ;

: f-1
0 0 q-upvalue
1 1 shove-back ;

: f-2
0 0 q-upvalue
1 1 shove-back ;

: f-3
false q-bool
1 1 shove-back ;

: f-4
0 q-pick
qt-2 @
q-eq?
q? r> drop if f-2 else f-3 then ;

: f-5
0 q-pick
qt-1 @
q-eq?
q? r> drop if f-1 else f-4 then ;

: f-6
0 q-pick
qt-0 @
q-eq?
q? r> drop if f-0 else f-5 then ;

: f-7
['] f-6 0 q-closure
1 3 shove-back ;

: f-8
dup d-john !
drop
qt-5 @
d-john @
2 0 shove-back
setup-closure r> drop execute ;

: f-9
['] f-7 0 q-closure
dup d-person !
drop
qt-3 @
23 q-number
qt-4 @
d-person @
q-call
f-8 ;

\ toplevel
: oop-toplevel
s-name
qt-5 !
s-joyful
qt-4 !
s-john
qt-3 !
s-personality
qt-2 !
s-age
qt-1 !
s-name
qt-0 !
f-9 ;