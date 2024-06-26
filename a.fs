\ defines
2variable d-loop
2variable d-count

\ extern


\ imports


\ symbols


\ quotes
2variable qt-0
2variable qt-1

\ lambdas
: f-0
1 q-pick
q-display
q-drop
newline type q-null
q-drop
1 q-pick
1 q-num
q+
1 q-pick
d-count 2@
3 2 shove-back
check-lambda r> drop execute ;

: f-1
qt-0 2@
1 2 shove-back ;

: f-2
1 q-pick
1 q-pick
q<=
q? if f-0 else f-1 then ;

: f-3
42 q-num
q-display
q-drop
d-loop 2@
1 0 shove-back
check-lambda r> drop execute ;

: f-4
q-display
1 0 shove-back ;

: f-5
q-drop
qt-1 2@
d-funny_u003F 2@
q-call
f-4 ;

: f-6
q-drop
['] f-3 q-lambda
2dup d-loop 2!
q-drop
d-loop 2@
q-call
f-5 ;

: f-7
q-drop
['] f-2 q-lambda
2dup d-count 2!
q-drop
1 q-num
1000000000 q-num
d-count 2@
q-call
f-6 ;

: f-8
b-toplevel
f-7 ;

\ toplevel
: a-toplevel
s-adam
qt-1 2!
q-null
qt-0 2!
f-8 ;