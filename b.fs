include qruntime.fs
include symbols.fs

\ defines
2variable d-funny_u003F

\ extern


\ imports


\ symbols


\ quotes
2variable qt-0

\ lambdas
: f-0
0 q-pick
qt-0 2@
q=
 ;

: f-1
['] f-0 q-lambda
2dup d-funny_u003F 2!
 ;

\ toplevel
: b-toplevel
s-sandy
qt-0 2!
f-1
 ;