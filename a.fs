include qruntime.fs
include symbols.fs

\ defines


\ extern


\ imports


\ symbols


\ quotes
2variable qt-0

\ lambdas
: f-0
q-display
 ;

: f-1
q-drop
qt-0 2@
d-funny_u003F 2@
q-call
f-0 ;

: f-2
b-toplevel
f-1 ;

\ toplevel
: a-toplevel
s-sandy
qt-0 2!
f-2
 ;