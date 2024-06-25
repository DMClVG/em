include qruntime.fs
include symbols.fs

\ defines
2variable d_aba
2variable d_funnee

\ extern


\ imports


\ symbols





\ quotes
2variable qt_0
2variable qt_1
2variable qt_2
2variable qt_3

\ lambdas
: f_0
qt_0 2@
 ;

: f_1
0 q-pick
2 q-pick
4 q-pick
qt_1 2@
q-pair
q-pair
q-pair
q-display
 ;

: f_2
qt_2 2@
q-display
 ;

: f_3
qt_3 2@
q-display
 ;

: f_4
['] f_0 q-lambda
2dup d_funnee 2!
q-drop
['] f_1 q-lambda
2dup d_aba 2!
q-drop
11 q-num
1 q-num
1 q-pick
q-display
q-drop
newline type q-null
q-drop
1 q-pick
1 q-pick
q+
q-display
q-drop
newline type q-null
1 2 shove-back
q-drop
true q-bool
false q-bool
q-and
q-display
q-drop
false q-bool
true q-bool
q-or
q-display
q-drop
false q-bool
q-not
q-display
q-drop
2 q-num
3 q-num
q>=
q? if f_2 else f_3 then ;

\ toplevel
: a_toplevel
s_nooo
qt_3 2!
s_whaaat
qt_2 2!
q-null
qt_1 2!
s_h
s_a
s_h
s_a
q-null
q-pair
q-pair
q-pair
q-pair
qt_0 2!
f_4
 ;