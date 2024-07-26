module typed

\ defines
variable d-tdisplay d-tdisplay root-address 
: tdisplay d-tdisplay @ ;
variable d-tstring d-tstring root-address 
: tstring d-tstring @ ;
variable d-tnull d-tnull root-address 
: tnull d-tnull @ ;
variable d-tbool d-tbool root-address 
: tbool d-tbool @ ;
variable d-tcons d-tcons root-address 
: tcons d-tcons @ ;
variable d-tnumber d-tnumber root-address 
: tnumber d-tnumber @ ;
variable d-bool_u002D_u003Estring d-bool_u002D_u003Estring root-address 
: bool->string d-bool_u002D_u003Estring @ ;
variable d-display_u002Da_u002Dlist d-display_u002Da_u002Dlist root-address 
: display-a-list d-display_u002Da_u002Dlist @ ;
variable d-cons_u003F d-cons_u003F root-address 
: cons? d-cons_u003F @ ;
variable d-string_u003F d-string_u003F root-address 
: string? d-string_u003F @ ;
variable d-number_u003F d-number_u003F root-address 
: number? d-number_u003F @ ;
variable d-boolean_u003F d-boolean_u003F root-address 
: boolean? d-boolean_u003F @ ;
variable d-null_u003F d-null_u003F root-address 
: null? d-null_u003F @ ;
variable d-datum_u003F d-datum_u003F root-address 
: datum? d-datum_u003F @ ;
variable d-type_u003F d-type_u003F root-address 
: type? d-type_u003F @ ;
variable d-typed d-typed root-address 
: typed d-typed @ ;

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
variable qt-16
variable qt-17
variable qt-18
variable qt-19

\ lambdas
: f-0
1 pick
1 pick
cons
3 2 shove-back
execute ;

: f-1
0 pick
car
2 1 shove-back
execute ;

: f-2
0 pick
cdr
2 1 shove-back
execute ;

: f-3
eq?
3 1 shove-back
execute ;

: f-4
qt-0 @
1 pick
type?
execute
f-3 ;

: f-5
eq?
3 1 shove-back
execute ;

: f-6
qt-1 @
1 pick
type?
execute
f-5 ;

: f-7
eq?
3 1 shove-back
execute ;

: f-8
qt-2 @
1 pick
type?
execute
f-7 ;

: f-9
eq?
3 1 shove-back
execute ;

: f-10
qt-3 @
1 pick
type?
execute
f-9 ;

: f-11
eq?
3 1 shove-back
execute ;

: f-12
qt-4 @
1 pick
type?
execute
f-11 ;

: f-13
display-a-list
2 1 shove-back
execute ;

: f-14
datum?
execute
f-13 ;

: f-15
drop
0 pick
cdr
execute
f-14 ;

: f-16
qt-5 @
display
execute
f-15 ;

: f-17
tdisplay
2 1 shove-back
execute ;

: f-18
drop
0 pick
cdr
execute
f-17 ;

: f-19
qt-6 @
display
execute
f-18 ;

: f-20
qt-7 @
1 1 shove-back ;

: f-21
0=
if f-19 else f-20 then ;

: f-22
null?
execute
f-21 ;

: f-23
0 pick
cdr
execute
f-22 ;

: f-24
if f-16 else f-23 then ;

: f-25
cons?
execute
f-24 ;

: f-26
drop
0 pick
cdr
execute
f-25 ;

: f-27
tdisplay
execute
f-26 ;

: f-28
0 pick
car
execute
f-27 ;

: f-29
qt-8 @
1 1 shove-back ;

: f-30
qt-9 @
1 1 shove-back ;

: f-31
0 pick
if f-29 else f-30 then ;

: f-32
qt-10 @
1 pick
typed
3 1 shove-back
execute ;

: f-33
typed
3 2 shove-back
execute ;

: f-34
qt-11 @
2 pick
2 pick
cons
execute
f-33 ;

: f-35
qt-12 @
1 pick
typed
3 1 shove-back
execute ;

: f-36
qt-15 @
1 pick
typed
3 1 shove-back
execute ;

: f-37
drop
qt-17 @
display
2 1 shove-back
execute ;

: f-38
display-a-list
execute
f-37 ;

: f-39
drop
0 pick
datum?
execute
f-38 ;

: f-40
qt-16 @
display
execute
f-39 ;

: f-41
display
2 1 shove-back
execute ;

: f-42
number->string
execute
f-41 ;

: f-43
0 pick
datum?
execute
f-42 ;

: f-44
display
2 1 shove-back
execute ;

: f-45
0 pick
datum?
execute
f-44 ;

: f-46
qt-18 @
display
2 1 shove-back
execute ;

: f-47
display
2 1 shove-back
execute ;

: f-48
bool->string
execute
f-47 ;

: f-49
0 pick
datum?
execute
f-48 ;

: f-50
qt-19 @
display
2 1 shove-back
execute ;

: f-51
if f-49 else f-50 then ;

: f-52
0 pick
boolean?
execute
f-51 ;

: f-53
if f-46 else f-52 then ;

: f-54
0 pick
null?
execute
f-53 ;

: f-55
if f-45 else f-54 then ;

: f-56
0 pick
string?
execute
f-55 ;

: f-57
if f-43 else f-56 then ;

: f-58
0 pick
number?
execute
f-57 ;

: f-59
if f-40 else f-58 then ;

: f-60
0 pick
cons?
execute
f-59 ;

: f-61
dup d-tnull !
drop
['] f-36
dup d-tstring !
drop
['] f-60
dup d-tdisplay !
1 0 shove-back ;

: f-62
['] f-0
dup d-typed !
drop
['] f-1
dup d-type_u003F !
drop
['] f-2
dup d-datum_u003F !
drop
['] f-4
dup d-null_u003F !
drop
['] f-6
dup d-boolean_u003F !
drop
['] f-8
dup d-number_u003F !
drop
['] f-10
dup d-string_u003F !
drop
['] f-12
dup d-cons_u003F !
drop
['] f-28
dup d-display_u002Da_u002Dlist !
drop
['] f-31
dup d-bool_u002D_u003Estring !
drop
['] f-32
dup d-tnumber !
drop
['] f-34
dup d-tcons !
drop
['] f-35
dup d-tbool !
drop
qt-13 @
qt-14 @
typed
execute
f-61 ;

: init-quotes
s" ERR" q-string
qt-19 !
s" '()" q-string
qt-18 !
s" )" q-string
qt-17 !
s" (" q-string
qt-16 !
s-string
qt-15 !
q-null
qt-14 !
s-null
qt-13 !
s-boolean
qt-12 !
s-cons
qt-11 !
s-number
qt-10 !
s" #f" q-string
qt-9 !
s" #t" q-string
qt-8 !
q-null
qt-7 !
s"  . " q-string
qt-6 !
s"  " q-string
qt-5 !
s-cons
qt-4 !
s-string
qt-3 !
s-number
qt-2 !
s-boolean
qt-1 !
s-null
qt-0 !
 ;

provide



: typed-toplevel
init-quotes
f-62 ;

end-module