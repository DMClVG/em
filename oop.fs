\ defines
variable d-melissa d-melissa root-address 
: melissa d-melissa @ ;
variable d-john d-john root-address 
: john d-john @ ;
variable d-person d-person root-address 
: person d-person @ ;
variable d-person_u002Dbehavior d-person_u002Dbehavior root-address 
: person-behavior d-person_u002Dbehavior @ ;
variable d-inc_u0021 d-inc_u0021 root-address 
: inc! d-inc_u0021 @ ;
variable d-people_u002Dcount d-people_u002Dcount root-address 
: people-count d-people_u002Dcount @ ;
variable d-info d-info root-address 
: info d-info @ ;

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
drop
newline
1 1 shove-back
execute ;

: f-1
display
execute
f-0 ;

: f-2
drop
qt-5 @
1 pick
execute
f-1 ;

: f-3
drop
qt-4 @
display
execute
f-2 ;

: f-4
drop
newline
execute
f-3 ;

: f-5
display
execute
f-4 ;

: f-6
number->string
execute
f-5 ;

: f-7
drop
qt-3 @
1 pick
execute
f-6 ;

: f-8
drop
qt-2 @
display
execute
f-7 ;

: f-9
drop
newline
execute
f-8 ;

: f-10
display
execute
f-9 ;

: f-11
drop
qt-1 @
1 pick
execute
f-10 ;

: f-12
qt-0 @
display
execute
f-11 ;

: f-13
+
set!
3 1 shove-back
execute ;

: f-14
0 pick
1
2 pick
ref
execute
f-13 ;

: f-15
0 pick
car
2 2 shove-back
execute ;

: f-16
car
2 2 shove-back
execute ;

: f-17
0 pick
cdr
execute
f-16 ;

: f-18
car
2 2 shove-back
execute ;

: f-19
cdr
execute
f-18 ;

: f-20
0 pick
cdr
execute
f-19 ;

: f-21
info
2 2 shove-back
execute ;

: f-22
0 pick
person-behavior
object
execute
f-21 ;

: f-23
qt-10 @
1 2 shove-back ;

: f-24
if f-22 else f-23 then ;

: f-25
1 pick
qt-9 @
eq?
execute
f-24 ;

: f-26
if f-20 else f-25 then ;

: f-27
1 pick
qt-8 @
eq?
execute
f-26 ;

: f-28
if f-17 else f-27 then ;

: f-29
1 pick
qt-7 @
eq?
execute
f-28 ;

: f-30
if f-15 else f-29 then ;

: f-31
1 pick
qt-6 @
eq?
execute
f-30 ;

: f-32
person-behavior
object
3 3 shove-back
execute ;

: f-33
cons
execute
f-32 ;

: f-34
cons
execute
f-33 ;

: f-35
drop
['] f-31
dup d-person_u002Dbehavior !
drop
2 pick
2 pick
2 pick
qt-11 @
cons
execute
f-34 ;

: f-36
people-count
inc!
execute
f-35 ;

: f-37
dup d-melissa !
1 0 shove-back ;

: f-38
dup d-john !
drop
qt-14 @
27
qt-15 @
person
execute
f-37 ;

: f-39
dup d-people_u002Dcount !
drop
['] f-14
dup d-inc_u0021 !
drop
['] f-36
dup d-person !
drop
qt-12 @
23
qt-13 @
person
execute
f-38 ;

: f-40
['] f-12
dup d-info !
drop
0
box
execute
f-39 ;

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
f-40 ;