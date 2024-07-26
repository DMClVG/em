module oop

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
qt-6 @
1 2 shove-back ;

: f-16
person-behavior
object
3 3 shove-back
execute ;

: f-17
cons
execute
f-16 ;

: f-18
cons
execute
f-17 ;

: f-19
drop
['] f-15
dup d-person_u002Dbehavior !
drop
2 pick
2 pick
2 pick
qt-7 @
cons
execute
f-18 ;

: f-20
people-count
inc!
execute
f-19 ;

: f-21
dup d-melissa !
1 0 shove-back ;

: f-22
dup d-john !
drop
qt-10 @
27
qt-11 @
person
execute
f-21 ;

: f-23
dup d-people_u002Dcount !
drop
['] f-14
dup d-inc_u0021 !
drop
['] f-20
dup d-person !
drop
qt-8 @
23
qt-9 @
person
execute
f-22 ;

: f-24
0
drop
['] f-12
dup d-info !
drop
0
box
execute
f-23 ;

: init-quotes
s" ヾ(@^▽^@)ノ" q-string
qt-11 !
s" Melissa" q-string
qt-10 !
s" (¬_¬)ﾉ" q-string
qt-9 !
s" John" q-string
qt-8 !
q-null
qt-7 !
q-null
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
 ;

provide

: info info ; 
: john john ; 

: oop-toplevel
init-quotes
f-24 ;

end-module