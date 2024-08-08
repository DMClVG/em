module read

\ defines
variable d-read d-read root-address 
: read d-read @ ;
variable d-float d-float root-address 
: float d-float @ ;
variable d-symbol d-symbol root-address 
: symbol d-symbol @ ;
variable d-string d-string root-address 
: string d-string @ ;
variable d-old_u002Dcons d-old_u002Dcons root-address 
: old-cons d-old_u002Dcons @ ;
variable d-value d-value root-address 
: value d-value @ ;
variable d-type d-type root-address 
: type d-type @ ;
variable d-tcell d-tcell root-address 
: tcell d-tcell @ ;

\ quotes
variable qt-0
variable qt-1
variable qt-2
variable qt-3
variable qt-4
variable qt-5

\ lambdas
: f-0
drop
0 pick
1 1 shove-back
1 2 shove-back ;

: f-1
drop
0 pick
1
3 pick
offset-set!
execute
f-0 ;

: f-2
0 pick
0
4 pick
offset-set!
execute
f-1 ;

: f-3
2
make-memory
execute
f-2 ;

: f-4
0 pick
0
offset-ref
3 1 shove-back
execute ;

: f-5
0 pick
1
offset-ref
3 1 shove-back
execute ;

: f-6
qt-0 @
1 pick
tcell
3 1 shove-back
execute ;

: f-7
qt-1 @
1 pick
tcell
3 1 shove-back
execute ;

: f-8
qt-2 @
1 pick
tcell
3 1 shove-back
execute ;

: f-9
tcell
3 1 shove-back
execute ;

: f-10
cons
execute
f-9 ;

: f-11
qt-5 @
symbol
execute
f-10 ;

: f-12
qt-3 @
qt-4 @
symbol
execute
f-11 ;

: f-13
0
drop
['] f-3
dup d-tcell !
drop
['] f-4
dup d-type !
drop
['] f-5
dup d-value !
drop
cons
dup d-old_u002Dcons !
drop
['] f-6
dup d-string !
drop
['] f-7
dup d-symbol !
drop
['] f-8
dup d-float !
drop
['] f-12
dup d-read !
1 0 shove-back ;

: init-quotes
s-b
qt-5 !
s-a
qt-4 !
s-cons
qt-3 !
s-float
qt-2 !
s-symbol
qt-1 !
s-string
qt-0 !
 ;

provide

: type type ; 
: value value ; 
: read read ; 

: read-toplevel
init-quotes
f-13 ;

end-module