\ defines
variable d-person

\ extern


\ imports


\ symbols


\ quotes


\ lambdas
: f-0
1 3 shove-back ;

: f-1
['] f-0 q-lambda
dup d-person !
1 0 shove-back ;

\ toplevel
: oop-toplevel

f-1 ;