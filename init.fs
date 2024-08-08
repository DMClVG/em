\ store arguments
argc @ constant pre-argc
here pre-argc cells allot constant pre-argv

argv @
pre-argv
pre-argc cells
move

: drop-args argc @ 0 do next-arg 2drop loop ;
drop-args \ make gforth stop processing further arguments

64 1024 * 1024 * true false s" q/gc.fs" included drop 2drop

s" q/qruntime.fs" included

: include-code
s" symbols.fs" included
s" extra.fs" included

s" std.fs" included
s" read.fs" included
s" person.fs" included

s" main.fs" included
  ;

include-code

: rl
  s" make" system
  include-code ;

: r main-toplevel ;
: q bye ;
