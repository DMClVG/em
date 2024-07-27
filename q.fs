
64 1024 * 1024 * true false s" gc.fs" included drop 2drop

s" qruntime.fs" included

: include-code
s" symbols.fs" included
s" typed.fs" included
s" b.fs" included
s" oop.fs" included
s" a.fs" included
  ;

include-code

: rl
  s" make" system
  include-code ;
