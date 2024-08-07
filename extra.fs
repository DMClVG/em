:noname
  q-unbox
  slurp-file
  q-string
; constant read-file-all

:noname
  cells pre-argv + @
  cstring>sstring q-string ; constant cli-arg

:noname pre-argc ; constant cli-arg-count
