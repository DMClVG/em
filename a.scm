(require std)
(require person)

(define (factorial n)
  (if (= n 0)
      1
      (* (factorial (- n 1)) n)))

(display "'(a b c) has length: ")
(printf 'number (list-length '(a b c)))
(newline)

(printf 'string "~~~INFO~~~")
(newline)
(info john)
(printf 'string "~~~~~~~~~~")
(newline)

(printf 'string "~~~INFO~~~")
(newline)
(info melissa)
(printf 'string "~~~~~~~~~~")
(newline)

(printf 'string (read-file-all (cli-arg 1)))
