;; Example program: cat

(require std)

(define (cat path)
  (printf 'string (read-file-all path)))

(define (show-usage)
  (printf 'string "USAGE: cat.fs [PATH]")
  (newline))

(if (not (eq? (cli-arg-count) 2))
    (show-usage)
    (cat (cli-arg 1)))
