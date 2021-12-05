(load "load.ss")
(load "match.ss")
(load "read.ss")

(unless (= (length argv) 1)
  (displayln "usage: main.ss FILENAME")
  (exit 1))

(call-with-input-file (car argv)
  (lambda (in)
    (let loop ((exprs null))
      (define e (read in))
      (cond
       ((eof-object? e) (reverse exprs))
       (#t
        (writeln e)
        (loop (cons e exprs)))))))

;; (println
;;  (match 42
;;    '(((? string?) "a string")
;;      ((or (? string?)
;;           (? number? n)) n))))
