(load "list.ss")

(define load
  (let ((orig-load load)
        (loaded '("list.ss" "load.ss")))
    (lambda (filename)
      (unless (member filename loaded)
        (set! loaded (cons filename loaded))
        (orig-load filename)))))
