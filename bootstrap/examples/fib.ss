(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 2))
         (fib (- n 1)))))

;; (let loop ((n 0))
;;   (unless (= n 8)
;;     (println (fib n))
;;     (loop (+ n 1))))

(displayln (fib 8))
