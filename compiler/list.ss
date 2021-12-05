(define null '())

(define (caar lst) (car (car lst)))
(define (cadr lst) (car (cdr lst)))

(define (length lst)
  (let loop ((len 0)
             (lst lst))
    (if (null? lst)
        len
        (loop (+ len 1) (cdr lst)))))

(define assq 'undefined)
(define assoc 'undefined)
(let ((make-assoc-proc
       (lambda (eq-proc)
         (lambda (v lst)
           (let loop ((lst lst))
             (cond
              ((null? lst) #f)
              (#t
               (define hd (car lst))
               (if (and (pair? hd)
                        (eq-proc v (car hd)))
                   hd
                   (loop (cdr lst))))))))))
  (set! assq (make-assoc-proc eq?))
  (set! assoc (make-assoc-proc equal?)))

(define memq 'undefined)
(define memv 'undefined)
(define member 'undefined)
(let ((make-member-proc
       (lambda (eq-proc)
         (lambda (v lst)
           (let loop ((lst lst))
             (cond
              ((null? lst) #f)
              ((eq-proc v (car lst)) (car lst))
              (#t (loop (cdr lst)))))))))
  (set! memq (make-member-proc eq?))
  (set! memv (make-member-proc eqv?))
  (set! member (make-member-proc equal?)))

(define (reverse lst)
  (let loop ((lst lst)
             (res null))
    (if (null? lst)
        res
        (loop (cdr lst) (cons (car lst) res)))))
