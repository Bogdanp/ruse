(load "bool.ss")
(load "list.ss")

(define match
  (let ((match?
         (lambda (e m-expr)
           (let match? ((e e) (m-expr m-expr))
             (cond
              ((eq? m-expr '_)
               (cons null null))

              ((and (number? m-expr) (= m-expr e))
               (cons null null))

              ((and (string? m-expr) (equal? m-expr e))
               (cons null null))

              ((symbol? m-expr)
               (cons (list m-expr)
                     (list e)))

              ((and (pair? m-expr)
                    (eq? (car m-expr) 'or))
               (let loop ([m-exprs (cdr m-expr)])
                 (and (not (null? m-exprs))
                      (or (match? e (car m-exprs))
                          (loop (cdr m-exprs))))))

              ((and (pair? m-expr)
                    (eq? (car m-expr) '?))
               (and (eval `(,(cadr m-expr) ,e))
                    (cons
                     (cdr (cdr m-expr))
                     (list e))))

              (#t
               (error 'match? (format "invalid match expr ~s" m-expr))))))))
    (lambda (e clauses)
      (let loop ((clauses clauses))
        (cond
         ((null? clauses)
          (error 'match "no matching clauses"))
         (#t
          (define clause (car clauses))
          (define expr (car clause))
          (define body (cadr clause))
          (define m (match? e expr))
          (cond
           (m
            (define bindings
              (let loop ((names (car m))
                         (values (cdr m))
                         (bindings '()))
                (cond
                 ((or (null? names)
                      (null? values))
                  (reverse bindings))
                 (#t
                  (define binding
                    `(,(car names) ,(car values)))
                  (loop (cdr names)
                        (cdr values)
                        (cons binding bindings))))))
            (eval `(let ,bindings ,body)))
           (#t
            (loop (cdr clauses))))))))))
