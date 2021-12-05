#lang racket/base

#|review: ignore|#

(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/port)

;; env ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct env (bindings parent)
  #:transparent)

(define (make-env [prims (make-hasheq)] [parent #f])
  (env prims parent))

(define (env-lookup the-env name [fail-thunk (λ () (error 'env-lookup (format "unbound variable ~a" name)))])
  (hash-ref
   (env-bindings the-env)
   name
   (lambda ()
     (cond
       [(env-parent the-env)
        => (λ (parent-env)
             (env-lookup parent-env name fail-thunk))]
       [else (fail-thunk)]))))

(define (env-add the-env name value)
  (begin0 the-env
    (hash-set! (env-bindings the-env) name value)))

(define (env-set the-env name value)
  (cond
    [(hash-has-key? (env-bindings the-env) name)
     (hash-set! (env-bindings the-env) name value)]
    [(env-parent the-env)
     => (λ (parent-env)
          (env-set parent-env name value))]
    [else
     (error 'env-set  (format "unbound variable ~a" name))]))

(define (env-fresh the-env)
  (make-env (make-hasheq) the-env))

(begin-for-syntax
  (define-syntax-class env-clause
    (pattern name:id
             #:with id #''name
             #:with expr #'name)
    (pattern [name:id expr:expr]
             #:with id #''name)))

(define-syntax (make-env* stx)
  (syntax-parse stx
    [(_ clause:env-clause ...+)
     #'(make-env (make-hasheq (list (cons clause.id clause.expr) ...)))]))


;; eval ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-env
  (make-parameter #f))

(define (eval* e the-env)
  (define-values (res _res-env)
    (eval e the-env))
  res)

(define (eval e the-env)
  (with-handlers ([exn:fail? (λ (ex)
                               (eprintf "~a~n  in: ~s~n" (exn-message ex) e)
                               (exit 1))])
    (match e
      [(or (? boolean?)
           (? char?)
           (? eof-object?)
           (? string?)
           (? number?))
       (values e the-env)]

      [(? symbol? name)
       (values (env-lookup the-env name) the-env)]

      [`(quote ,e)
       (values e the-env)]
      [`(quasiquote ,e)
       (define (help qe [in-list? #f])
         (match qe
           [(or (? boolean?)
                (? string?)
                (? number?)
                (? symbol?))
            qe]
           [(cons 'unquote e)
            (eval* (car e) the-env)]
           [(cons 'unquote-splicing e)
            (unless in-list?
              (raise-syntax-error 'interp "invalid context within quasiquote" qe))
            (eval* (car e) the-env)]
           [`(,es ...)
            (for/fold ([res null])
                      ([e (in-list es)])
              (if (and (pair? e)
                       (eq? (car e) 'unquote-splicing))
                  (append res (help e #t))
                  (append res (list (help e #t)))))]))
       (values (help e) the-env)]

      [`(begin ,body-es ...)
       (for/fold ([res (void)]
                  [the-env the-env])
                 ([body-e (in-list body-es)])
         (define-values (body-res body-env)
           (eval body-e the-env))
         (values body-res body-env))]

      [`(if ,cond-e ,then-e ,else-e)
       (define-values (cond-res cond-env)
         (eval cond-e the-env))
       (eval (if cond-res then-e else-e) cond-env)]
      [`(cond [,cond-e ,body-es ...] ,clauses ...)
       (eval (if (null? clauses)
                 `(if ,cond-e (begin ,@body-es) (void))
                 `(if ,cond-e (begin ,@body-es) (cond ,@clauses)))
             the-env)]
      [`(when ,cond-e ,body-es ...)   (eval `(if ,cond-e (begin ,@body-es) (void)) the-env)]
      [`(unless ,cond-e ,body-es ...) (eval `(if ,cond-e (void) (begin ,@body-es)) the-env)]

      [`(or) (values #f the-env)]
      [`(or ,e) (eval e the-env)]
      [`(or ,e0 ,es ...) (eval `(if ,e0 ,e0 (or ,@es)) the-env)]

      [`(and) (values #t the-env)]
      [`(and ,e) (eval e the-env)]
      [`(and ,e0 ,es ...) (eval `(if ,e0 (and ,@es) #f) the-env)]

      [`(lambda (,names ...) ,body-es ...)
       (for ([name (in-list names)])
         (unless (symbol? name)
           (raise-syntax-error 'interp "(lambda (NAME ...) ...)" e)))
       (define lam
         (lambda args
           (unless (= (length args)
                      (length names))
             (error 'interp "bad arity"))
           (define lambda-env
             (for/fold ([lambda-env (env-fresh the-env)])
                       ([name (in-list names)]
                        [arg (in-list args)])
               (env-add lambda-env name arg)))
           (for/fold ([lambda-res (void)]
                      [lambda-env lambda-env]
                      #:result lambda-res)
                     ([body-e (in-list body-es)])
             (define-values (body-res body-env)
               (parameterize ([current-env lambda-env])
                 (eval body-e lambda-env)))
             (values body-res body-env))))
       (values lam the-env)]
      [`(let () ,body-es ...)
       (eval `((lambda () ,@body-es)) the-env)]
      [`(let ([,name ,clause-e]) ,body-es ...)
       (eval `((lambda (,name) ,@body-es) ,clause-e) the-env)]
      [`(let ([,name ,clause-e] ,rest-clauses ...) ,body-es ...)
       (eval `(let ([,name ,clause-e])
                (let ,rest-clauses ,@body-es))
             the-env)]
      [`(let ,let-name ([,names ,clause-es] ...) ,body-es ...)
       (eval `((lambda ()
                 (define ,let-name
                   (lambda (,@names)
                     ,@body-es))
                 (,let-name ,@clause-es)))
             the-env)]
      [`(let ,_ ...)
       (raise-syntax-error 'interp "(let MAYBE-NAME ([NAME E] ...) BODY ...)" e)]

      [`(set! ,(? symbol? name) ,e)
       (define-values (value _value-env)
         (eval e the-env))
       (env-set the-env name value)
       (values value the-env)]
      [`(set! ,_ ...)
       (raise-syntax-error 'interp "(set! NAME VALUE)" e)]

      [`(define ,(? symbol? name) ,body-e)
       (define-values (body define-env)
         (eval body-e the-env))
       (values (void) (env-add define-env name body))]
      [`(define (,name ,arg-names ...) ,body-es ...)
       (eval `(define ,name (lambda ,arg-names ,@body-es)) the-env)]
      [`(define ,_ ...)
       (raise-syntax-error 'interp "(define NAME VALUE)" e)]

      [`(,rator-e ,rand-es ...)
       (define-values (rator rator-env)
         (eval rator-e the-env))
       (define-values (rands rands-env)
         (for/fold ([rands null]
                    [rands-env rator-env]
                    #:result (values (reverse rands) rands-env))
                   ([rand-e (in-list rand-es)])
           (define-values (rand rand-env)
             (eval rand-e rands-env))
           (values (cons rand rands) rand-env)))
       (values (apply rator rands) rands-env)]

      [_
       (raise-syntax-error 'interp "invalid syntax" e)])))

(define (load filename [the-env (make-base-env)])
  (define-values (source-dir source-filename)
    (parameterize ([current-directory (current-load-relative-directory)])
      (define source-filename (build-path (current-directory) filename))
      (define-values (source-dir _name _must-be-dir?)
        (split-path source-filename))
      (values source-dir source-filename)))
  (define prog
    (call-with-input-file source-filename
      (lambda (in)
        (for/list ([e (in-port read in)])
          e))))
  (define-values (_res _env)
    (parameterize ([current-load-relative-directory source-dir])
      (interp prog the-env)))
  (void))

(define (interp prog [the-env (make-base-env)])
  (parameterize ([current-env the-env])
    (for/fold ([res (void)]
               [the-env the-env])
              ([e (in-list prog)])
      (eval e the-env))))

(define (make-base-env)
  (define base-env
    (make-env*
     [load (λ (filename) (load filename base-env))]
     [eval (λ (expr) (eval* expr (current-env)))]
     [argv (cdr (vector->list (current-command-line-arguments)))]
     eq? eqv? equal? boolean? number? char? string? symbol? pair? list? null? void? eof-object?
     apply exit error void eof cons car cdr list string string->symbol
     format display displayln print println write writeln newline
     call-with-input-file read-char peek-char port->string
     = + - * / < > <= >=))
  base-env)

(module+ main
  (require racket/cmdline)
  (parameterize ([current-load-relative-directory (current-directory)])
    (load
     (command-line
      #:args [filename . rest]
      filename))))
