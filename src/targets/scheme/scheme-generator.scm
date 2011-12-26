;;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module scheme-generator
  (export %boilerplate %begin %define %define-const %cond %match-case %match-lambda %if %string-append %+ %module))

(define (%boilerplate)
  '(begin
     (define (*pr-string *indent str)
       (format "~s" str))
     (define (*pr-integer *indent int)
       (format "~a" int))
     (define (**pr-list lst sep)
       (reduce (lambda (substr chunk)
                 (format "~a~a~a" chunk sep substr))
               "" lst))
     (define (*get-constant const) const)))

(define (%begin . statements)
  (letrec ([plainize
            (lambda (statement statements)
              (cond [(and (pair? statement) (eq? 'begin (car statement)))
                     (append (cdr statement) statements)]
                    [else (cons statement statements)]))])
    `(begin
       ,@(reduce plainize '() (cons '() statements)))))
;; `(begin ,@statements))

(define (%define name args body)
  `(begin
     (define ,(cons name args) ,body)))

(define (%define-const name body)
  `(begin
     (define ,name ,body)))

(define (%cond branches else-branch)
  (cond [(and (list? branches) (> (length branches) 0))
         `(cond ,@branches
                [else ,else-branch])]
        [else else-branch]))

(define (%match-case argument branches else-branch)
  (cond [(> (length branches) 0)
         `(match-case ,argument
            ,@branches
            [else ,else-branch])]
        [else else-branch]))

(define (%match-lambda branches else-branch)
  (cond [(> (length branches) 0)
         `(match-lambda
            ,@branches
            [else ,else-branch])]
        [else else-branch]))

(define (%if predicate consequent alternative)
  `(if ,predicate ,consequent ,alternative))

(define (%string-append . args)
  (cond [(= 0 (length args)) ""]
        [(= 1 (length args)) (car args)]
        [else `(string-append ,@(reverse (reduce (lambda (arg args)
                                                   (if (and (string? arg) (string? (car args)))
                                                       (cons (string-append (car args) arg) (cdr args))
                                                       (cons arg args)))
                                                 '() (cons (list (car args)) (cdr args)))))]))

(define (%+ . args)
  (let ([args (filter (lambda (arg)
                     (not (and (number? arg) (= arg 0))))
                   args)])
    (cond [(= 0 (length args)) 0]
          [(= 1 (length args)) (car args)]
          [else (cons '+ args)])))

(define (%module name exports)
  `(module ,name
    (export ,@exports)))