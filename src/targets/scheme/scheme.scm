;;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module scheme
  (import common cfg adt scheme-generator)
  (export generate-scheme-code))

(set! *pp-width* 80)

(define *predefined-types* ; (type-name type-predicate)
  '(("integer" . integer?)
    ("string" . string?)))

(define (predefined-type? type-name)
  (assoc type-name *predefined-types*))

(define (generate-scheme-code module adts)
  (with-output-to-string
    (lambda ()
      (match-case adts
        [(adts ?context ?types)
         (let ([code (cons (generate-module-signature module types)
                           (append (cdr (%boilerplate))
                                   (append (cdr (apply %begin (map (generate-def context) types)))
                                           (cdr (generate-common-printer types)))))])
           (for-each pp code))]))))

(define (generate-def context)
  (match-lambda
    [(bind ?type ?body)
     (match-case body
       [(sum ?disjuncts)
        (apply %begin (cons (generate-predicate type body)
                            (cons (generate-printer context type body)
                                  (map (generate-constructor type) disjuncts))))]
       [(string ?regexp)
        (%begin (generate-predicate type body)
                (generate-printer context type body))]
       [else (error "generate-def" "Neither sum type nor token" body)])]
    [?else (error "generate-def" "Not a type definition" else)]))

(define (generate-predicate type body)
  (match-case body
    [(sum ?disjuncts)
     (%define-const (generate-predicate-name type)
                    (%match-lambda
                      (map (lambda (p)
                             `(,(generate-product-pattern p) #t))
                           disjuncts)
                      #f))]
    [(string ?regexp)
     (%define (generate-predicate-name type)
              '(value)
              `(pregexp-match ,regexp value))]
    [else (error "generate-predicate" "Neither sum type nor token" body)]))

(define (generate-printer context type body)
  (let ([printer-name (generate-printer-name type)]
        [printer-front (generate-printer-front type)])
    (let ([arg (generate-printer-argument type)])
      (match-case body
        [(sum ?disjuncts)
         (%define printer-name
                  `(*indent ,arg)
                  (%match-case arg
                               (map (match-lambda
                                      [(product ?constructor ?refs ?repr)
                                       (let ([arguments (generate-arguments refs)]
                                             [tag (generate-tag constructor)])
                                         `[(,tag ,@(map (lambda (arg) ($ '? arg)) arguments))
                                           ,(apply %string-append (generate-repr-printer context repr (map $ arguments)))])])
                                    disjuncts)
                               `(error "pr" ,(format "Type error, `~a' expected, got" type) ,arg)))]
        [(string ?-)
         (%define printer-name
                  `(*indent ,arg)
                  `(format "~a" ,arg))]))))

(define (generate-common-printer types)
  (%define 'pr
           '(value)
           (%cond (map (match-lambda
                         [(bind ?type ?-)
                          (let ([predicate (generate-predicate-name type)]
                                [printer (generate-printer-name type)])
                            `[(,predicate value) (,printer 0 value)])])
                       types)
                  `(error "pr" ,(format "Type error, value of any of types ~a expected, got"
                                        (apply &, (map (match-lambda [(bind ?type ?-) (format "`~a'" type)]) types)))
                          value))))

(define (generate-repr-printer context reprs arguments)
  (let loop ([code '()]
             [reprs (process-constants context reprs)]
             [args arguments]
             [indent 0])
    (cond [(not (pair? reprs)) (reverse code)]
          [else (let ([repr (car reprs)]
                      [rest-reprs (cdr reprs)])
                  (match-case repr
                    [(repr-terminal ?value)
                     (loop (cons (prepare-terminal value) code)
                           rest-reprs
                           args
                           indent)]
                    [(repr-nonterminal ?name)
                     (loop (cons `(,(generate-printer-name name) ,(%+ '*indent indent) ,(car args)) code)
                           rest-reprs
                           (cdr args)
                           indent)]
                    [(repr-list ?name ?separator)
                     (loop (cons `(**pr-list (map (lambda (arg)
                                                      (,(generate-printer-name name) ,(%+ '*indent indent) arg))
                                                    ,(car args))
                                   ,(car (generate-repr-printer context (list separator) '())))
                                 code)
                           rest-reprs
                           (cdr args)
                           indent)]
                    [(repr-const ?name)
                     (loop (append (generate-repr-printer context (cfg/get-constant context name) '()) code)
                           rest-reprs
                           args
                           indent)]
                    ["{"
                     (loop code rest-reprs args (+ indent 1))]
                    ["}"
                     (loop code rest-reprs args (- indent 1))]
                    ["_"
                     (loop (cons `(make-string (* 4 ,(%+ '*indent indent)) #\space) code) rest-reprs args indent)]
                    ["."
                     (loop code rest-reprs args indent)]
                    [else (error "generate-repr-printer" "Type error, not a repr" repr)]))])))

(define (prepare-terminal terminal)
  (pregexp-replace (pregexp-quote "\\n") terminal "\n"))

(define (process-constants context reprs)
  (let ([expanded (expand-constants context reprs)])
    (if (equal? reprs expanded)
        (insert-separators context expanded)
        (process-constants context expanded))))

(define (expand-constants context reprs)
  (reverse
   (reduce (lambda (repr reprs)
             (match-case repr
               [(repr-const ?name)
                (let ([r (reverse (cfg/get-constant context name))])
                  (cond [(equal? name "}")
                         (append r (cons name reprs))]
                        [(or (equal? name ".") (equal? name "{"))
                         (cons name (append r reprs))]
                        [(equal? name "_")
                         (cons name reprs)]
                        [else (append r reprs)]))]
               [else (cons repr reprs)]))
           '()
           (if (pair? reprs)
               (cons '() reprs)
               '()))))

(define (insert-separators context reprs)
  (let ([sep (reverse (cfg/get-constant context ","))]
        [internal? (lambda (repr)
                     (or (equal? repr "{")
                         (equal? repr "}")
                         (equal? repr "_")))])
    (letrec ([pcar (lambda (reprs)
                     (if (pair? reprs)
                         (if (not (internal? (car reprs)))
                             (car reprs)
                             (pcar (cdr reprs)))
                         "."))])
      (reverse
       (reduce (lambda (repr reprs)
                 (cond [(or (internal? repr) (internal? (car reprs))) (cons repr reprs)]
                       [(or (equal? repr ".") (equal? (pcar reprs) ".")) (cons repr reprs)]
                       [else (cons repr (append sep reprs))]))
               '()
               (if (pair? reprs)
                   (cons (list (car reprs)) (cdr reprs))
                   '()))))))

(define (generate-product-pattern product)
  (match-case product
    [(product ?constructor ?refs ?-)
     `(,(generate-tag constructor) ,@(make-list (length refs) '?-))]))

(define (generate-constructor type)
  (match-lambda
    [(product ?constructor ?refs ?-)
     (let* ([arguments (map string->symbol (generate-arguments refs))]
            [constructor-name (generate-constructor-name constructor)]
            [tag (generate-tag constructor)])
       (%define constructor-name
                arguments
                (%cond (map (generate-check (symbol->string constructor-name)) refs arguments)
                       `(list ',tag ,@arguments))))]))

(define (generate-constructor-name type-name)
  ($ "%" type-name))

(define (generate-predicate-name type-name)
  (let ([predefined (predefined-type? type-name)])
    (if predefined
        (cdr predefined)
        ($ "*" type-name "?"))))

(define (generate-printer-name type-name)
  ($ "*pr-" type-name))

(define (generate-printer-front type-name)
  ($ "pr-" type-name))

(define (generate-printer-argument type-name)
  ($ type-name))

(define (generate-tag type-name)
  ($ type-name))

(define (generate-arguments refs)
  (enumerate-duplicates
    (map (match-lambda
           [(ref ?type) type]
           [(ref* ?type) type]
           [(ref+ ?type) type])
         refs)))

(define (enumerate-duplicates args)
  (let loop ([args args]
             [enumerated '()]
             [counts '()])
    (cond [(not (pair? args)) (reverse enumerated)]
          [(let ([arg (car args)]
                 [args (cdr args)])
            (let* ([pair (assoc arg counts)]
                   [count (if (not pair) 1 (+ 1 (cdr pair)))]
                   [counts (cons (cons arg count) counts)])
                (let ([arg (if (or (member arg args) (> count 1))
                               (string-append arg (number->string count))
                               arg)])
                  (loop args
                        (cons arg enumerated)
                        counts))))])))

(define (generate-check func)
  (lambda (ref arg) ; i really miss currying in scheme...
    (match-case ref
      [(ref ?type)
       `((not (,(generate-predicate-name type) ,arg))
         (error ,func ,(format "Type error, `~a' expected, got" type) ,arg))]
      [(ref* ?type)
       `((not (and (list? ,arg) (every? ,(generate-predicate-name type) ,arg)))
         (error ,func ,(format "Type error, list of `~a' expected, got" type) ,arg))]
      [(ref+ ?type)
       `((not (and (list? ,arg) (> (length ,arg) 0) (every? ,(generate-predicate-name type) ,arg)))
         (error ,func ,(format "Type error, list of `~a' expected, got" type) ,arg))])))

(define (generate-module-signature module types)
  (%module (string->symbol module)
   (cons 'pr (reverse (apply append
                             (map (match-lambda
                                   [(bind ?- (sum ?disjuncts))
                                    (map (match-lambda
                                          [(product ?constructor ?- ?-) (generate-constructor-name constructor)])
                                         disjuncts)]
                                   [else '()])
                                  types))))))