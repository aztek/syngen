;;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module haskell
  (import common adt haskell-generator)
  (export generate-haskell-code))

(define *predefined-types* ; (type-name type-predicate)
  '(("integer" . integer?)
    ("string" . string?)))

(define (predefined-type? type-name)
  (assoc type-name *predefined-types*))

(define *boilerplate-imports*
  (list (%import "Data.List")
        (%import "Text.Regexp.Posix")))

(define *boilerplate-typeclass*
  (%class "Printer" (list "a")
   (list (%signature "pri" (%fun-type (%var-type "a") (%fun-type (%plain-type "Int") (%plain-type "String"))))
         (%signature "pr" (%fun-type (%var-type "a") (%plain-type "String")))
         (%function "pr" (list) (%expr-funcall "pri" (list (%expr-num 0)))))))

(define (generate-haskell-code module types)
  (pr
   (%module (generate-module-name module)
            (generate-exports types)
            *boilerplate-imports*
            (generate-definitions types))))

(define (generate-definitions types)
  (cons *boilerplate-typeclass*
        (apply append (map generate-definition types))))

(define generate-definition
  (match-lambda
    [(bind ?type ?body)
     (match-case body
       [(sum ?disjuncts)
        (cons (generate-adt type disjuncts)
              (cons (generate-printer type disjuncts)
                    (generate-injectors disjuncts)))]
       [(string ?regexp)
        (list (generate-adt type (list (%product type (list (%ref "String")) '()))))]
       [else (error "generate-def" "Neither sum type nor token" body)])]
    [?else (error "generate-def" "Not a type definition" else)]))

(define (generate-adt type disjuncts)
  (%data (generate-type-name type)
         (map (match-lambda
                [(product ?constructor ?refs ?-)
                 (%branch (generate-constructor-name constructor)
                          (map (match-lambda
                                 [(ref ?type) (%plain-type (generate-type-name type))]
                                 [((or ref* ref+) ?type) (%list-type (generate-type-name type))])
                               refs))]
                [?else (error "generate-adt" "Not a product type" else)])
              disjuncts)))

(define (generate-printer type disjuncts)
  (%instance "Printer" (generate-type-name type)
             (map (match-lambda
                   [(product ?constructor ?refs ?repr)
                    (let ([arguments (generate-arguments refs)])
                      (%match "pri" (generate-constructor-name constructor) arguments
                              (reduce %expr-concat '() (generate-repr-printer repr arguments))))]
                   [?else (error "generate-printer" "Not a product type" else)])
                  disjuncts)))


(define (generate-repr-printer reprs arguments)
  (let loop ([code '()]
             [reprs reprs]
             [args arguments]
             [indent 0])
    (cond [(not (pair? reprs)) (reverse code)]
          [else (let ([repr (car reprs)]
                      [rest-reprs (cdr reprs)])
                  (match-case repr
                    [(repr-terminal ?value)
                     (loop (cons (%expr-string (prepare-terminal value)) code)
                           rest-reprs
                           args
                           indent)]
                    [(repr-nonterminal ?name)
                     (loop (cons (%expr-funcall "pri" (list (if (> indent 0)
                                                                (%expr-plus (%expr-var "indent") (%expr-num indent))
                                                                (%expr-var "indent"))
                                                            (%expr-var (car args)))) code)
                           rest-reprs
                           (cdr args)
                           indent)]
                    [(repr-list ?name ?separator)
                     (loop (cons (%expr-apply (%expr-fun "concat")
                                              (%expr-apply (%expr-funcall "intersperse" (list (car (generate-repr-printer (list separator) '()))))
                                                           (%expr-funcall "map" (list (%expr-var "pr") (%expr-var (car args))))))
                                 code)
                           rest-reprs
                           (cdr args)
                           indent)]
                    [(repr-const "{")
                     (loop code rest-reprs args (+ indent 1))]
                    [(repr-const "}")
                     (loop code rest-reprs args (- indent 1))]
                    [(repr-const "_")
                     (loop (cons (%expr-string "indent") code) rest-reprs args indent)]
                    [else (error "generate-repr-printer" "Not a repr" repr)]))])))

(define (prepare-terminal terminal)
  (pregexp-replace (pregexp-quote "\\n") terminal "\n"))

(define (generate-arguments refs)
  (enumerate-duplicates
    (map (match-lambda
           [(ref ?type) (generate-variable-name type)]
           [(ref* ?type) (generate-variable-name type)]
           [(ref+ ?type) (generate-variable-name type)])
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

(define (generate-exports types)
  (cons (%export-fun "pr")
        (reverse (apply append
                        (map (match-lambda
                              [(bind ?type (sum ?disjuncts))
                               (cons (%export-type (generate-type-name type))
                                     (map (match-lambda
                                           [(product ?constructor ?- ?-)
                                            (%export-fun (generate-exported-injector-name constructor))])
                                          disjuncts))]
                              [else '()])
                             types)))))


(define (generate-injectors disjuncts)
  (map (match-lambda
         [(product ?constructor ?- ?-)
          (%fundef (generate-exported-injector-name constructor) (list) (%expr-fun (generate-constructor-name constructor)))])
       disjuncts))

;;; Names
(define generate-constructor-name uppercamelcase)

(define generate-exported-injector-name camelcase)

(define generate-module-name uppercamelcase)

(define generate-type-name uppercamelcase)

(define generate-variable-name camelcase)