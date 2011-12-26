(set! *pp-width* 80)

(define *predefined-types* ; (type-name type-predicate)
  '(("integer" . integer?)
    ("string" . string?)))

(define (predefined-type? type-name)
  (assoc type-name *predefined-types*))

(define (generate-code inductives)
  (match-case inductives
    [(inductives ?context ?types)
     (for-each pp (cdr (generate-boilerplate)))
     (let ([code (apply %begin (map (generate-def context) types))])
       (for-each pp (cdr code)))]))

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
                (generate-printer context type body))])]))

(define (generate-predicate type body)
  (match-case body
    [(sum ?disjuncts)
     (%define (generate-predicate-name type)
              '(arg)
              (%match-case 'arg
                           (map (lambda (p)
                                  `(,(generate-product-pattern p) #t))
                                disjuncts)
                           #f))]
    [(string ?regexp)
     (%define (generate-predicate-name type)
              '(arg)
              `(pregexp-match ,regexp arg))]))

(define (generate-printer context type body)
  (let ([printer-name (generate-printer-name type)]
        [printer-front (generate-printer-front type)])
    (match-case body
      [(sum ?disjuncts)
       (%begin (%define printer-front
                        '(arg)
                        `(,printer-name 0 arg))
               (%define printer-name
                        '(*indent arg)
                        (%match-case 'arg
                                     (map (lambda (p)
                                            (match-case p
                                              [(product ?constructor ?refs ?repr)
                                               (let ([arguments (generate-pattern-arguments refs)]
                                                     [tag (generate-tag constructor)])
                                                 `[(,tag ,@(map (lambda (arg) ($ '? arg)) arguments))
                                                   ,(apply %string-append (generate-repr-printer context repr arguments))])]))
                                          disjuncts)
                                     `(error ,(symbol->string printer-name) "Type error" arg))))]
      [(string ?-)
       (let ([tag (generate-tag type)])
         (%define printer-name
                  '(*indent arg)
                  '(format "~a" arg)))])))

(define (generate-pattern-arguments refs)
  (map (lambda (index)
         ($ "arg" index))
       (iota (length refs))))
  
;;  (match-case ref
;;    [(ref ?type) ($ type)]
;;    [(ref-list* ?type) ($ type "*")]
;;    [(ref-list+ ?type) ($ type "+")]))

(define (generate-repr-printer context reprs arguments)
  (let loop ([code '()]
             [reprs (process-variables context reprs)]
             [args arguments]
             [indent 0])
;;    (if (null? code)
;;        (debug reprs))
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
                     (loop (cons `(**show-list (map (lambda (arg)
                                                      (,(generate-printer-name name) ,(%+ '*indent indent) arg))
                                                    ,(car args))
                                   ,(match-case separator
                                      [(nothing) ""]
                                      [(some ?sep) (car (generate-repr-printer context (list sep) '()))]
                                      [else (error "generate-repr-printer*" "Type error" separator)]))
                                 code)
                           rest-reprs
                           (cdr args)
                           indent)]
                    [(repr-variable ?name)
                     (loop (append (generate-repr-printer context (cfg/get-variable context name) '()) code)
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

(define (process-variables context reprs)
  (let ([reprs (*process-variables context reprs)])
    ;;    (debug reprs)
    (*insert-separators context reprs)))

(define (*process-variables context reprs)
  (let ([expanded (expand-variables context reprs)])
    ;;    (debug expanded)
    (if (equal? reprs expanded)
        expanded
        (*process-variables context expanded))))

(define (expand-variables context reprs)
  (reverse
   (reduce (lambda (repr reprs)
             (match-case repr
               [(repr-variable ?name)
                (let ([r (reverse (cfg/get-variable context name))])
                  (cond [(equal? name "}")
                         (append r (cons name reprs))]
                        [(or (equal? name ".") (equal? name ",") (equal? name "{") )
                         (cons name (append r reprs))]
                        [(equal? name "_")
                         (cons name reprs)]
                        [else (append r reprs)]))]
               [else (cons repr reprs)]))
           '()
           (if (pair? reprs)
               (cons '() reprs)
               '()))))

(define (*insert-separators context reprs)
  (let ([sep (reverse (cfg/get-variable context ","))]
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
     (let* ([arguments (generate-arguments refs)]
            [constructor-name (generate-constructor-name constructor)]
            [tag (generate-tag constructor)])
       (%define constructor-name
                (generate-constructor-arguments refs arguments)
                (%cond (map generate-check refs arguments)
                       `(list ',tag ,@arguments))))]))

(define (generate-constructor-name type-name)
  ($ "%" type-name))

(define (generate-predicate-name type-name)
  (let ([predefined (predefined-type? type-name)])
    (if predefined
        (cdr predefined)
        ($ "*" type-name "?"))))

(define (generate-printer-name type-name)
  ($ "*show-" type-name))

(define (generate-printer-front type-name)
  ($ "show-" type-name))

(define (generate-tag type-name)
  ($ type-name))

(define (generate-arguments refs)
  (map (lambda (index)
         ($ "arg" index))
       (iota (length refs))))

(define (generate-constructor-arguments refs args)
  (let ([len (length refs)])
    (cond [(= 0 len) '()]
          [else
           (match-case (list-ref refs (- len 1))
             [((or ref-list+ ref-list*) ?-)
              `(,@(take args (- len 1)) . ,(list-ref args (- len 1)))]
             [else args])])))

(define (generate-check ref arg)
  (match-case ref
    [(ref ?type)
     `((not (,(generate-predicate-name type) ,arg))
       (error "some-func" "Type error" ,arg))]
    [(ref-list* ?type)
     `((not (and (list? ,arg) (every? ,(generate-predicate-name type) ,arg)))
       (error "some-func" "Type error" ,arg))]
    [(ref-list+ ?type)
     `((not (and (list? ,arg) (> (length ,arg) 0) (every? ,(generate-predicate-name type) ,arg)))
       (error "some-func" "Type error" ,arg))]))

(define (generate-boilerplate)
  '(begin
     (define (*show-string *indent str)
       (format "~s" str))
     (define (*show-integer *indent int)
       (format "~a" int))
     (define (**show-list lst sep)
       (reduce (lambda (substr chunk)
                 (format "~a~a~a" chunk sep substr))
               "" lst))
     (define (*get-variable var) var)))

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

(define (%cond branches else-branch)
  (cond [(> (length branches) 0)
         `(cond ,@branches
                [else ,else-branch])]
        [else else-branch]))

(define (%match-case argument branches else-branch)
  (cond [(> (length branches) 0)
         `(match-case ,argument
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