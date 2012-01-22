;;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module transform
  (import common cfg adt)
  (export grammar->adts))

;;; Transforming grammar productions to ADTs
(define grammar->adts
  (match-lambda
    [(grammar ?constants ?definitions)
     (let ([context (transform-constants (cfg/builtin-constants) constants)])
       (map (definition->adt context) definitions))]
    [?else (error "grammar->adts" "Not a grammar" else)]))

(define (definition->adt context)
  (match-lambda
    [(bind ?name (token ?regexp))
     (%bind name (%string regexp))]
    [(bind ?name (rule ?productions))
     (%bind name (%sum (map (production->product-type context) productions)))]
    [?else (error "definition->adt" "Neither a nonterminal nor token declaration" else)]))

(define (production->product-type context)
  (match-lambda
    [(production ?constructor ?symbols)
     (%product constructor
      (map symbol->ref (filter (match-lambda
                                 [((or nonterminal nonterminal+ nonterminal*) ???-) #t]
                                 [else #f])
                        symbols))
      (optimize-reprs
       (process-constants context (map (symbol->repr context) symbols))))]
    [?else (error "production->product-type" "Not a grammar production" else)]))

(define symbol->ref
  (match-lambda
    [(nonterminal ?name) (%ref name)]
    [(nonterminal* ?name ?separator) (%ref* name)]
    [(nonterminal+ ?name ?separator) (%ref+ name)]
    [?else (error "symbol->ref" "Not a nonterminal" else)]))

(define (symbol->repr context)
  (match-lambda
    [(terminal ?value)
     (%repr-terminal value)]
    [(nonterminal ?name)
     (%repr-nonterminal name)]
    [(nonterminal+ ?name ?separator)
     (let ([sep (car (optimize-reprs (process-constants context (list ((symbol->repr context) separator)))))]) ; TODO: this is sloppy and probably buggy in some cases
       (match-case sep
         [(terminal ?value) (%repr-list name (%repr-terminal value))]
         [else (%repr-list name sep)]))]
    [(nonterminal* ?name ?separator)
     (let ([sep (car (optimize-reprs (process-constants context (list ((symbol->repr context) separator)))))]) ; TODO: need to come up with something better
       (match-case sep
         [(terminal ?value) (%repr-list name (%repr-terminal value))]
         [else (%repr-list name sep)]))]
    [(constant ?name)
     (%repr-const name)]
    [?else (error "symbol->repr" "Not a grammar symbol" else)]))

(define (transform-constants context constants)
  (if (null? constants)
      context
      (match-case (car constants)
        [(const ?name ?symbols)
         (let ([const (%const name (process-constants context (map (symbol->repr context) symbols)))])
           (transform-constants (cons const context) (cdr constants)))]
        [else (error "transform-constants" "Not a constant definition" (car constants))])))

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
  (let ([sep (reverse (map (symbol->repr context) (cfg/get-constant context ",")))] ; TODO: probably need to call process-constants here as well
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

(define (optimize-reprs reprs)
  (let loop ([reprs reprs]
             [optimized '()])
    (if (not (pair? reprs))
        (reverse optimized)
        (let ([repr (car reprs)]
              [reprs (cdr reprs)])
          (match-case repr
            ["." (loop reprs optimized)]
            [(? string?) (loop reprs (cons (%repr-const repr) optimized))]
            [(repr-terminal ?value)
             (if (pair? optimized)
                 (match-case (car optimized)
                   [(repr-terminal ?value2)
                    (loop reprs (cons (%repr-terminal (string-append value2 value)) (cdr optimized)))]
                   [else (loop reprs (cons repr optimized))])              
                 (loop reprs (cons repr optimized)))]
            [else (loop reprs (cons repr optimized))])))))