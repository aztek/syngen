;;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module macros
  (export (macro define-inductive))
  )

(define-macro (define-inductive type . disjuncts)
  `(begin
     ,@(map (lambda (disjunct)
              (cond [(pair? disjunct)
                     (let ([typename (car disjunct)]
                           [fields   (cdr disjunct)])
                       `(define (,(symbol-append  '% typename) ,@fields)
                          (list ',typename ,@fields)))]
                    [else (error "define-inductive" "Malformed macro" disjunct)]))
            disjuncts)))