;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module cfg
  (import common)
  (export %grammar %bind %const %rule %token %production)
  (export %terminal %nonterminal %nonterminal+ %nonterminal* %constant)
  (export cfg/nonterminal-symbol?)
  (export cfg/join-constants cfg/pattern-pivot *cfg/default-constants* cfg/get-constant cfg/fit-in-pattern))

(define (%grammar constants definitions) (list 'grammar constants definitions))

(define (%bind name rule) (list 'bind name rule))

(define (%const name symbols) (list 'const name symbols))

;; Rule
(define (%rule productions) (list 'rule productions))
(define (%token regexp) (list 'token regexp))

;; Productions
(define (%production constructor symbols) (list 'production constructor symbols))

(define (%terminal value) (list 'terminal value))
(define (%nonterminal name) (list 'nonterminal name))
(define (%nonterminal+ name separator) (list 'nonterminal+ name separator))
(define (%nonterminal* name separator) (list 'nonterminal* name separator))
(define (%constant name) (list 'constant name))

(define cfg/terminal-symbol?
  (match-lambda
   [(terminal ???-) #t]
   [else #f]))

(define cfg/nonterminal-symbol?
  (match-lambda
    [(nonterminal  ???-) #t]
    [(nonterminal+ ???-) #t]
    [(nonterminal* ???-) #t]
    [else #f]))

;; Grammar constants
;(define (cfg/special-constant? constant)
;  (match-case constant
;   [(constant (or "{" "_" "}")) #t]
;   [else #f]))

(define (cfg/joining-symbol? symb)
  (match-case symb
   [(constant ".") #t]
   [else #f]))

;; Special constants
(define *cfg/default-constants*
  (list (%const "." (list))
        (%const "," (list (%terminal " ")))
        (%const "{" (list (%terminal "{\n")))
        (%const "}" (list (%terminal "}\n")))
        (%const "_" (list (%terminal "    ")))))

(define (cfg/special-constant? const)
  (or (equal? const ".")
      (equal? const ",")
      (equal? const "{")
      (equal? const "}")
      (equal? const "_")))

(define (cfg/join-constants specific-constants common-constants)
  (append specific-constants common-constants))

(define (cfg/get-constant constants const)
  (and (pair? constants)
       (match-case (car constants)
         [(const ?name ?value)
          (if (equal? name const)
              value
              (cfg/get-constant (cdr constants) const))]
         [else #f])))

(define cfg/production-nonterminals
  (match-lambda
   [(production ?- ?symbols ?-) (filter cfg/nonterminal-symbol? symbols)]
   [?else (error "cfg/production-nonterminals" "Not a grammar production" else)]))

(define (cfg/pattern-pivot pattern)
  (and (pair? pattern)
       (match-case (car pattern)
         [(nonterminal ?name) name]
         [else (cfg/pattern-pivot (cdr pattern))])))

(define (cfg/fit-in-pattern pattern symbols)
  (cond [(not (pair? pattern)) symbols]
        [else (match-case (car pattern)
                [(nonterminal ?-) (append symbols (cdr pattern))]
                [else (cons (car pattern) (cfg/fit-in-pattern (cdr pattern) symbols))])]))