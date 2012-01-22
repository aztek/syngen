;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module cfg
  (export %grammar %bind %const %rule %token %production)
  (export %terminal %nonterminal %nonterminal+ %nonterminal* %constant)
  (export cfg/builtin-constants cfg/get-constant))

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

;; Grammar constants
(define (cfg/builtin-constants)
  (list (%const "." (list))
        (%const "," (list (%terminal " ")))
        (%const "{" (list (%terminal "{\n")))
        (%const "}" (list (%terminal "}\n")))
        (%const "_" (list (%terminal "    ")))))

(define (cfg/get-constant constants const)
  (and (pair? constants)
       (match-case (car constants)
         [(const (? (lambda (name) (equal? name const))) ?value) value]
         [else (cfg/get-constant (cdr constants) const)])))