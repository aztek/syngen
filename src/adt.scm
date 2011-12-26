;;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module adt
  (export %adts)
  (export %product %sum %string)
  (export %ref %ref* %ref+)
  (export %repr-terminal %repr-nonterminal %repr-list %repr-const))

(define (%adts constants types) (list 'adts constants types))

;; Inductive types
(define (%product constructor refs representation) (list 'product constructor refs representation))
(define (%sum disjuncts) (list 'sum disjuncts))
(define (%string regexp) (list 'string regexp))

;; Type reference
(define (%ref type) (list 'ref type))
(define (%ref* type) (list 'ref* type))
(define (%ref+ type) (list 'ref+ type))

;; Value representation
(define (%repr-terminal value) (list 'repr-terminal value))
(define (%repr-nonterminal name) (list 'repr-nonterminal name))
(define (%repr-list name separator) (list 'repr-list name separator))
(define (%repr-const name) (list 'repr-const name))