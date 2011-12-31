;;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module transform
  (import common cfg adt)
  (export grammar->adts))

;;; Transforming grammar productions to ADTs
(define grammar->adts
  (match-lambda
    [(grammar ?constants ?definitions)
     (%adts (map constant->repr constants)
            (map definition->adt definitions))]))

(define definition->adt
  (match-lambda
    [(bind ?name (token ?regexp))
     (%bind name (%string regexp))]
    [(bind ?name (rule ?productions))
     (%bind name (%sum (map production->product-type productions)))]
    [?else (error "definition->adt" "Neither a nonterminal nor token declaration" else)]))

(define production->product-type
  (match-lambda
    [(production ?constructor ?symbols)
     (%product constructor
               (map symbol->ref (filter cfg/nonterminal-symbol? symbols))
               (map symbol->repr symbols))]))

(define symbol->ref
  (match-lambda
    [(nonterminal ?name) (%ref name)]
    [(nonterminal* ?name ?separator) (%ref* name)]
    [(nonterminal+ ?name ?separator) (%ref+ name)]
    [?else (error "symbol->ref" "Not a nonterminal" else)]))

(define symbol->repr
  (match-lambda
    [(terminal ?value)
     (%repr-terminal value)]
    [(nonterminal ?name)
     (%repr-nonterminal name)]
    [(nonterminal+ ?name ?separator)
     (%repr-list name (symbol->repr separator))]
    [(nonterminal* ?name ?separator)
     (%repr-list name (symbol->repr separator))]
    [(constant ?name)
     (%repr-const name)]
    [?else (error "symbol->repr" "Not a grammar symbol" else)]))

(define constant->repr
  (match-lambda
    [(const ?name ?symbols)
     (%const name (map symbol->repr symbols))]
    [?else (error "constant->repr" "Not a constant definition" else)]))