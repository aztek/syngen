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
    [(nonterminal+ ?name (some ?separator))
     (%repr-list name (%some (symbol->repr separator)))]
    [(nonterminal* ?name (some ?separator))
     (%repr-list name (%some (symbol->repr separator)))]
    [(nonterminal+ ?name (nothing))
     (%repr-list name (%nothing))]
    [(nonterminal* ?name (nothing))
     (%repr-list name (%nothing))]
    [(constant ?name)
     (%repr-const name)]
    [?else (error "symbol->repr" "Type error" else)]))

(define constant->repr
  (match-lambda
    [(const ?name ?symbols)
     (%const name (map symbol->repr symbols))]
    [?else (error "constant->repr" "Not a constant definition" else)]))