;;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module parser
  (import common cfg)
  (export parse))

(define (generate-abstract-name type-name productions symbols)
  (cond [(= 1 (length productions)) type-name]
        [else
         (match-case (car symbols)
           [(terminal ?value) value]
           [(nonterminal ?name) name]
           [else
            (apply &_ (map (match-lambda
                                  [(terminal ?value) value]
                                  [(nonterminal ?name) name]
                                  [else ""])
                                symbols))])]))

(define (pattern-pivot pattern)
  (and (pair? pattern)
       (match-case (car pattern)
         [(nonterminal ?name) name]
         [else (pattern-pivot (cdr pattern))])))

(define (fit-in-pattern pattern symbols)
  (cond [(not (pair? pattern)) symbols]
        [else (match-case (car pattern)
                [(nonterminal ?-) (append symbols (cdr pattern))]
                [else (cons (car pattern) (fit-in-pattern (cdr pattern) symbols))])]))

(define *rl-grammar*
  (regular-grammar
    ([whitespace	(or #\newline #\tab #\space)]
     [terminal		(or (: #\" (* (out #\")) #\")
                        (: #\' (* (out #\')) #\')
                        (: #\` (* (out #\`)) #\`))]
     [nonterminal	(: alpha (* (in alnum #\- #\_)))]
     [constructor	(: #\( (+ (out #\))) #\))]
     [constant		(: #\$ (+ (out whitespace)))]
     [specialconst	(in #\. #\, #\{ #\} #\_)]
     [regexp		(: #\/ (* all) #\/)]
     [comment		(: #\% (* all))])

    (whitespace		(ignore))
    (comment		(ignore))
    (constant		`(CONSTANT		. ,(the-substring 1 (the-length))))
    (specialconst	`(CONSTANT		. ,(the-string)))
    (terminal		`(TERMINAL		. ,(the-substring 1 -1)))
    (constructor	`(CONSTRUCTOR	. ,(the-substring 1 -1)))
    (nonterminal	`(NONTERMINAL	. ,(the-string)))
    (regexp			`(REGEXP		. ,(the-substring 1 -1)))
    (#\:			'(COLON			. ":"))
    (#\;			'(SEMICOLON		. ";"))
    (#\|			'(PIPE			. "|"))
    (#\^			'(SUPER			. "^"))
    (#\*			'(*CLOSURE		. "*"))
    (#\+			'(+CLOSURE		. "+"))
    (#\=			'(EQUAL			. "="))
    (#\(			'(LPAREN		. "("))
    (#\)			'(RPAREN		. ")"))))

(define *lalr-grammar*
  (lalr-grammar
    ;; Terminal symbols
    (CONSTANT TERMINAL NONTERMINAL CONSTRUCTOR
     COLON SEMICOLON PIPE *CLOSURE +CLOSURE SUPER
     REGEXP EQUAL LPAREN RPAREN)

    ;; Production rules
    (unit
     [()]
     [(binds) (let ([binds (separate binds (match-lambda [(const ?- ?-) #t] [else #f]))])
                (%grammar (car binds) (reverse (cdr binds))))])

    (binds
     [(bind binds) (cons bind binds)]
     [(bind) (list bind)])

    (bind
     [(CONSTANT COLON symbols SEMICOLON) (%const CONSTANT symbols)]
     [(pattern COLON bind-body)
      (let ([name (pattern-pivot pattern)])
        (%bind name
               (match-case bind-body
                 [(token ?-) bind-body]
                 [(rule ?productions)
                  (%rule (map (match-lambda
                                [(production (some ?constructor) ?symbols)
                                 (%production constructor (fit-in-pattern pattern symbols))]
                                [(production (nothing) ?symbols)
                                 (%production (generate-abstract-name name productions symbols) (fit-in-pattern pattern symbols))])
                              productions))])))])

    (pattern
     [(static pattern) (cons static pattern)]
     [(NONTERMINAL statics) (cons (%nonterminal NONTERMINAL) statics)]
     [(NONTERMINAL) (list (%nonterminal NONTERMINAL))])
    
    (statics
     [(static statics) (cons static statics)]
     [(static) (list static)])
    
    (static
     [(TERMINAL) (%terminal TERMINAL)]
     [(CONSTANT) (%constant CONSTANT)])
    
    (bind-body
     [(REGEXP SEMICOLON) (%token REGEXP)]
     [(productions SEMICOLON)
      (%rule productions)])

    (productions
     [(production PIPE productions) (cons production productions)]
     [(production) (list production)])

    (production
     [(constructor symbols) (%production constructor symbols)])

    (constructor
     [() (%nothing)]
     [(CONSTRUCTOR) (%some CONSTRUCTOR)])

    (symbols
     [(symbol symbols) (cons symbol symbols)]
     [(symbol) (list symbol)])

    (symbol
     [(static)      static]
     [(nonterminal) nonterminal])

    (nonterminal
     [(NONTERMINAL separator +CLOSURE) (%nonterminal+ NONTERMINAL separator)]
     [(NONTERMINAL separator *CLOSURE) (%nonterminal* NONTERMINAL separator)]
     [(NONTERMINAL)                    (%nonterminal  NONTERMINAL)])
    
    (separator
     [() (%terminal "")]
     [(SUPER static) static])))

(define (parse input-port)
  (let ([grammar (read/lalrp *lalr-grammar* *rl-grammar* input-port)])
    (reset-eof input-port)
    grammar))
