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

(define *rl-grammar*
  (regular-grammar
    ([whitespace	(or #\newline #\tab #\space)]
     [terminal		(or (: #\" (* (out #\")) #\")
                        (: #\' (* (out #\')) #\')
                        (: #\` (* (out #\`)) #\`))]
     [nonterminal	(: alpha (* alnum))]
     [constructor	(: #\( (+ (out #\))) #\))]
     [constant		(: #\$ (+ (out whitespace)))]
     [specialconst	(in #\. #\, #\{ #\} #\_)]
     [regexp		(: #\/ (* (out #\newline)) #\/)]
     [comment		(: #\% (* (out #\newline)))])

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
                (%grammar (cfg/join-constants (car binds) *cfg/default-constants*) (cdr binds)))])

    (binds
     [(bind binds) (cons bind binds)]
     [(bind) (list bind)])

    (bind
     [(CONSTANT COLON symbols SEMICOLON) (%const CONSTANT symbols)]
     [(pattern COLON bind-body)
      (let ([name (cfg/pattern-pivot pattern)])
        (%bind name
               (match-case bind-body
                 [(token ?-) bind-body]
                 [(rule ?productions)
                  (%rule (map (match-lambda
                                [(production (some ?constructor) ?symbols)
                                 (%production constructor (cfg/fit-in-pattern pattern symbols))]
                                [(production (nothing) ?symbols)
                                 (%production (generate-abstract-name name productions symbols) (cfg/fit-in-pattern pattern symbols))])
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
     [() (%nothing)]
     [(SUPER static) (%some static)])))

(define (parse input-port)
  (let ([grammar (read/lalrp *lalr-grammar* *rl-grammar* input-port)])
    (reset-eof input-port)
    grammar))

;;(define (test-parser args)
;;  (let* ([grammar (parse (open-input-file "../examples/json/json.sg"))]
;;         [adts (grammar->adts grammar)])
;;    (pp grammar)
;;    (pp adts)
;;    (generate-code adts)
;;    ))

;;(define (test-parser args)
;;  (let ([port (open-input-file "../examples/python/python.sg")])
;;    (let loop ([token (read/rp *rl-grammar* port)])
;;      (if (not (eof-object? token))
;;          (begin
;;            (print token)
;;            (loop (read/rp *rl-grammar* port)))))))

;;(define (test-parser args)
;;  (let ([port (open-input-file "../examples/json/json.sg")])
;;    (let loop ([result (read/lalrp *lalr-grammar* *rl-grammar* port)])
;;      (if (not (eof-object? result))
;;          (begin
;;            (pp result)
;;            (loop (read/rp *rl-grammar* port)))))))
