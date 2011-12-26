(define (*show-string *indent str)
  (format "~s" str))

(define (*show-integer *indent int)
  (format "~a" int))

(define (**show-list lst sep)
  (reduce
    (lambda (substr chunk)
       (format "~a~a~a" chunk sep substr))
    ""
    lst))

(define (*get-variable var) var)
(define (*show-id *indent arg) (format "~a" arg))
(define (*id? arg)
  (pregexp-match "[a-zA-Z_][a-zA-Z_0-9]*" arg))

(define (%unit . arg0)
  (cond ((not (and (list? arg0)
                   (> (length arg0) 0)
                   (every? *stmnt? arg0)))
         (error "some-func" "Type error" arg0))
        (else (list 'unit arg0))))

(define (*show-unit *indent arg)
  (match-case
    arg
    ((unit ?arg0)
     (**show-list
       (map (lambda (arg) (*show-stmnt *indent arg))
            arg0)
       ""))
    (else (error "*show-unit" "Type error" arg))))

(define (show-unit arg) (*show-unit 0 arg))
(define (*unit? arg)
  (match-case arg ((unit ?-) #t) (else #f)))

(define (%if arg0 . arg1)
  (cond ((not (*expr? arg0))
         (error "some-func" "Type error" arg0))
        ((not (and (list? arg1)
                   (> (length arg1) 0)
                   (every? *stmnt? arg1)))
         (error "some-func" "Type error" arg1))
        (else (list 'if arg0 arg1))))

(define (%var arg0 arg1)
  (cond ((not (*id? arg0))
         (error "some-func" "Type error" arg0))
        ((not (*expr? arg1))
         (error "some-func" "Type error" arg1))
        (else (list 'var arg0 arg1))))

(define (%return arg0)
  (cond ((not (*expr? arg0))
         (error "some-func" "Type error" arg0))
        (else (list 'return arg0))))

(define (%def arg0 arg1 . arg2)
  (cond ((not (*id? arg0))
         (error "some-func" "Type error" arg0))
        ((not (and (list? arg1) (every? *id? arg1)))
         (error "some-func" "Type error" arg1))
        ((not (and (list? arg2)
                   (> (length arg2) 0)
                   (every? *stmnt? arg2)))
         (error "some-func" "Type error" arg2))
        (else (list 'def arg0 arg1 arg2))))

(define (*show-stmnt *indent arg)
  (match-case
    arg
    ((def ?arg0 ?arg1 ?arg2)
     (string-append
       (make-string (* 4 *indent) #\space)
       "def "
       (*show-id *indent arg0)
       "("
       (**show-list
         (map (lambda (arg) (*show-id *indent arg)) arg1)
         ", ")
       "):\n"
       (**show-list
         (map (lambda (arg) (*show-stmnt (+ *indent 1) arg))
              arg2)
         "")
       "\n"))
    ((return ?arg0)
     (string-append
       (make-string (* 4 *indent) #\space)
       "return "
       (*show-expr *indent arg0)
       "\n"))
    ((var ?arg0 ?arg1)
     (string-append
       (make-string (* 4 *indent) #\space)
       (*show-id *indent arg0)
       " = "
       (*show-expr *indent arg1)
       "\n"))
    ((if ?arg0 ?arg1)
     (string-append
       (make-string (* 4 *indent) #\space)
       "if "
       (*show-expr *indent arg0)
       ":\n"
       (**show-list
         (map (lambda (arg) (*show-stmnt (+ *indent 1) arg))
              arg1)
         "")
       "\n"))
    (else (error "*show-stmnt" "Type error" arg))))

(define (show-stmnt arg) (*show-stmnt 0 arg))
(define (*stmnt? arg)
  (match-case
    arg
    ((def ?- ?- ?-) #t)
    ((return ?-) #t)
    ((var ?- ?-) #t)
    ((if ?- ?-) #t)
    (else #f)))

(define (%call arg0 . arg1)
  (cond ((not (*id? arg0))
         (error "some-func" "Type error" arg0))
        ((not (and (list? arg1) (every? *expr? arg1)))
         (error "some-func" "Type error" arg1))
        (else (list 'call arg0 arg1))))

(define (%op arg0 arg1 arg2)
  (cond ((not (*expr? arg0))
         (error "some-func" "Type error" arg0))
        ((not (*op? arg1))
         (error "some-func" "Type error" arg1))
        ((not (*expr? arg2))
         (error "some-func" "Type error" arg2))
        (else (list 'op arg0 arg1 arg2))))

(define (%int arg0)
  (cond ((not (integer? arg0))
         (error "some-func" "Type error" arg0))
        (else (list 'int arg0))))

(define (%ref arg0)
  (cond ((not (*id? arg0))
         (error "some-func" "Type error" arg0))
        (else (list 'ref arg0))))

(define (*show-expr *indent arg)
  (match-case
    arg
    ((ref ?arg0) (*show-id *indent arg0))
    ((int ?arg0) (*show-integer *indent arg0))
    ((op ?arg0 ?arg1 ?arg2)
     (string-append
       (*show-expr *indent arg0)
       " "
       (*show-op *indent arg1)
       " "
       (*show-expr *indent arg2)))
    ((call ?arg0 ?arg1)
     (string-append
       (*show-id *indent arg0)
       "("
       (**show-list
         (map (lambda (arg) (*show-expr *indent arg))
              arg1)
         ", ")
       ")"))
    (else (error "*show-expr" "Type error" arg))))

(define (show-expr arg) (*show-expr 0 arg))
(define (*expr? arg)
  (match-case
    arg
    ((ref ?-) #t)
    ((int ?-) #t)
    ((op ?- ?- ?-) #t)
    ((call ?- ?-) #t)
    (else #f)))

(define (%mult) (list 'mult))
(define (%minus) (list 'minus))
(define (%plus) (list 'plus))
(define (%equal) (list 'equal))
(define (*show-op *indent arg)
  (match-case
    arg
    ((equal) "==")
    ((plus) "+")
    ((minus) "-")
    ((mult) "*")
    (else (error "*show-op" "Type error" arg))))

(define (show-op arg) (*show-op 0 arg))
(define (*op? arg)
  (match-case
    arg
    ((equal) #t)
    ((plus) #t)
    ((minus) #t)
    ((mult) #t)
    (else #f)))

