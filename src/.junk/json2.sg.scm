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
  (pregexp-match "[a-zA-Z0-9]+" arg))

(define (%object . arg0)
  (cond ((not (and (list? arg0) (every? *entry? arg0)))
         (error "some-func" "Type error" arg0))
        (else (list 'object arg0))))

(define (%array . arg0)
  (cond ((not (and (list? arg0) (every? *value? arg0)))
         (error "some-func" "Type error" arg0))
        (else (list 'array arg0))))

(define (%integer arg0)
  (cond ((not (integer? arg0))
         (error "some-func" "Type error" arg0))
        (else (list 'integer arg0))))

(define (%id arg0)
  (cond ((not (*id? arg0))
         (error "some-func" "Type error" arg0))
        (else (list 'id arg0))))

(define (%string arg0)
  (cond ((not (string? arg0))
         (error "some-func" "Type error" arg0))
        (else (list 'string arg0))))

(define (%false) (list 'false))
(define (%true) (list 'true))
(define (%null) (list 'null))
(define (*show-value *indent arg)
  (match-case
    arg
    ((null) "null")
    ((true) "true")
    ((false) "false")
    ((string ?arg0) (*show-string *indent arg0))
    ((id ?arg0) (*show-id *indent arg0))
    ((integer ?arg0) (*show-integer *indent arg0))
    ((array ?arg0)
     (string-append
       "["
       (**show-list
         (map (lambda (arg) (*show-value *indent arg))
              arg0)
         ", ")
       "]"))
    ((object ?arg0)
     (string-append
       "{\n"
       (**show-list
         (map (lambda (arg) (*show-entry (+ *indent 1) arg))
              arg0)
         ",\n")
       "\n"
       (make-string (* 4 *indent) #\space)
       "}"))
    (else (error "*show-value" "Type error" arg))))

(define (show-value arg) (*show-value 0 arg))
(define (*value? arg)
  (match-case
    arg
    ((null) #t)
    ((true) #t)
    ((false) #t)
    ((string ?-) #t)
    ((id ?-) #t)
    ((integer ?-) #t)
    ((array ?-) #t)
    ((object ?-) #t)
    (else #f)))

(define (%entry arg0 arg1)
  (cond ((not (string? arg0))
         (error "some-func" "Type error" arg0))
        ((not (*value? arg1))
         (error "some-func" "Type error" arg1))
        (else (list 'entry arg0 arg1))))

(define (*show-entry *indent arg)
  (match-case
    arg
    ((entry ?arg0 ?arg1)
     (string-append
       (make-string (* 4 *indent) #\space)
       (*show-string *indent arg0)
       ": "
       (*show-value *indent arg1)))
    (else (error "*show-entry" "Type error" arg))))

(define (show-entry arg) (*show-entry 0 arg))
(define (*entry? arg)
  (match-case arg ((entry ?- ?-) #t) (else #f)))

