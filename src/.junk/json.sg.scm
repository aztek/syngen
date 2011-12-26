;; This file was automatically generated by syngen

(module json
  (main main))

(define (print-string arg)
  (format "~s" arg))

(define (print-integer arg)
  (format "~a" arg))

(define (*value? value)
  (match-case value
    [(null) #t]
    [(true) #t]
    [(false) #t]
    [(string ?-) #t]
    [(integer ?-) #t]
    [(array ?-) #t]
    [(object ?-) #t]
    [else #f]))

(define (print-value value)
  (match-case value
    [(null) "null"]
    [(true) "true"]
    [(false) "false"]
    [(string ?arg) (print-string arg)]
    [(integer ?arg) (print-integer arg)]
    [(array ?value*) (string-append "[" (reduce (lambda (chunk substr) (format "~a, ~a" substr chunk)) "" (map print-value value*)) "]")]
    [(object ?entry*) (string-append "{" (reduce (lambda (chunk substr) (format "~a, ~a" substr chunk)) "" (map print-entry entry*)) "}")]
    [else (error "print-value" "Type error, `value' is expected" value)]))

(define (%null)
  (list 'null))

(define (%true)
  (list 'true))

(define (%false)
  (list 'false))

(define (%string arg)
  (if (string? arg)
      (list 'string arg)
      (error "%string" "Type error, Scheme string is expected" arg)))

(define (%integer arg)
  (if (number? arg)
      (list 'integer arg)
      (error "%intger" "Type error, Scheme number is expected" arg)))

(define (%array . value*)
  (if (and (list? value*) (every? *value? value*))
      (list 'array value*)
      (error "%array" "Type error, `value' is expected" value*)))

(define (%object . entry*)
  (if (and (list? entry*) (every? *entry? entry*))
      (list 'object entry*)
      (error "%object" "Type error, `entry' is expected" entry*)))

(define (*entry? value)
  (match-case value
    [(entry ?- ?-) #t]
    [else #f]))

(define (%entry arg1 arg2)
  (cond [(not (string? arg1))
         (error "%entry" "Type error, Scheme string is expected" arg1)]
        [(not (*value? arg2))
         (error "%entry" "Type error, `value' is expected" arg2)]
        [else (list 'entry arg1 arg2)]))

(define (print-entry entry)
  (match-case entry
    [(entry ?string ?value) (string-append (print-string string) ": " (print-value value))]
    [else (error "print-entry" "Type error, `entry' is expected" entry)]))





(define superhero
  (%object (%entry "name" (%string "john doe"))
           (%entry "age" (%integer 20))
           (%entry "superpowers" (%array (%string "flying") (%string "infravision")))
           (%entry "busted" (%false))))

(define (main args)
  (display (print-value superhero)))