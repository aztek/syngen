;;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module common
  (export & &_ &- &, $ $_)
  (export separate)
  (export tokenize camelcase uppercamelcase)
  (export enumerate-duplicates)
  (export %some %nothing))

(define (string-intersperse separator . strings)
  (reduce (lambda (arg substr) (format "~a~a~a" substr separator arg))
          "" strings))

(define (& . args)
  (apply string-intersperse (cons "" args)))

(define (&_ . args)
  (apply string-intersperse (cons " " args)))

(define (&- . args)
  (apply string-intersperse (cons "-" args)))

(define (&, . args)
  (apply string-intersperse (cons ", " args)))

(define ($ . args)
  (string->symbol (apply & args)))

(define ($_ . args)
  (string->symbol (apply &_ args)))

(define (%some value) (list 'some value))
(define (%nothing) (list 'nothing))

(define (separate lst pred)
  (let loop ([lst lst]
             [result '(() . ())])
    (if (pair? lst)
        (if (pred (car lst))
            (loop (cdr lst) (cons (cons (car lst) (car result)) (cdr result)))
            (loop (cdr lst) (cons (car result) (cons (car lst) (cdr result)))))
        result)))

(define *name-tokenizer-grammar*
  (regular-grammar ()
   (define *tokens* '())
   [(+ alpha) (set! *tokens* (cons (string-downcase (the-string)) *tokens*))
              (ignore)]
   [(out alpha) (ignore)]
   [else *tokens*]))

(define (tokenize name)
  (with-input-from-string name
   (lambda ()
     (reverse (read/rp *name-tokenizer-grammar* (current-input-port))))))

(define (camelcase name)
  (let ([tokens (tokenize name)])
    (apply & (cons (car tokens) (map string-capitalize (cdr tokens))))))

(define (uppercamelcase name)
  (apply & (map string-capitalize (tokenize name))))

(define (enumerate-duplicates args)
  (let loop ([args args]
             [enumerated '()]
             [counts '()])
    (cond [(not (pair? args)) (reverse enumerated)]
          [(let ([arg (car args)]
                 [args (cdr args)])
            (let* ([pair (assoc arg counts)]
                   [count (if (not pair) 1 (+ 1 (cdr pair)))]
                   [counts (cons (cons arg count) counts)])
                (let ([arg (if (or (member arg args) (> count 1))
                               (string-append arg (number->string count))
                               arg)])
                  (loop args
                        (cons arg enumerated)
                        counts))))])))