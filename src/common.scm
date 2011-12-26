;;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module common
  (export & &_ &- $ $_)
  (export separate)
  (export debug)
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

(define (debug . args)
  (apply fprint (cons (current-error-port) args)))