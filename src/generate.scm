;;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module generate
  (import scheme haskell)
  (export *targets* *default-target* supported-target? generate-code))

(define *targets* '("scheme" "haskell"))

(define (supported-target? target)
  (member (string-downcase target) *targets*))

(define *default-target* "scheme")

(define (generate-code target module adts output-port)
  (let ([generator (match-case target
                     ["scheme" generate-scheme-code]
                     ["haskell" generate-haskell-code])])
    (display (generator module adts)
             output-port)))