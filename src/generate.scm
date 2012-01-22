;;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module generate
  (import scheme haskell)
  (export *targets* *default-target* supported-target? generate-code))

(define *targets* '("scheme" "haskell"))

(define (supported-target? target)
  (member (string-downcase target) *targets*))

(define *default-target* "scheme")

(define (generate-code target module adts output-port)
  (let ([generate (match-case target
                    ["scheme" generate-scheme-code]
                    ["haskell" generate-haskell-code])])
    (display (generate module adts)
             output-port)))