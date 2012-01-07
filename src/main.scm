;;;; 2011, Evgeny Kotelnikov <evgeny.kotelnikov@gmail.com>

(module syngen
  (main main)
  (import common cfg adt transform parser generate))

(define *syngen-version* "0.0.1")

(define *type-namespace* "json-")
(define *type-constructor-namespace* "%")

(define (main argv)
  (let ([*target* *default-target*]
        [*module* "generated"]
        [*input* #f]
        [*output* #f]
        [*namespace* *type-namespace*])
    (args-parse (cdr argv)
      (section "Input and Output")
      [(("-i" "--input") ?input (help "Input file"))
       (set! *input* input)]
      [(("-o" "--output") ?output (help "Output file"))
       (set! *output* output)]
      (section "Code generation")
      [("-t" ?target (help "Target programming language"))
       (set! *target* target)]
      [("-m" ?module (help "Name of generated module"))
       (set! *module* module)]
      (section "Misc")
      [(("-v" "--version") (help "Version number"))
       (print *syngen-version*)]
      (section "Help")
      [(("-h" "--help" "?") (help "?,-h,--help" "This help message"))
       (args-parse-usage #f)]
      [else
       (print "Illegal argument '" else "'. Usage:")
       (args-parse-usage #f)])

    (cond [(not (supported-target? *target*))
           (fprintf (current-error-port) "`~a' is currently not supported. Supported languages are Scheme~%" *target* *targets*)]
          [(not *input*)
           (fprintf (current-error-port) "Input file is not specified. Use -i flag.")]
          [else
           (let ([input-port  (open-input-file *input*)]
                 [output-port (if *output* (open-output-file *output*) (current-output-port))])
             (cond [(not input-port)
                    (fprintf (current-error-port) "Unable to open ~s for reading.~%" *input*)]
                   [(and (not output-port) *output*)
                    (fprintf (current-error-port) "Unable to open `~s' for writing.~%" *output*)]
                   [(not output-port)
                    (fprintf (current-error-port) "Unable to open standart output port.~%")]
                   [else (let* ([grammar (parse input-port)]
                                [adts (grammar->adts grammar)])
                           (generate-code *target* *module* adts output-port)
                           (close-output-port output-port))]))])))