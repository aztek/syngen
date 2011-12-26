(module python
  (main main)
  (include "python.sg.scm"))

(define unit
  (%unit
   (%def "fact" '("n")
         (%if (%op (%ref "n") (%equal) (%int 0))
              (%return (%int 1)))
         (%return (%op (%ref "n")
                       (%mult)
                       (%call "fact" (%op (%ref "n")
                                          (%minus)
                                          (%int 1))))))))

(define (main args)
  (display (show-unit unit)))