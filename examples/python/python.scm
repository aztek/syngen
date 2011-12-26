(module python-test
  (main main)
  (import (python "python.sg.scm")))

(define (main args)
  (print
   (pr
    (%unit
     (%def "fact" '("n")
           (%if (%op (%ref "n") (%equal) (%int 0))
                (%return (%int 1)))
           (%return (%op (%ref "n")
                         (%mult)
                         (%call "fact" (%op (%ref "n")
                                            (%minus)
                                            (%int 1))))))))))