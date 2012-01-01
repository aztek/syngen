(module python-test
  (main main)
  (import (python "python.sg.scm")))

(define (main args)
  (print
   (pr
    (%unit
     (list (%def "fact" '("n")
                 (list (%if (%op (%ref "n") (%equal) (%int 0))
                            (list (%return (%int 1))))
                       (%return (%op (%ref "n")
                                (%mult)
                                (%call "fact" (list (%op (%ref "n")
                                                         (%minus)
                                                         (%int 1)))))))))))))