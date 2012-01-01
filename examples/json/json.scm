(module json-test
  (main main)
  (import (json "json.sg.scm")))

(define (main args)
  (print
   (pr
    (%object (list (%entry "type" (%string "superhero"))
                   (%entry "name" (%string "Superman"))
                   (%entry "busted" (%false))
                   (%entry "powers" (%array (list (%string "flying")
                                                  (%string "infravision")))))))))