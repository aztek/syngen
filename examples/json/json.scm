(module json-test
  (main main)
  (import (json "json.sg.scm")))

(define (main args)
  (print
   (pr
    (%object (%entry "type" (%string "superhero"))
             (%entry "name" (%string "Superman"))
             (%entry "busted" (%false))
             (%entry "powers" (%array (%string "flying")
                                      (%string "infravision")))))))