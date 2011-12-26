(module json
  (main main)
  (include "json2.sg.scm"))

(define superhero
  (%object (%entry "name" (%string "john doe"))
           (%entry "age" (%integer 20))
           (%entry "superpowers" (%object (%entry "flying" (%false))
                                          (%entry "infravision" (%true))))
           (%entry "busted" (%false))))

(define (main args)
  (display (show-value superhero)))