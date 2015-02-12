((define (func a)
   (-> int int)
   (cond
     [(= a 0) a]
     [else (print a)
           (func (- a 1))]))
 (func 5))
