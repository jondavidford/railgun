((define (func x)
   (-> int int)
   (+ x 1))
 (print (map func (collection (1 2 3 4 5)))))
