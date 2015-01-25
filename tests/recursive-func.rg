(define (func a)
  (-> int int)
  (if (= a 0)
    a
    (func (- a 1))))

(func 5)
