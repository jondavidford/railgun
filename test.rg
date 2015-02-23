(
(define (add1 x) (-> int int) (+ x 1))
(define (func a) (-> <int> <int>) (map add1 a))
(func (collection (1 2 3 4)))
)
