#lang racket
(require test-engine/racket-tests)

(provide parse-type)

(provide deffun)
(provide defvar)
(provide cond-e)
(provide if-e)
(provide arith)
(provide comp)
(provide funcall)
(provide map-e)
(provide immed)

(struct deffun (name arguments contract body))
(struct defvar (name type expr))
(struct cond-e (type preds bodies))
(struct if-e (type pred then else))
(struct arith (type op arguments))
(struct comp (type op arguments))
(struct funcall (type name arguments))
(struct map-e (type fun collection))
(struct immed (type val))

(define (get-type expr)
  (match expr
    [(struct deffun (name arguments contract body))
     'void]
    [(struct defvar (vartype name expr))
     'void]
    [(struct cond-e (type preds bodies))
     type]
    [(struct if-e (type pred then else))
     type]
    [(struct arith (type op arguments))
     type]
    [(struct comp (type op arguments))
     type]
    [(struct funcall (type name arguments))
     type]
    [(struct map-e (type fun collection))
     type]
    [(struct immed (type val))
     type]))
     

(define (parse-type expr)
  (match expr
    [(or `(+ ,operands ..2)
         `(- ,operands ..2)
         `(* ,operands ..2)
         `(/ ,operands ..2)
         `(% ,operands ..2))
     ;more type checking here required
     (define typed-operands (map parse-type operands))
     (define type
       (if (ormap (lambda (op) (eq? (get-type op) 'float)) typed-operands)
           'float
           'int))
     (arith type (first expr) (map parse-type operands))]
    [(or `(= ,op1 ,op2)
         `(<= ,op1 ,op2)
         `(>= ,op1 ,op2)
         `(< ,op1 ,op2)
         `(> ,op1 ,op2))
     (comp 'bool (first expr) `(,(parse-type op1) ,(parse-type op2)))]
    [`(cond (,preds ,bodies ...) ...)
     (define typed-bodies (map (lambda (body) (parse-type (last body))) bodies))
     (define uniform-type? 
       (apply (lambda (x y) (eq? (get-type x) (get-type y))) typed-bodies))
     ;; NOTE: should there also be a check here that the types are not void? i.e., they are defines
     (if uniform-type?
         (cond-e (get-type (first typed-bodies)) preds bodies)
         (error 'type-error "cond requires all bodies to return the same type"))]
    [`(define (,func ,args ...)
        ;contract list length is args list length plus one
        (-> ,contract ...)
        ,body ...)
     (unless (= (add1 (length args)) (length contract))
       (error 'type-error "contract length does not match function declaration"))
     (unless (symbol=? (last contract) (get-type (parse-type (last body))))
       (error 'type-error "function return type does not match contract"))
     (deffun func args contract (map parse-type body))]
    [`(define ,type ,var ,expr)
     (define typed-expr (parse-type expr))
     (unless (eq? (get-type typed-expr) type)
       (error 'type-error "expression does not match define type"))
     (defvar type var typed-expr)]
    [`(if ,pred ,then ,else)
     (define typed-then (parse-type then))
     (define typed-else (parse-type else))
     (define typed-pred (parse-type pred))
     ;; NOTE: should there also be a check here that the types are not void? i.e., they are defines
     (unless (eq? (get-type typed-then) (get-type typed-else))
       (error 'type-error "if requires both expressions to return the same type"))
     (if (eq? 'bool (get-type typed-pred))
         (if-e (get-type typed-then) typed-pred typed-then typed-else)
         (error 'type-error "if requires predicate to be a boolean expression"))]
    [`(map ,func ,collection)
     void]
    [`(print ,exp)
     (funcall 'void print (parse-type exp))]
    [`(,func ,operands ...)
     (define type (get-type (parse-type (last operands))))
     (funcall type func (map parse-type operands))]
    [x
     (cond
       [(and (boolean? x))
        (immed 'bool x)]
       [(and (number? x) (regexp-match #rx"[:digit:]*" (number->string x)))
         (immed 'int x)]
       [(and (symbol? x) (regexp-match #rx"[:digit:]*.[:digit:]*f" (symbol->string x)))
        (immed 'float x)]
       [else
        (error 'parse-type "unspecified parse-type error")])]))

;; test cases
(check-expect (get-type (parse-type `(+ 1 2))) 'int)
(check-expect (get-type (parse-type `(- 1 2))) 'int)
(check-expect (get-type (parse-type `(* 1 2))) 'int)
(check-expect (get-type (parse-type `(/ 1 2))) 'int)

(check-expect (get-type (parse-type `(= 1 2))) 'bool)
(check-expect (get-type (parse-type `(< 1 2))) 'bool)
(check-expect (get-type (parse-type `(<= 1 2))) 'bool)
(check-expect (get-type (parse-type `(> 1 2))) 'bool)
(check-expect (get-type (parse-type `(>= 1 2))) 'bool)

(check-expect (get-type (parse-type `(+ 1.0f 2 4 6 7))) 'float)
(check-expect (get-type (parse-type `(* 1 2.0f 3 4 1.2f))) 'float)
(check-expect (get-type (parse-type `(- 1.0f 2 5 99))) 'float)
(check-expect (get-type (parse-type `(/ 1.0f 2.0f 2.3f))) 'float)

(check-expect (get-type (parse-type `(cond
                                       [(= 1 2) 5]
                                       [(= 1 1) 2]))) 'int)
(check-error (get-type (parse-type `(cond
                                      [(= 1 2) 5]
                                      [(= 1 1) #t]))) "type-error: cond requires all bodies to return the same type")

(check-expect (get-type (parse-type `(define (func a b)
                                      (-> int int int)
                                      (+ 1 1)))) 'void)
(check-error (get-type (parse-type `(define (func a b)
                                      (-> int int)
                                      (+ 1 1)))) "type-error: contract length does not match function declaration")

(check-expect (get-type (parse-type `(define int a 1))) 'void)
(check-expect (get-type (parse-type `(define float a 2.0f))) 'void)
(check-expect (get-type (parse-type `(define bool a #f))) 'void)
(check-error (get-type (parse-type `(define int a 2.0f))) "type-error: expression does not match define type")

(check-expect (get-type (parse-type `(print 1))) 'void)
                                                 
(check-expect (get-type (parse-type `(if (= 1 1)
                                         (+ 1 1)
                                         (+ 1 1)))) 'int)
(check-expect (get-type (parse-type `(if (= 1.0f 1.0f)
                                         (+ 1.0f 10)
                                         (* 1.0f 3)))) 'float)
(check-expect (get-type (parse-type `(if #t
                                         #t
                                         #f))) 'bool)
(check-error (get-type (parse-type `(if #t
                                        #t
                                        1.0f))) "type-error: if requires both expressions to return the same type")

(test)



                                   