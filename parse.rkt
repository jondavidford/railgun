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

(struct namecontract (name contract))
(struct nametype (name type))

(define (get-funcontext body)
  (define (strip-funcontext expr)
    (match expr
      [`(define (,func ,args ...)
        ;contract list length is args list length plus one
        (-> ,contract ...)
        ,body ...)
       `(,(namecontract func contract))]
      [else
       `()]))
  (apply append (map strip-funcontext body)))

(define (get-varcontext expr)
  (match expr
    [`(define ,type ,var ,expr)
     `(,(nametype var type))]
    [else
     `()]))

(define (get-namecontract funname funcontext)
  (unless (not (empty? funcontext))
    (error 'parse-error "reference to undefined function"))
  (if (eq? (namecontract-name (first funcontext)) funname)
      (first funcontext)
      (get-namecontract funname (rest funcontext))))

(define (parse body)
  (define body-context (get-funcontext body))
  (for/fold ([varcontext '()]
             [parsed '()])
            ([expr body])
    (values (append varcontext (get-varcontext expr))
            (append parsed `(,(parse-type expr body-context))))))

(define (parse-type expr funcontext)
  (match expr
    [(or `(+ ,operands ..2)
         `(- ,operands ..2)
         `(* ,operands ..2)
         `(/ ,operands ..2)
         `(% ,operands ..2))
     ;more type checking here required
     (define typed-operands (map (lambda (x) (parse-type x funcontext)) operands))
     (define type
       (if (ormap (lambda (op) (eq? (get-type op) 'float)) typed-operands)
           'float
           'int))
     (arith type (first expr) (map (lambda (x) (parse-type x funcontext)) operands))]
    [(or `(= ,op1 ,op2)
         `(<= ,op1 ,op2)
         `(>= ,op1 ,op2)
         `(< ,op1 ,op2)
         `(> ,op1 ,op2))
     (comp 'bool (first expr) `(,(parse-type op1 funcontext) ,(parse-type op2 funcontext)))]
    [`(cond (,preds ,bodies ...) ...)
     (define perbody-funcontext (map get-funcontext bodies))
     (define typed-bodies 
       (map (lambda (body context) (parse-type (last body) (append context funcontext)))
        bodies 
        perbody-funcontext))
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
     (unless (symbol=? (last contract) (get-type (parse-type (last body) funcontext)))
       (error 'type-error "function return type does not match contract"))
     (define body-funcontext (get-funcontext body))
     (deffun func args contract (map (lambda (x) (parse-type x (append body-funcontext funcontext))) body))]
    [`(define ,type ,var ,expr)
     (define typed-expr (parse-type expr funcontext))
     (unless (eq? (get-type typed-expr) type)
       (error 'type-error "expression does not match define type"))
     (defvar type var typed-expr)]
    [`(if ,pred ,then ,else)
     (define typed-then (parse-type then funcontext))
     (define typed-else (parse-type else funcontext))
     (define typed-pred (parse-type pred funcontext))
     ;; NOTE: should there also be a check here that the types are not void? i.e., they are defines
     (unless (eq? (get-type typed-then) (get-type typed-else))
       (error 'type-error "if requires both expressions to return the same type"))
     (if (eq? 'bool (get-type typed-pred))
         (if-e (get-type typed-then) typed-pred typed-then typed-else)
         (error 'type-error "if requires predicate to be a boolean expression"))]
    [`(map ,func ,collection)
     void]
    [`(print ,exp)
     (funcall 'void print (parse-type exp funcontext))]
    [`(,func ,operands ...)
     (funcall (last (namecontract-contract (get-namecontract func funcontext))) func (map (lambda (x) (parse-type x funcontext)) operands))]
    [x
     (cond
       [(and (boolean? x))
        (immed 'bool x)]
       [(and (number? x) (regexp-match #rx"[:digit:]*" (number->string x)))
        (immed 'int x)]
       [(and (symbol? x) (regexp-match #rx"[:digit:]*.[:digit:]*f" (symbol->string x)))
        (immed 'float x)]
       [(symbol? x)
        (immed 'varthisisnottherealtypetho x)]
       [else
        (error 'parse-type "unspecified parse-type error")])]))

(parse `((define int x 1)
         (define int y 1)
         y))

;; test cases
(check-expect (get-type (parse-type `(+ 1 2) '())) 'int)
(check-expect (get-type (parse-type `(- 1 2) '())) 'int)
(check-expect (get-type (parse-type `(* 1 2) '())) 'int)
(check-expect (get-type (parse-type `(/ 1 2) '())) 'int)

(check-expect (get-type (parse-type `(= 1 2) '())) 'bool)
(check-expect (get-type (parse-type `(< 1 2) '())) 'bool)
(check-expect (get-type (parse-type `(<= 1 2) '())) 'bool)
(check-expect (get-type (parse-type `(> 1 2) '())) 'bool)
(check-expect (get-type (parse-type `(>= 1 2) '())) 'bool)

(check-expect (get-type (parse-type `(+ 1.0f 2 4 6 7) '())) 'float)
(check-expect (get-type (parse-type `(* 1 2.0f 3 4 1.2f) '())) 'float)
(check-expect (get-type (parse-type `(- 1.0f 2 5 99) '())) 'float)
(check-expect (get-type (parse-type `(/ 1.0f 2.0f 2.3f) '())) 'float)

(check-expect (get-type (parse-type `(cond
                                       [(= 1 2) 5]
                                       [(= 1 1) 2]) '())) 'int)
(check-error (get-type (parse-type `(cond
                                      [(= 1 2) 5]
                                      [(= 1 1) #t]) '())) "type-error: cond requires all bodies to return the same type")

(check-expect (get-type (parse-type `(define (func a b)
                                      (-> int int int)
                                      (+ 1 1)) '())) 'void)
(check-error (get-type (parse-type `(define (func a b)
                                      (-> int int)
                                      (+ 1 1)) '())) "type-error: contract length does not match function declaration")

(check-expect (get-type (parse-type `(define int a 1) '())) 'void)
(check-expect (get-type (parse-type `(define float a 2.0f) '())) 'void)
(check-expect (get-type (parse-type `(define bool a #f) '())) 'void)
(check-error (get-type (parse-type `(define int a 2.0f) '())) "type-error: expression does not match define type")

(check-expect (get-type (parse-type `(print 1) '())) 'void)

(check-expect (get-type (parse-type `(func 1) `(,(namecontract 'func '(int int))))) 'int)
                                                 
(check-expect (get-type (parse-type `(if (= 1 1)
                                         (+ 1 1)
                                         (+ 1 1)) '())) 'int)
(check-expect (get-type (parse-type `(if (= 1.0f 1.0f)
                                         (+ 1.0f 10)
                                         (* 1.0f 3)) '())) 'float)
(check-expect (get-type (parse-type `(if #t
                                         #t
                                         #f) '())) 'bool)
(check-error (get-type (parse-type `(if #t
                                        #t
                                        1.0f) '())) "type-error: if requires both expressions to return the same type")

(test)



                                   