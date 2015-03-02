#lang racket
(require test-engine/racket-tests)

(provide parse)
(provide get-type)
(provide get-output)
(provide look-in-collection)

(provide line)
(provide return)
(provide deffun)
(provide defvar)
(provide cond-e)
(provide if-e)
(provide arith)
(provide comp)
(provide funcall)
(provide map-e)
(provide immed)
(provide print-e)
(provide collection)

(struct return (expr) #:transparent)
(struct line (expr) #:transparent)
(struct deffun (name arguments contract body) #:transparent)
(struct defvar (name type expr) #:transparent)
(struct cond-e (type output preds bodies) #:transparent)
(struct if-e (type output red then else) #:transparent)
(struct arith (type op arguments) #:transparent)
(struct comp (type op arguments) #:transparent)
(struct funcall (type output name arguments) #:transparent)
(struct map-e (type output fun collection) #:transparent)
(struct immed (type val) #:transparent)
(struct print-e (argument) #:transparent)
(struct collection (type output elements) #:transparent)
(struct lambda-e (name arguments contract body) #:transparent)

(define (get-type expr)
  (match expr
    [(struct deffun (name arguments contract body))
     'void]
    [(struct defvar (vartype name expr))
     'void]
    [(struct cond-e (type output preds bodies))
     type]
    [(struct if-e (type output pred then else))
     type]
    [(struct arith (type op arguments))
     type]
    [(struct comp (type op arguments))
     type]
    [(struct funcall (type output name arguments))
     type]
    [(struct map-e (type output fun collection))
     type]
    [(struct immed (type val))
     type]
    [(struct print-e (argument))
     'void]
    [(struct collection (type output elements))
     type]
    [(struct lambda-e (name arguments contract body))
     'void]))

(define (get-output expr)
  (match expr
    [(struct deffun (name arguments contract body))
     'void]
    [(struct defvar (vartype name expr))
     'void]
    [(struct cond-e (type output preds bodies))
     output]
    [(struct if-e (type output pred then else))
     output]
    [(struct arith (type op arguments))
     'void]
    [(struct comp (type op arguments))
     'void]
    [(struct funcall (type output name arguments))
     output]
    [(struct map-e (type output fun collection))
     output]
    [(struct immed (type val))
     'void]
    [(struct print-e (argument))
     'void]
    [(struct collection (type output elements))
     output]
    [(struct lambda-e (name arguments contract body))
     'void]))

(define output-counter 0)
(define (generate-output prefix)
  (set! output-counter (add1 output-counter))
  (string->symbol (string-append "output" (number->string output-counter))))

(define lambda-counter 0)
(define (generate-lambda)
  (set! lambda-counter (add1 lambda-counter))
  (string->symbol (string-append "lambda" (number->string lambda-counter))))

(define (get-collection-type elements)
  (cond
    [(list? elements) 
     (define types (map get-collection-type elements))
     (define uniform? (andmap (lambda (x) (eq? x (first types))) types))
     (if uniform?
         (string->symbol (string-append "<" (symbol->string (first types)) ">"))
         (error 'type-error "collection does not contain elements of the same type"))]
    [else
     (get-type (parse-type elements '() '()))]))

(define (look-in-collection col)
  (cond
    [(regexp-match #rx"<*>" (symbol->string col))
     (string->symbol (list->string (drop-right (rest (string->list (symbol->string col))) 1)))]
    [else
     (error 'type-error "contract does not match input collection type")]))

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

(define (get-namecontract funname funcontext)
  (unless (not (empty? funcontext))
    (error 'parse-error "reference to undefined function"))
  (if (eq? (namecontract-name (first funcontext)) funname)
      (first funcontext)
      (get-namecontract funname (rest funcontext))))

(define (get-varcontext expr)
  (match expr
    [`(define (,func ,args ...)
        ;contract list length is args list length plus one
        (-> ,contract ...)
        ,body ...)
     (map (lambda (name type) (nametype name type)) args (take contract (sub1 (length contract))))]
    [`(lambda (,args ...) 
               (-> ,contract ...)
               ,body ...)
     (map (lambda (name type) (nametype name type)) args (take contract (sub1 (length contract))))]
    [`(define ,type ,var ,expr)
     `(,(nametype var type))]
    [else
     `()]))

(define (get-nametype varname varcontext)
  (unless (not (empty? varcontext))
    (error 'parse-error "reference to undefined variable"))
  (if (eq? (nametype-name (first varcontext)) varname)
      (first varcontext)
      (get-nametype varname (rest varcontext))))

(define (parse program)
  (parse-format-body (parse-type-body program '() '()) #t))

(define (parse-type-body body funcontext varcontext)
  (define body-context (append (get-funcontext body) funcontext))
  (let-values ([(vc pb)
    (for/fold ([v varcontext]
               [parsed '()])
              ([expr body])
      (values (append (get-varcontext expr) v)
              (append parsed `(,(parse-type expr body-context (append (get-varcontext expr) v))))))])
    pb))

(define (parse-type expr funcontext varcontext)
  (match expr
    [(or `(+ ,operands ..2)
         `(- ,operands ..2)
         `(* ,operands ..2)
         `(/ ,operands ..2)
         `(% ,operands ..2))
     ;more type checking here required
     (define typed-operands (map (lambda (x) (parse-type x funcontext varcontext)) operands))
     (define type
       (if (ormap (lambda (op) (eq? (get-type op) 'float)) typed-operands)
           'float
           'int))
     (arith type (first expr) (map (lambda (x) (parse-type x funcontext varcontext)) operands))]
    [(or `(= ,op1 ,op2)
         `(<= ,op1 ,op2)
         `(>= ,op1 ,op2)
         `(< ,op1 ,op2)
         `(> ,op1 ,op2))
     (comp 'bool (first expr) `(,(parse-type op1 funcontext varcontext) ,(parse-type op2 funcontext varcontext)))]
    [`(cond (,preds ,bodies ...) ...)
     (define perbody-funcontext (map get-funcontext bodies))
     (define typed-bodies 
       (map (lambda (body context) (parse-type-body body (append context funcontext) varcontext))
        bodies 
        perbody-funcontext))
     (define uniform-type? 
       (foldl 
        (lambda (x y) (if (eq? x y) x #f)) 
        (get-type (last (first typed-bodies))) 
        (map (lambda (x) (get-type (last x))) typed-bodies)))
     ;; NOTE: should there also be a check here that the types are not void? i.e., they are defines
     (if uniform-type?
         (cond-e (get-type (last (last typed-bodies))) (generate-output "") preds bodies)
         (error 'type-error "cond requires all bodies to return the same type"))]
    [`(define (,func ,args ...)
        ;contract list length is args list length plus one
        (-> ,contract ...)
        ,body ...)
     (unless (= (add1 (length args)) (length contract))
       (error 'type-error "contract length does not match function declaration"))
     (define parsed-function
       (deffun func args contract 
         (parse-type-body body 
                          (append (get-funcontext body) funcontext) 
                          (append (get-varcontext expr) varcontext))))
     (unless (symbol=? (get-type (last (deffun-body parsed-function))) (last contract))
       (error 'type-error "function return type does not match function contract"))
     parsed-function]
    [`(define ,type ,var ,expr)
     (define typed-expr (parse-type expr funcontext varcontext))
     (unless (eq? (get-type typed-expr) type)
       (error 'type-error "expression does not match define type"))
     (defvar type var typed-expr)]
    [`(if ,pred ,then ,else)
     (define typed-then (parse-type then funcontext varcontext))
     (define typed-else (parse-type else funcontext varcontext))
     (define typed-pred (parse-type pred funcontext varcontext))
     ;; NOTE: should there also be a check here that the types are not void? i.e., they are defines
     (unless (eq? (get-type typed-then) (get-type typed-else))
       (error 'type-error "if requires both expressions to return the same type"))
     (if (eq? 'bool (get-type typed-pred))
         (if-e (get-type typed-then) (generate-output "") typed-pred typed-then typed-else)
         (error 'type-error "if requires predicate to be a boolean expression"))]
    [`(map ,func ,collection)
     ;; We assume single argument functions atm
     (define parsed-col (parse-type collection funcontext varcontext))
     (define parsed-func (parse-type func funcontext varcontext))
     (define contract
       (if (lambda-e? parsed-func)
           (lambda-e-contract parsed-func)
           (namecontract-contract (get-namecontract parsed-func funcontext varcontext) funcontext)))
     (define map-type (string->symbol (string-append "<" (symbol->string (second contract)) ">")))
     (if (eq? (first contract) (look-in-collection (get-type parsed-col)))
         (map-e map-type (generate-output "") func parsed-col)
         (error 'type-error "function argument type does not match collection type"))]
    [`(print ,arg)
     (define typed-arg (parse-type arg funcontext varcontext))
     (print-e typed-arg)]
    [`(collection (,elements ...))
     (collection (get-collection-type elements) (generate-output "") elements)]
    [`(lambda (,args ...) (-> ,contract ...) ,body ...)
     (unless (= (add1 (length args)) (length contract))
       (error 'type-error "contract length does not match lambda declaration"))
     (define parsed-lambda
       (lambda-e (generate-lambda) args contract 
         (parse-type-body body 
                          (append (get-funcontext body) funcontext) 
                          (append (get-varcontext expr) varcontext))))
     (unless (symbol=? (get-type (last (lambda-e-body parsed-lambda))) (last contract))
       (error 'type-error "lambda return type does not match lambda contract"))
     parsed-lambda]
    [`(,func ,operands ...)
     (funcall (last (namecontract-contract (get-namecontract func funcontext))) (generate-output "") func (map (lambda (x) (parse-type x funcontext varcontext)) operands))]
    [x
     (cond
       [(and (boolean? x))
        (immed 'bool x)]
       [(and (number? x) (regexp-match #rx"[:digit:]*" (number->string x)))
        (immed 'int x)]
       [(and (symbol? x) (regexp-match #rx"[:digit:]*.[:digit:]*f" (symbol->string x)))
        (immed 'float x)]
       [(symbol? x)
        (immed (nametype-type (get-nametype x varcontext)) x)]
       [else
        (error 'parse-type "unspecified parse-type error")])]))

; parse formatting functions
; used to add "line" and "return" structs
(define (parse-format-exp expr)
  (match expr
    [(struct cond-e (type output preds bodies))
     (cond-e type output preds (map (lambda (body) (parse-format-body body #f)) bodies))]
    [(struct deffun (name arguments contract body))
     (deffun name arguments contract (parse-format-body body #f))]
    [else (line expr)]))

(define (parse-format-return expr)
  (match expr
    [(struct cond-e _)
     (parse-format-exp expr)]
    [(or (struct deffun _)
         (struct defvar _))
     (error "last expression in body must evaluate to a value: define does not evaluate to a value")]
    [(struct if-e (type output pred then else))
     (if-e type output pred (parse-format-return then) (parse-format-return else))]
    [else (return expr)]))
    

(define (parse-format-body body main?)
  (cond 
    [main?
     `(,@(map parse-format-exp body))]
    [(> (length body) 1)
     `(,@(map parse-format-exp (take body (sub1 (length body))))
       ,(parse-format-return (last body)))]
    [else
      `(,(parse-format-return (first body)))]))

#|
(parse `((define (factorial a)
  (-> int int)
  (cond
    [(= a 0) 1]
    [(= a 1) 1]
    [else
     (* a (factorial (- a 1)))]))
         (factorial 10)))
|#

;; test cases
(check-expect (get-type (parse-type `(+ 1 2) '() '())) 'int)
(check-expect (get-type (parse-type `(- 1 2) '() '())) 'int)
(check-expect (get-type (parse-type `(* 1 2) '() '())) 'int)
(check-expect (get-type (parse-type `(/ 1 2) '() '())) 'int)

(check-expect (get-type (parse-type `(= 1 2) '() '())) 'bool)
(check-expect (get-type (parse-type `(< 1 2) '() '())) 'bool)
(check-expect (get-type (parse-type `(<= 1 2) '() '())) 'bool)
(check-expect (get-type (parse-type `(> 1 2) '() '())) 'bool)
(check-expect (get-type (parse-type `(>= 1 2) '() '())) 'bool)

(check-expect (get-type (parse-type `(+ 1.0f 2 4 6 7) '() '())) 'float)
(check-expect (get-type (parse-type `(* 1 2.0f 3 4 1.2f) '() '())) 'float)
(check-expect (get-type (parse-type `(- 1.0f 2 5 99) '() '())) 'float)
(check-expect (get-type (parse-type `(/ 1.0f 2.0f 2.3f) '() '())) 'float)

(check-expect (get-type (parse-type `(cond
                                       [(= 1 2) 5]
                                       [(= 1 1) 2]) '() '())) 'int)
(check-error (get-type (parse-type `(cond
                                      [(= 1 2) 5]
                                      [(= 1 1) #t]) '() '())) "type-error: cond requires all bodies to return the same type")

(check-expect (get-type (parse-type `(define (func a b)
                                      (-> int int int)
                                      (+ 1 1)) '() '())) 'void)
(check-error (get-type (parse-type `(define (func a b)
                                      (-> int int)
                                      (+ 1 1)) '() '())) "type-error: contract length does not match function declaration")

(check-expect (get-type (parse-type `(define int a 1) '() '())) 'void)
(check-expect (get-type (parse-type `(define float a 2.0f) '() '())) 'void)
(check-expect (get-type (parse-type `(define bool a #f) '() '())) 'void)
(check-error (get-type (parse-type `(define int a 2.0f) '() '())) "type-error: expression does not match define type")

(check-expect (get-type (parse-type `(print 1) '() '())) 'void)

(check-expect (get-type (parse-type `(func 1) `(,(namecontract 'func '(int int))) '())) 'int)
                                                 
(check-expect (get-type (parse-type `(if (= 1 1)
                                         (+ 1 1)
                                         (+ 1 1)) '() '())) 'int)
(check-expect (get-type (parse-type `(if (= 1.0f 1.0f)
                                         (+ 1.0f 10)
                                         (* 1.0f 3)) '() '())) 'float)
(check-expect (get-type (parse-type `(if #t
                                         #t
                                         #f) '() '())) 'bool)
(check-error (get-type (parse-type `(if #t
                                        #t
                                        1.0f) '() '())) "type-error: if requires both expressions to return the same type")

(check-expect (get-type (parse-type `(collection (1 2 3 4)) '() '())) '<int>)

(check-expect (get-type (parse-type `(collection ((1 2 3 4))) '() '())) '<<int>>)

(check-expect (get-type (parse-type `(collection ((12 3 43) (1 2 3 4))) '() '())) '<<int>>)

(check-expect (get-type (parse-type `(collection (((12) (3) (43)) ((1) (2) (3) (4)))) '() '())) '<<<int>>>)

(check-error (get-type (parse-type `(collection (((12) (3) (43)) ((1) (2) (3) 4))) '() '())) "type-error: collection does not contain elements of the same type")

(check-expect (get-type (parse-type `(define (func x) (-> int int) (+ x 1)) '() '())) 'void)

(check-expect (get-type (parse-type `(lambda (x) (-> int int) (+ x 1)) '() '())) 'void)

(test)

(define lambda-test `((map (lambda (x) (-> int int) (+ x 1)) (collection (1 2 3 4 5)))))

(define map-test `((define (func x)
                     (-> int int)
                     (+ x 1))
                   (print (map func (collection (1 2 3 4 5))))))

(define func-test `((define (func x)
                     (-> int int)
                     (+ x 1))
                    (func 1)))

(define line-prog `((define (func a)
                 (-> int int)
                 (define int x (+ 1 a))
                 (define int y (* 5 a))
                 (- y x))
                    (func 1)))

(define if-prog `((define (func a b) (-> int int int) (if (= a 0)
                                                         (if (= a 0)
                                                         (func (- a 1) (+ b 1))
                                                         b)
                                                         (if (= a 0)
                                                         (func (- a 1) (+ b 1))
                                                         b)))
                  (func 5 4)))

(define program `((define (add1 x) (-> int int) (+ x 1))
                  (define (func a) (-> <int> <int>) (map add1 a))
                  (func (collection (1 2 3 4)))))
