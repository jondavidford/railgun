#lang racket

(provide compile)

; ################
;;;;;;;;;;;;;;;;;;
;     PARSER
;;;;;;;;;;;;;;;;;;
; ################

(define (parse program)
  (map parse-exp program))

; adds "return" and "line" expressions, making compiling much easier
(define (parse-exp expr)
  (match expr
    [`(cond (,preds ,bodies ...) ...)
     `(cond
        ,@(map (lambda (pred body)
                 `(,pred ,@(parse-body body)))
               preds
               bodies))]
    [`(define (,func ,args ...)
        (-> ,contract ...)
        ,body ...)
     `(define (,func ,@args)
        (-> ,@contract)
        ,@(parse-body body))]
    [else `(line ,expr)]))

;; PARSER HELPER FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (return-parse expr)
  (match expr
    [`(cond ,clauses ...)
     (parse-exp expr)]
    [`(define ,x ...)
     (error "last expression in body must evaluate to a value: define does not evaluate to a value")]
    [`(if ,pred ,then ,else)
     `(if ,pred
          ,(return-parse then)
          ,(return-parse else))]
    [else
     `(return ,expr)]))

;; calls parse-exp on first n-1 exprs in body
;; then calls return-parse on last expr in body
(define (parse-body body)
  (if (> (length body) 1)
      `(,(map parse-exp (take body (sub1 (length body))))
        ,(return-parse (last body)))
      `(,(return-parse (first body)))))

; ################
;;;;;;;;;;;;;;;;;;
;    COMPILER
;;;;;;;;;;;;;;;;;;
; ################

(define (compile program)
  (define compiled-program (apply string-append (map compile-exp (parse program))))
  (format #<<--
#include <stdio.h>

template<typename T>
struct Collection
{
    T* elements;
    int count;
};

void print(int variable)
{
    printf("%d\n", variable);
}

~a

int main()
{
~a
}
--
    functions
    compiled-program))

(define functions "")

(define (compile-exp expr)
  (match expr
    ;use regexes to have "+ or - or * or % ...
    [(or `(+ ,operands ..2)
         `(- ,operands ..2)
         `(* ,operands ..2)
         `(/ ,operands ..2)
         `(% ,operands ..2))
     (define op (first expr))
     (define infix 
       (apply string-append
              (map (λ (x) (format "~a ~a " x op))
                   (map compile-exp operands))))
     (format "(~a)" (substring infix 0 (- (string-length infix) 3)))]
    [(or `(= ,op1 ,op2)
         `(<= ,op1 ,op2)
         `(>= ,op1 ,op2)
         `(< ,op1 ,op2)
         `(> ,op1 ,op2))
     (define op (if (eq? (first expr) '=)
                    '==
                    (first exp)))
     (format "(~a ~a ~a)"
             (compile-exp op1)
             op
             (compile-exp op2))]
    [`(cond (,preds ,bodies ...) ...)
     ""]
    [`(define (,func ,args ...)
        ;contract list length is args list length plus one
        (-> ,contract ...)
        ,body ...)
     (define c-contract (map convert-type contract))
     (define arg-string
       (unless (empty? args)
         (apply string-append
                (map (λ (x y) (format "~a ~a, " x y))
                     (take c-contract (sub1 (length c-contract)))
                     (map compile-exp args)))))
     (define arguments
       (if (empty? args)
           ""
           (substring arg-string 0 (- (string-length arg-string) 2))))
     (define compiled-body
       (if (< (length body) 2)
           ""
           (apply (lambda (x) (format "~a;\n" x))
                  (map compile-exp (take body (sub1 (length body)))))))
     (define return
       (compile-exp (last body)))
     (set! functions (string-append functions (format #<<--
~a ~a(~a){
    ~a
    ~a
}
--
             (last c-contract) func arguments compiled-body return)))
             ""]
    [`(define ,type ,var ,expr)
     (format "~a ~a = ~a" (convert-type type) var (compile-exp expr))]
    [`(,func ,operands ...)
     (define arg-string
       (apply string-append
              (map (λ (x) (format "~a, " x))
                   (map compile-exp operands))))
     (define arguments
       (unless (empty? operands)
         (substring arg-string 0 (- (string-length arg-string) 2))))
     (format "~a(~a)" func arguments)]
    [`(if ,pred ,then ,else)
     (format #<<--
if(~a) {
    ~a
} else {
    ~a
}
--
      (compile-exp pred)
      (compile-exp then)
      (compile-exp else))]
    [`(line ,exp)
     (format "~a;\n" (compile-exp exp))]
    [`(print ,exp)
     (format "print(~a)" (compile-exp exp))]
    [`(return ,exp)
     (format "return ~a;\n" (compile-exp exp))]
    ;check for distinct argument name
    [x
     (format "~a" x)]))

;; COMPILER HELPER FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert-type type)
  (cond
    [(list? type) (format "Collection<~a>" (convert-type (first type)))]
    [else (symbol->string type)]))

; ##################
;;;;;;;;;;;;;;;;;;;;
;      TESTS
;;;;;;;;;;;;;;;;;;;;
; ##################

(define program `((define (func a b) (-> int int int) (if (= a 0)
                                                         (if (= a 0)
                                                         (func (- a 1) (+ b 1))
                                                         b)
                                                         (if (= a 0)
                                                         (func (- a 1) (+ b 1))
                                                         b)))
                  (print (func 5 4))))

(define line `(define (func a)
                (-> int int)
                (define x int (+ 1 a))
                (define y int (* 5 a))
                (- y x)))
