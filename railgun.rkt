#lang racket
(define header #<<--
#include <stdio.h>

class Managed {
public:
  void *operator new(size_t len) {
    void *ptr;
    cudaMallocManaged(&ptr, len);
    return ptr;
  }

  void operator delete(void *ptr) {
    cudaFree(ptr);
  }
};

template<typename T>
struct Collection
{
    T* elements;
    int count;
};

void print(int variable)
{
    printf("Output: %d", variable);
}
--
  )

(define main #<<--
int main()
{
--
  )

(define footer #<<--

}
--
  )

(define program `(define (func a b) (-> int int int) (if (= a 0)
                                                         (func (- a 1) (+ b 1))
                                                         b)))

(define (convert-type type)
  (cond
    [(list? type) (format "Collection<~a>" (convert-type (first type)))]
    [else (symbol->string type)]))

; adds "return" and "line" expressions, making compiling much easier
(define (parse-exp expr return?)
  (match expr
    [`(define (,func ,args ...)
        (-> ,contract ...)
        ,body ...)
     `(define (,func ,@args)
        (-> ,@contract)
        ,@(map (lambda (x) (parse-exp x #f)) (take body (sub1 (length body))))
        ,@(parse-exp (last body) #t))]
    [`(if ,pred ,then ,else)
     (if return?
         `(if ,pred
              ,(if (and (list? then)
                        (eq? (first then) 'if))
                   (parse-exp then)
                   `(return ,then))
              ,(if (and (list? then)
                        (eq? (first else) 'if))
                   (parse-exp else)
                   `(return ,else)))
         expr)]
    [`(cond ,clauses ...)
     (if return?
         `(cond
            ,@(map (lambda (clause)
                     `[,pred 
    [else
     (cond
       [return? `(return ,expr)]
       [else expr])]))

(define (compile-exp expr)
  (match expr
    ;check for distinct argument names
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
     (format #<<--
~a ~a(~a){
    ~a
    ~a
}
--
             (last c-contract) func arguments compiled-body return)]
    [`(define ,type ,var ,expr)
     (format "~a ~a = ~a" (convert-type type) var (compile-exp expr))]
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
                   operands)))
     (format "(~a)" (substring infix 0 (- (string-length infix) 3)))]
    [`(,func ,operands ...)
     (define arg-string
       (apply string-append
              (map (λ (x) (format "~a, " x))
                   (map compile-exp operands))))
     (define arguments
       (unless (empty? operands)
         (substring arg-string 0 (- (string-length arg-string) 2))))
     (format "~a(~a)" func arguments)]
    [x
     (format "~a" x)]))


