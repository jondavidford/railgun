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

(define program `(define (func a b) (-> [int] [int] [int]) (+ a b)))

(define program2 `(define (func a b)
                    (-> int int int)
                    (+ 1 2)
                    (+ 1 2)))

(define program3 `(define (func a b)
                    (-> int int int)
                    (define x int 1)
                    x))

(define (convert-type type)
  (cond
    [(list? type) (format "Collection<~a>" (convert-type (first type)))]
    [else (symbol->string type)]))

(define (compile-function expr)
  (match expr
    [`(define (,func ,args ...)
        ;contract list length is args list length plus one
        (-> ,contract ...)
        ,body ...)
     (define c-contract (map convert-type contract))
     (map printf c-contract)
     (define arg-string
       (unless (empty? args)
         (apply string-append
                (map (λ (x y) (format "~a ~a, " x y))
                     (take c-contract (sub1 (length c-contract)))
                     args))))
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
    return ~a;
}
--
             (last c-contract) func arguments compiled-body return)]
    [`(lambda (,args ...) ) "hi"]))

(define (compile-exp expr)
  (match expr
    ;check for distinct argument names
    [`(define (,func ,args ...)
        ;contract list length is args list length plus one
        (-> ,contract ...)
        ,body ...)
     (define c-contract (map convert-type contract))
     (map printf c-contract)
     (define arg-string
       (unless (empty? args)
         (apply string-append
                (map (λ (x y) (format "~a ~a, " x y))
                     (take c-contract (sub1 (length c-contract)))
                     args))))
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
    return ~a;
}
--
             (last c-contract) func arguments compiled-body return)]
    [`(define ,var ,type ,expr)
     (format "~a ~a = ~a" (convert-type type) var (compile-exp expr))]
    [`(,op ,operands ...)
     (define infix 
       (apply string-append
              (map (λ (x) (format "~a ~a " x 
                                  (compile-exp op)))
                   operands)))
     (format "(~a)" (substring infix 0 (- (string-length infix) 3)))]
    [x
     (format "~a" x)]))