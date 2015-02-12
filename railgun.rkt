#lang racket

(require "parse.rkt")

(provide compile)


; ################
;;;;;;;;;;;;;;;;;;
;    COMPILER
;;;;;;;;;;;;;;;;;;
; ################

(define functions "")
(define device-functions "")

(define (compile program)
  (set! functions "")
  (set! device-functions "")
  (define compiled-program (apply string-append (map compile-exp (parse program))))
  (format #<<--
#include <stdio.h>

template<typename T>
struct Collection
{
    T* elements;
    int count;
};

~a

~a

void main()
{
    ~a
}
--
    device-functions
    functions
    compiled-program))

(define (compile-exp expr)
  (match expr
    ;use regexes to have "+ or - or * or % ...
    [(struct arith (type op arguments))
     (define infix 
       (apply string-append
              (map (λ (x) (format "~a ~a " x op))
                   (map compile-exp arguments))))
     (format "(~a)" (substring infix 0 (- (string-length infix) 3)))]
    [(struct comp (type op arguments))
     (format "(~a ~a ~a)"
             (compile-exp (first arguments))
             op
             (compile-exp (second arguments)))]
    [`(cond (,preds ,bodies ...) ...)
     ""]
    [(struct deffun (name arguments contract body))
     (define c-contract (map convert-type contract))
     (define arg-string
       (unless (empty? arguments)
         (apply string-append
                (map (λ (x y) (format "~a ~a, " x y))
                     (take c-contract (sub1 (length c-contract)))
                     (map compile-exp arguments)))))
     (define formatted-arguments
       (if (empty? arguments)
           ""
           (substring arg-string 0 (- (string-length arg-string) 2))))
     (define compiled-body
       (if (< (length body) 2)
           (compile-exp (first body))
           (apply string-append
                  (map compile-exp body))))
     (define c-function (format #<<--
~a ~a(~a){
    ~a
}
--
             (last c-contract) name formatted-arguments compiled-body))
     (set! functions (string-append functions c-function))
     (set! device-functions (string-append device-functions (format "__device__ ~a" c-function)))
             ""]
    [(struct defvar (vartype name expr))
     (format "~a ~a = ~a" (convert-type vartype) name (compile-exp expr))]
    [(struct if-e (type pred then else))
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
    [(struct line (exp))
     (format "~a;\n" (compile-exp exp))]
    [`(map ,func ,collection)
     (set! functions 
           (string-append functions
                          (format #<<--
__global__ void map~a(Collection<~a>* in, Collection<~a>* out)
{
    out[threadIdx.x] = ~a(in[threadIdx.x]);
}
--
                                  func
                                  (convert-type (first (first collection)))
                                  (convert-type (first (first collection)))
                                  func)))
     (format #<<--
allocate Collection;
dim3 dimBlock( ~a->count, 1 );
dim3 dimGrid( 1, 1 );
map~a<<<dimGrid, dimBlock>>>(&~a, allocatedCollection);
--
             collection
             func
             collection)]
    [(struct print-e (exp))
     (format "printf(\"~a\\n\", ~a)"
             (match (get-type exp)
               ['int "%d"]
               ['float "%f"])
             (compile-exp exp))]
    [(struct return (exp))
     (format "return ~a;\n" (compile-exp exp))]
    [(struct funcall (type name arguments))
     (define arg-string
       (apply string-append
              (map (λ (x) (format "~a, " x))
                   (map compile-exp arguments))))
     (define formatted-arguments
       (unless (empty? arguments)
         (substring arg-string 0 (- (string-length arg-string) 2))))
     (format "~a(~a)" name formatted-arguments)]
    ;check for distinct argument name
    [(struct immed (type val))
     (format "~a" val)]
    [x
     (format "~a" x)]))

;; COMPILER HELPER FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert-type type)
  (cond
    [(list? type)
     (format "Collection<~a>" (convert-type (first type)))]
    [else
     (symbol->string type)]))

; ##################
;;;;;;;;;;;;;;;;;;;;
;      TESTS
;;;;;;;;;;;;;;;;;;;;
; ##################

(define print `((print 1)))

(define function `((define (func a) (-> int int) a) (func 1)))

(define program `((define (func a b) (-> int int int) (if (= a 0)
                                                         (if (= a 0)
                                                         (func (- a 1) (+ b 1))
                                                         b)
                                                         (if (= a 0)
                                                         (func (- a 1) (+ b 1))
                                                         b)))
                  (func 5 4)))

(define line-prog `((define (func a)
                 (-> int int)
                 (define int x (+ 1 a))
                 (define int y (* 5 a))
                 (- y x))
                    (func 1)))


