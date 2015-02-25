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

class Managed
{
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
struct Collection : public Managed
{
    T* elements;
    int count;
};

template<typename T>
T* managedArray(int size)
{
    void *ptr;
    cudaMallocManaged(& ptr, size*sizeof(T));
    return (T*)ptr;
}

~a

~a

int main()
{
    ~a
}
--
    device-functions
    functions
    compiled-program))

(define (compile-exp expr)
  (match expr
    ; ARITHMETIC OPERATION
    [(struct arith (type op arguments))
     (define infix 
       (apply string-append
              (map (λ (x) (format "~a ~a " x op))
                   (map compile-exp arguments))))
     (format "(~a)" (substring infix 0 (- (string-length infix) 3)))]
    ; COMPARISON OPERATION
    [(struct comp (type op arguments))
     (format "(~a ~a ~a)"
             (compile-exp (first arguments))
             (if (eq? op '=)
                 "=="
                 op)
             (compile-exp (second arguments)))]
    ; FUNCTION DEFINE
    [(struct deffun (name output arguments contract body))
     (define c-contract (map convert-type contract))
     (define arg-string
       (unless (empty? arguments)
         (apply string-append
                (map (λ (x y) (format "~a ~a, " x y))
                     (map convert-type (take c-contract (sub1 (length c-contract))))
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
     ; format two copies of the function
     ; one to run on host and one on device
     (define c-function (format #<<--
~a ~a(~a){
    ~a
}
--
                                (convert-type (last c-contract))
                                name 
                                formatted-arguments 
                                compiled-body))
     (define device-function (format #<<--
__device__ ~a device_~a(~a){
    ~a
}
--
                                     (convert-type (last c-contract))
                                     name 
                                     formatted-arguments 
                                     compiled-body))
     ; update the global function and device function variables
     (set! functions (string-append functions c-function))
     (set! device-functions (string-append device-functions device-function))
     ; return empty string since functions are pulled out to top level in C code
     ""]
    ; VARIABLE DEFINE
    [(struct defvar (vartype name expr))
     (format "~a ~a = ~a" (convert-type vartype) name (compile-exp expr))]
    ; IF STATEMENT
    [(struct if-e (type output pred then else))
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
    ; LINE EXPRESSION
    ; the parser thinks this expression can be translated into a single C line
    [(struct line (exp))
     (format "~a;\n" (compile-exp exp))]
    ; MAP EXPRESSION
    [(struct map-e (type output func collection))
     (define input (get-output collection))
     (define compiled-collection (compile-exp collection))
     (define collection-class-string (make-collection-class-string (symbol->string type)))
     (set! functions 
           (string-append functions
                          (format #<<--
__global__ void map~a(~a* in, ~a* out)
{
    out->elements[threadIdx.x] = device_~a(in->elements[threadIdx.x]);
}
--
                                  func
                                  collection-class-string
                                  collection-class-string
                                  func)))
     (format #<<--
~a
~a* ~a = new ~a;
~a->count = ~a->count;
~a->elements = managedArray<~a>(~a->count);

dim3 dimBlock( ~a->count, 1 );
dim3 dimGrid( 1, 1 );
map~a<<<dimGrid, dimBlock>>>(~a, ~a);
--
             compiled-collection
             collection-class-string
             output
             collection-class-string
             output
             input
             output
             (look-in-collection type)
             input
             input
             func
             input
             output)]
    ; PRINT EXPRESSION
    [(struct print-e (exp))
     (define compiled-exp (compile-exp exp))
     (match (symbol->string (get-type exp))
       ; currently only Collection<int> supported
       [(regexp #rx"<*>") 
        (define output (get-output exp))
        (format #<<--
~a

int __collection_size = ~a->size;
printf("[");
for (int i = 0; i < __collection_size-1; ++i) {
    printf("%d ,", ~a->elements[i]);
}
printf("%d", ~a->elements[__collection_size-1]);
printf("]");
--
                compiled-exp
                output
                output
                output)]
       [(regexp #rx"int") (format "printf(\"%d\\n\", ~a)" compiled-exp)]
       [(regexp #rx"float") (format "printf(\"%f\\n\", ~a)" compiled-exp)]
       [(regexp #rx"bool") (format "printf(\"%s\\n\", ~a ? \"#t\" : \"#f\")" compiled-exp)])]
    ; RETURN EXPRESSION
    ; the parser thinks this expression can be compiled into a single line
    ; and that the line is a return statement in C
    [(struct return (exp))
     (format "return ~a;\n" (compile-exp exp))]
    ; FUNCTION CALL
    [(struct funcall (type output name arguments))
     (define arg-string
       (apply string-append
              (map (λ (x) (format "~a, " x))
                   (map compile-exp arguments))))
     (define formatted-arguments
       (unless (empty? arguments)
         (substring arg-string 0 (- (string-length arg-string) 2))))
     (format "~a(~a)" name formatted-arguments)]
    ; note: check for distinct argument name?
    
    ; COLLECTION IMMEDIATE
    ; generate multiple lines of code that allocate an array for a collection
    [(struct collection (type output elements))
     (define collection-class-string (make-collection-class-string (symbol->string type)))
     (define element-type (look-in-collection type))
     (define count (length elements))
     (format #<<--
~a* ~a = new ~a;
~a->count = ~a;
~a->elements = managedArray<~a>(~a);
int ~aImmediate[~a] = {~a};
memcpy(~a->elements, ~aImmediate, sizeof(~a)*~a);
--
             collection-class-string
             output
             collection-class-string
             output
             count
             output
             element-type
             count
             output
             count
             (list->initializer elements)
             output
             output
             element-type
             count)]
    ; IMMEDIATE VALUE
    [(struct immed (type val))
     (format "~a" val)]
    [x
     (format "~a" x)]))

;; COMPILER HELPER FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(define (make-collection-class-string type)
  (cond
    [(regexp-match #rx"<*>" type)
     (format "Collection<~a>"
             (make-collection-class-string (list->string (drop-right (rest (string->list type)) 1))))]
    [else type]))

(define (convert-type type)
  (cond
    [(string? type)
     (if (string=? (substring type 0 1) "<")
         (format "Collection<~a>" (convert-type (substring type 1 (sub1 (string-length type)))))
         type)]
    [else
     (if (symbol? type)
         (symbol->string type)
         type)]))

(define (list->initializer l)
  (define with-commas (apply string-append (map (lambda (x) (string-append (number->string x) ",")) l)))
  (substring with-commas 0 (sub1 (string-length with-commas))))

; ##################
;;;;;;;;;;;;;;;;;;;;
;  TEST PROGRAMS
;;;;;;;;;;;;;;;;;;;;
; ##################

(define print `((print 1)))

(define function `((define (func a) (-> int int) a) (func 1)))

(define program `((define (add1 x) (-> int int) (+ x 1))
                  (define (func a) (-> <int> <int>) (map add1 a))
                  (func (collection (1 2 3 4)))))

(define line-prog `((define (func a)
                      (-> int int)
                      (define int x (+ 1 a))
                      (define int y (* 5 a))
                      (- y x))
                    (func 1)))

(define equal-test `((print (= 1 1))
                     (print (= 1 0))))

(define map-test `((define (func x)
                     (-> int int)
                     (+ x 1))
                   (print (map func (collection (1 2 3 4 5))))))



