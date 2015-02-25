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
             (if (eq? op '=)
                 "=="
                 op)
             (compile-exp (second arguments)))]
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
     (define c-function (format #<<--
~a ~a(~a){
    ~a
}
--
             (convert-type (last c-contract)) name formatted-arguments compiled-body))
     (set! functions (string-append functions c-function))
     (set! device-functions (string-append device-functions (format "__device__ ~a" c-function)))
             ""]
    [(struct defvar (vartype name expr))
     (format "~a ~a = ~a" (convert-type vartype) name (compile-exp expr))]
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
    [(struct line (exp))
     (format "~a;\n" (compile-exp exp))]
    [(struct map-e (type output func collection))
     (define input (get-output collection))
     (define compiled-collection (compile-exp collection))
     (set! functions 
           (string-append functions
                          (format #<<--
__global__ void map~a(Collection<~a>* in, Collection<~a>* out)
{
    out[threadIdx.x] = ~a(in[threadIdx.x]);
}
--
                                  "func"
                                  "int"
                                  "int"
                                  "func")))
     (format #<<--
~a
dim3 dimBlock( ~a->count, 1 );
dim3 dimGrid( 1, 1 );
map~a<<<dimGrid, dimBlock>>>(&~a, generatedOutput);
--
             compiled-collection
             input
             func
             input)]
    [(struct print-e (exp))
     (define compiled-exp (compile-exp exp))
     (match (get-type exp)
       ['int (format "printf(\"%d\\n\", ~a)" compiled-exp)]
       ['float (format "printf(\"%f\\n\", ~a)" compiled-exp)]
       ['bool (format "printf(\"%s\\n\", ~a ? \"#t\" : \"#f\"" compiled-exp)])]
    [(struct return (exp))
     (format "return ~a;\n" (compile-exp exp))]
    [(struct funcall (type output name arguments))
     (define arg-string
       (apply string-append
              (map (λ (x) (format "~a, " x))
                   (map compile-exp arguments))))
     (define formatted-arguments
       (unless (empty? arguments)
         (substring arg-string 0 (- (string-length arg-string) 2))))
     (format "~a(~a)" name formatted-arguments)]
    ;check for distinct argument name
    [(struct collection (type output elements))
     (define element-type (substring (symbol->string type) 1 (sub1 (string-length (symbol->string type)))))
     (define count (length elements))
     (format #<<--
Collection<~a>* ~a = new Collection<~a>;
~a->count = ~a;
~a->elements = managedArray<~a>(~a);
int ~aImmediate[~a] = {~a};
memcpy(~a->elements, ~aImmediate, sizeof(~a)*~a);
--
             element-type
             output
             element-type
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
    [(struct immed (type val))
     (format "~a" val)]
    [x
     (format "~a" x)]))

;; COMPILER HELPER FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



