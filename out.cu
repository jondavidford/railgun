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

__device__ int add1(int x){
    return (x + 1);

}

__device__ Collection<int> func(Collection<int> a){
    return a
    dim3 dimBlock( void->count, 1 );
    dim3 dimGrid( 1, 1 );
    mapadd1<<<dimGrid, dimBlock>>>(&void, generatedOutput);;

}

int add1(int x){
    return (x + 1);

}

__global__ void mapfunc(Collection<int>* in, Collection<int>* out)
{
    out[threadIdx.x] = func(in[threadIdx.x]);
}

Collection<int> func(Collection<int> a){
    return a
    dim3 dimBlock( void->count, 1 );
    dim3 dimGrid( 1, 1 );
    mapadd1<<<dimGrid, dimBlock>>>(&void, generatedOutput);;
}

int main()
{
    func(Collection<int>* output5 = new Collection<int>;
    output5->count = 4;
    output5->elements = managedArray<int>(4);
    int output5Immediate[4] = {1,2,3,4};
    memcpy(output5->elements, output5Immediate, sizeof(int)*4););
}
