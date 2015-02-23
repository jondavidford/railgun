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
struct Collection
{
    T* elements;
    int count;
};

tempalte<typename T>
T* managedArray(int size)
{
    void *ptr;
    cudaMallocManaged(& ptr, size*sizeof(T));
    return (T*)ptr;
}





void main()
{
    printf("%d\n", (3 * 5 * 1 * 2 * 4));

}