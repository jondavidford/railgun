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





int main()
{
    Collection<int>* aPlaceHolderCollectionName = new Collection<int>;
aPlaceHolderCollectionName->count = 4;
aPlaceHolderCollectionName->elements = managedArray<int>(4);
int aPlaceHolderArrayName[4] = {1,2,3,4};
memcpy(aPlaceHolderCollectionName->elements, aPlaceHolderArrayName, sizeof(int)*4);;
printf("%d\n", 1);

}