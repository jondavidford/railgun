#include <stdio.h>

int main(void){
    printf("#include <stdio.h>\n\nclass Managed {\npublic:\n  void *operator new(size_t len) {\n    void *ptr;\n    cudaMallocManaged(&ptr, len);\n    return ptr;\n  }\n\n  void operator delete(void *ptr) {\n    cudaFree(ptr);\n  }\n};\n\ntemplate<typename T>\nstruct Collection\n{\n    T* elements;\n    int count;\n};\n\nvoid print(int variable)\n{\n    printf(\"%d\", variable);\n}\n\nint func(int a, int b){\n    \n    if((a == 0)) {\n    if((a == 0)) {\n    return func((a - 1), (b + 1));\n\n} else {\n    return b;\n\n}\n} else {\n    if((a == 0)) {\n    return func((a - 1), (b + 1));\n\n} else {\n    return b;\n\n}\n}\n}\n\nint main()\n{\n( print(func(5, 4));\n)\n}");
    return 0;
}
