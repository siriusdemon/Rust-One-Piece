#include <stdint.h>


// fromspace 代表的是程序可用的 heap。如果这个 heap 不够用了，就启动垃圾收集
// 把 fromspace 中的还有用的变量全部拷贝到 tospace
// tospace 跟 fromspace 应该一样大
int64_t* fromspace_begin;
int64_t* fromspace_end;

int64_t* tospace_begin;
int64_t* tospace_end;

// free_ptr 一直指向下一个空闲的内存位置
int64_t* free_ptr;

// rootstack 是引用类型的栈。它是一个指针数组。数组的指针指向当前 heap 中的元素
// 在回收时，只有在这个栈的引用类型才会被复制到 tospace。
// 提示：int* 是一个指向整型的指针，int** 是一个指向整型指针的指针
int64_t** rootstack_begin;
int64_t** rootstack_end;


void initialize(uint64_t rootstack_size, uint64_t heap_size);
void collect(int64_t** rootstack_ptr, uint64_t bytes_requested);
void copy_vector(int64_t** vector_ptr_loc);
void cheney(int64_t** rootstack_ptr);


int64_t read_int();