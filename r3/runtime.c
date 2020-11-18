// so, this is a simpler runtime for r3
#include <inttypes.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include "runtime.h"


// 初始化程序的堆和栈
void initialize(uint64_t rootstack_size, uint64_t heap_size) 
{
    // 1. check
    assert(sizeof(int64_t) == sizeof(int64_t*));
    assert((heap_size % sizeof(int64_t)) == 0);
    assert((rootstack_size % sizeof(int64_t)) == 0);

    // 2. allocate
    fromspace_begin = malloc(heap_size);
    fromspace_end = fromspace_begin + heap_size / sizeof(int64_t);
    
    tospace_begin = malloc(heap_size);
    tospace_end = tospace_end + heap_size / sizeof(int64_t);

    rootstack_begin = malloc(rootstack_size);
    rootstack_end = rootstack_begin + rootstack_size / sizeof(int64_t);

    // check all
    assert(fromspace_begin && tospace_begin && rootstack_begin);

    // 3. init
    free_ptr = fromspace_begin;
}



// rootstack_ptr 代表栈顶的指针，即 top 指针
void collect(int64_t** rootstack_ptr, uint64_t bytes_requested)
{
    // 1. 先进行回收，cheney 把 fromspace 的有效引用全部复制到了 tospace，并交换了
    //    tospace 和 fromspace 的空间。也就是说，内容仍然在（新的） fromspace 中。
    cheney(rootstack_ptr);

    // 2. 看看是否有足够的空间来分配新的对象，如果没有，则需要扩大 heap 的空间
    if (sizeof(int64_t) * (fromspace_end - free_ptr) < bytes_requested) {
        printf("COLLECT: no enough room, enlarger the heap\n");
        
        unsigned long occupied_bytes = (free_ptr - fromspace_begin) * sizeof(int64_t);
        unsigned long needed_bytes = occupied_bytes + bytes_requested;
        unsigned long old_len = fromspace_end - fromspace_begin;
        unsigned long old_bytes = old_len * sizeof(int64_t);

        // 一直翻倍，直到超过 needed_bytes
        unsigned long new_bytes = old_bytes;
        while (new_bytes <= needed_bytes) {
            new_bytes *= 2;
        }

        // 现在，我们需要把旧的 tospace 释放掉，同时分配一个新的 tospace。然后，用 cheney 
        // 把数据拷贝到 tospace 来。注意，tospace 和 fromspace 又交换了。这时，fromspace
        // 比 tospace 要大，需要把 tospace 释放掉，分配一个新的。
        free(tospace_begin);
        tospace_begin = malloc(new_bytes);
        tospace_end = tospace_begin + new_bytes / sizeof(int64_t);
        assert(!tospace_begin);

        cheney(rootstack_ptr);

        free(tospace_begin);
        tospace_begin = malloc(new_bytes);
        tospace_end = tospace_begin + new_bytes / sizeof(int64_t);
    }
    
}

// cheney 算法的作用是，把 fromspace 的有效引用全都复制到 tospace。然后把栈上的指针重新指向
// tospace 的内容。最后，交换两个堆的指针。
void cheney(int64_t** rootstack_ptr)
{
    // 要在 tospace 分配内存，所以 free_ptr 需要调整到开始位置
    free_ptr = tospace_begin;

    // 首先把 rootstack 上的东西转移过去。copy_vector 会检查这个 vector 是否已经复制
    // 如果已经复制了，就把 cur 指向新的向量。如果没有，就先复制过去，再修改 cur 的值
    // -----------------------------------------------------------
    // p1* | p2* | p3* | ... | ...  | pn* | ...  
    // ^                               ^
    // cur  -> ->  -> 
    // begin                          ptr                   end 
    for (int64_t** cur = rootstack_begin; cur != rootstack_ptr; cur++) {
        copy_vector(cur);
    }

    // 现在，rootstack 上的内容全都在 tospace 中了，我们从左往右扫每一个节点，直到
    // 到达 free_ptr 的位置。free_ptr 不一定在 tospace 的开始位置了。因为复制过程
    // 会移动 free_ptr 指针
    int64_t* scan_ptr = tospace_begin;
    while (scan_ptr != free_ptr) {

        // tag 总是在 vector 的第一个位置
        int64_t tag = *scan_ptr;

        // 长度位于 tag 的第 1-6 个位
        int len = (tag >> 1) & 0b111111;

        // 算出下一个要处理的向量
        int64_t* next_ptr = scan_ptr + len + 1;

        // 取出 pointer mask，它提示我们当前这个向量中的数据类型
        int64_t mask = tag >> 7;
        
        // 扫一遍这些元素，如果是一个向量，则复制到 tospace
        scan_ptr += 1;
        while (scan_ptr != next_ptr) {
            if ((mask & 1) == 1) {
                // mask 为 1，说明 scan_ptr 所指向的内容是一个向量指针，所以呢，我们把
                // scan_ptr 转为指针的指针，用 copy_vector 进行复制
                copy_vector((int64_t**)scan_ptr);
            }
            mask = mask >> 1;
            scan_ptr += 1;
        }
        // scan_ptr 到达 next_ptr 时，对第一个元素的处理就结束了，开始处理下一个元素了。
    }
    // 来到这里，scan_ptr 跟 free_ptr 相遇，垃圾收集完毕。
    // 现在要交换两个空间了。
    int64_t* tmp_begin = tospace_begin;
    int64_t* tmp_end = tospace_end;
    tospace_begin = fromspace_begin;
    tospace_end = fromspace_end;
    fromspace_begin = tmp_begin;
    fromspace_end = tmp_end;
}


// copy_vector 接受一个指向向量指针的指针，把它所指向的向量复制到 tospace 中，并把这个指针
// 指向复制出来的那个向量
// 提示： *vector_ptr_loc 指的是一个指向向量的指针
//      **vector_ptr_loc 指的是真正的向量
void copy_vector(int64_t** vector_ptr_loc)
{
    int64_t* old_vector_ptr = *vector_ptr_loc;
    // 由于向量都是 8 字节对齐的，所以向量指针的最后 3 位总会是 0
    // 利用这一点，我们可以设置标记。因为我们要能够分辨这个指针是否已被处理过了。
    // 首先，取出标志位
    int old_tag = (int64_t)old_vector_ptr & 0b111;
    
    // 现在，我们规定 vector 类型的 tag 标志位为 010，与 EoC 官方提供的一样
    // 正确的做法应该是像官方写的那样，写一些宏定义。目前就先这样吧。
    // 如果不是向量或者指针，就直接返回
    if (!(old_tag == 0b010 || old_tag == 0b000)) { return; }

    // 检查了标志位，是一个向量/指针，那就得还原出地址。也就是清除标志位
    int64_t tmp = (int64_t)old_vector_ptr & ~0b111;
    old_vector_ptr = (int64_t*)tmp;

    // 现在有了向量的指针，可以真正进行回收了。这个向量的 tag 放在第一个位置
    int64_t tag = old_vector_ptr[0];

    // 当然，一个向量可能被多次访问。所以，在第一次访问的时候，我们必须标志这个
    // 向量已经被访问过。方法是：tag 的最低位为 1 时，说明向量未被访问。为 0 时
    // 说明向量已被访问过，并且整个 tag 代表了新向量在 tospace 中的位置。
    if ((tag & 1) == 0) {
        // 向量已经访问过了，tag 代表了新的向量的地址。我们只需要更新向量指针就可以
        // 更新的时候，需要把这个向量的信息标志信息一起编码
        *vector_ptr_loc = (int64_t*) (tag | old_tag);
    } else {
        // 向量未复制，我们先取出长度信息
        int length = (tag >> 1) & 0b111111;
        
        // 向量将被安置在 free_ptr 所在地
        int64_t* new_vector_ptr = free_ptr;

        // 复制的时候，需要连同 tag 一起复制了
        for (int i = 0; i < length + 1; i++) {
            new_vector_ptr[i] = old_vector_ptr[i];
        }

        // 复制完成，更新 free_ptr 指向下一个空闲内存。
        free_ptr = free_ptr + length + 1;

        // 在原来的向量中标志这个向量已经被访问过了，这个位置是不带 tag 信息的
        old_vector_ptr[0] = (int64_t) new_vector_ptr;

        // 最后，把指针指向 tospace 中新的向量，这个指针需要带上标志信息
        new_vector_ptr = (int64_t*)((int64_t)new_vector_ptr | old_tag);
        *vector_ptr_loc = new_vector_ptr;
    }
}



int64_t read_int() {
  int64_t i;
  scanf("%" SCNd64, &i);
  return i;
}
