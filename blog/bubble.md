# Rust One Piece | 泡沫群岛

朝着 One Piece 出发的路飞一行人，在泡泡岛遇上了海军最强战力，被可爱的大熊拍飞。两年静修，以退为进。

本篇，也是如此。

### EoC

EoC 是本次教程的教材，现在来到了第五章后半部分。但是，因内外的原因，我想就此终止，我没有额外的精力来继续完成它了。

但这份教程还是有一些价值，这些价值来源于 EOC 本身。它在章节的组织上比较清晰，好上手。具体的内容，我印象深刻的主要是

+ 解释器
+ IF 语句的编译
+ 图着色寄存器分配算法
+ 垃圾回收算法

很遗憾还没有走到下一节 函数 的编译。

目前的语言支持：

+ 类型检查
+ 算数运算/逻辑运算
+ int/bool 类型
+ if 语句
+ 变量
+ 自动寄存器分配
+ 垃圾回收（一半）
+ 元组（一半）


### Rust

Rust 集成了测试，所以写测试很方便。但 Rust 跟 Lisp 相比灵活性差得多了。因此，写的过程有很多繁琐之处，这或许也是 Rust 性能的代价。这一次教程我们用到了

+ Rust 模块系统
+ 生命周期
+ Rc/RefCell
+ box pattern
+ 测试

在第五章，Hastype 的引入使得代码变得难以阅读和调试，尽管我增加了一些辅助函数，但还是觉得心累。由于以前看过其他的书籍，我相信 Hastype 并不是一个优雅的方案，而我不想在这上面浪费时间了。

### Next

我从没写过这么多的 Rust 代码，大部分时候是比较快乐的，但也会对 Rust 感到厌烦。之前想用 chez scheme 的，但是错误提示实在不算友好，导致事情很难进行。在我用过为数不多的语言中，Rust 的错误提示排在前列。Rust 的工具链也是给我非常好的印象，而 chez scheme 并没有这样一个 team，虽然有 darkart 但是用户很少。

其实 Rust 很接近我心中的语言，它有底层操作，也有高层抽象，有速度也有效率，但是它过于复杂，我不喜欢太复杂的东西。我心中的编程语言像是这样：

+ 它基本上是一个 mll (multi-level lisp) 多层级 lisp 语言，当然啦 mllvm 。
+ 汇编的抽象层为类 rsic-v
+ 底层是类 C 的 lisp，支持 C 指针和汇编
+ 中间层是可选的 ownership 约束
+ 最高层是完整的 lisp，有 gc 等等
+ 三层之间互通无余，ffi 可以定义在不同的层
+ 像 Rust 那样的错误提示
+ 像 Scheme 那样的编译速度
+ 支持编译到 llvm，以便掂量自己几斤几两
+ repl

在此基础上，可以为 Python 等语言实现一个 Parser，编译到 mllvm 。

我只是说说而已，客官笑笑便罢。

### LiSP

Lisp in small pieces -- 在这系列博客开始之初，就有高人推荐了这么一本书。我读完了它的第一章，代码位于 LiSP 文件夹下。

    cp1.ss              : 114
    cp1-test.ss         : 25
    ----------------------------
    total               : 141


它实现了一个解释器，由五种 special form 组成

+ quote
+ begin
+ lambda
+ set!
+ if

支持函数，算术运算，布尔运算等等。

我想短期内不会再写关于编译器的东西，目前而言，默默努力啦！

> 两年之后，草帽一伙重聚，又朝着 One Piece 一路向前。