# Rust One Piece | 写一个编译器

想写一个编译器。写给那些与我一样，对编译器感兴趣的你，愿你永远不会退失学习的乐趣。

### 教材

我使用的是 eoc (Essential of Compilation)，是 IU 的教授写的，目前还没写完。可以免费下载：官方 [Github](https://github.com/IUCompilerCourse/public-student-support-code) 。这本书也包含在本次教程的 [Gitee](https://gitee.com/siriusdemon/Rust-One-Piece) 中。

### Rust

书中使用的语言是 Racket，官方 Github 提供了许多测试及辅助代码。决定用另一种语言写，意味着你没办法使用那些辅助代码，这相当具有挑战性，但如果你赢了，相信会获得对编译器更全面的理解。

我对 Python 其实更熟悉，C++ 也是候选，但选择 Rust，则是因为 Rust 有 match，而另外两者没有。

你可以选择任意你喜欢的语言来学习 eoc，但是有 match 会愉快很多。


### 目前进展

+ [R0](blog/r0.md)
+ [R1 一](blog/r1.md)
+ [R1 二](blog/r1_2.md)
+ [R1 三](blog/r1_3.md)
+ [R1 四](blog/r1_4.md)
+ [R1 Plus](blog/r1plus.md)
+ [R2 一](blog/r2.md)
+ [R2 二](blog/r2_2.md)
+ [R2 三](blog/r2_3.md)
+ [R2 四](blog/r2_4.md)
+ [R2 五](blog/r2_5.md)
+ [R3](blog/r3.md)
+ [R3 一](blog/r3_1.md)
+ [R3 二](blog/r3_2.md)

### 初心

对编译器的兴趣，来源于王垠。我从他的[博客](http://www.yinwang.org/)中收获了许多宝贝。在知道王垠之前，我也听过 Lisp 语言的大名。原因在于，我大一的时候，在百度上搜索：成为一名黑客要学习什么语言？看到有一个回答说：要学习四种语言，一是 C，二是 Python，三是 Perl，四是 Lisp。这次 EOC 所实现的语言是 Racket，也是一种 Lisp 方言。

有一段时间的工作内容是调研 TVM，现在深度学习编译器的佼佼者，当时便萌生了写编译器的想法。

这次刚好有 EOC 的开源和 Rust，我想写一系列的博客加以解说，但愿对于学习 Rust 和编译器的同行都会有所帮助。

### PS

EoC 本身具有很多不错的优点，但如果你也学习一下 P523，则会对两者的优势劣势有更清楚的认识。现在，P523-Rust 也有自己的一个仓库了，推荐你去看看。