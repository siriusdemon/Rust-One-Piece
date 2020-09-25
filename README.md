# Rust One Piece | 写一个编译器

想写一个编译器。写给那些与我一样，对编译器感兴趣的你，愿你永远不会退失学习的乐趣。

写编译器不容易，不言弃更困难。我决定用并不熟悉的 Rust 写我不曾写过的编译器，这就像是路飞说他要当海贼王。这份记录我称之为 Rust One Piece。

### 教材

我使用的是 eoc (Essential of Compilation)，是 IU 的教授写的，目前还没写完。可以免费下载：官方 [Github](https://github.com/IUCompilerCourse/public-student-support-code) 。这本书也包含在本次教程的 [Github](https://github.com/siriusdemon/eoc) 中。

### Rust

书中使用的语言是 Racket，官方 Github 提供了许多测试及辅助代码。决定用另一种语言写，意味着你没办法使用那些辅助代码，这相当具有挑战性，但如果你赢了，相信会获得对编译器更全面的理解。

我对 Python 其实更熟悉，C++ 也是候选，但选择 Rust，则是因为 Rust 有 match，而另外两者没有。

你可以选择任意你喜欢的语言来学习 eoc，但是有 match 会愉快很多。如果你选择了 Rust ……

那么，我们上路吧！