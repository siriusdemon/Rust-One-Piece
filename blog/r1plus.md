### R1 Plus

这实际上是 EOC 的第三章，写一个自动寄存器分配算法。但我对自己所写的代码并不够满意，如果强行写博客，恐怕也会把读者绕得云里雾里的。我想也许可以继续前进，过段时间再回头来看看这部分的代码，或许会得到更清晰的认识。

代码仅供参考，感谢@戚仁明 跟我一起学习！

```sh
compiler.rs    : 557
helper.rs      : 49
main.rs        : 49
parser.rs      : 63
syntax.rs      : 109
test.rs        : 241
---------------------
total          : 1068
```

### fix

在写 R2 的时候，发现了之前写 R1 的许多问题，修改了 `syntax.rs` 中的一些数据结构和字段类型。但因为太琐碎，而且对读者的理解影响较小，所以不打算罗列出来，代码见 R2 部分。

但这里要提及一个比较重要的 pass 的改动。在 Rust One Piece | R1（三） 中，我写了一个很不好的 explicate_control 的实现。现在我已经换成了与 Eoc 作者提示相契合的版本。

作者建议我们用两个辅助函数， explicate_assign 以及 explicate_tail 来完成这个 pass 。explicate_assign 只应用于赋值的表示式，explicate_tail 只应用于 tail 语句。具体而言：

+ explicate_assign 有三个参数，第一个是要被赋值的变量，第二个是变量的值，第三个是紧跟在赋值语句之后的 tail，它的返回是一个 tail
+ explicate_tail 有一个参数，一个 tail 语句。

怎么应用呢？

+ 由于整个程序就是一个 tail，所以首先对整个表达式应用 explicate_tail
+ 当遇到赋值语句 let 的时候，应用 explicate_assign。explicate_assign 的三个参数与 let 的三个表达式是一一对应的。

下面跟着代码分析。

```rs
use crate::syntax::{C0, C0Program};
pub fn explicate_control(expr: Expr) -> C0Program {
    let mut cfg = vec![];
    let mut locals = vec![];
    let expr = explicate_tail(expr, &mut cfg, &mut locals);
    cfg.push(("start".to_string(), expr));
    C0Program { locals, cfg }
}
```

explicate_control 的主体部分，调用 explicate_tail。我额外传入了 cfg 和 locals，这是为了就地修改这两个结构。

```rs
fn explicate_tail(expr: Expr, cfg: &mut Vec<(String, C0)>, locals: &mut Vec<C0>) -> C0 {
    match expr {
        Let(box Var(x), box e, box body) => {
            let tail = explicate_tail(body, cfg, locals);
            let tail = explicate_assign(C0::Var(x), e, tail, locals, cfg);
            return tail;
        }
        e => {
            return C0::Return(Box::new(expr_to_C0(e)));
        }
    }
}
```

explicate_tail 目前只对 let 语句作额外处理。因为 let 语句的 body 是最后执行的，它也是一个 tail，所以先对 body 求值，得到一个新的 tail。调用 explicate_assign 来对赋值作处理。其他的表达式则用 expr_to_C0 转换成 C0 的结构，然后用 Return 返回。

```rs
fn explicate_assign(x: C0, expr: Expr, tail: C0, locals: &mut Vec<C0>, cfg: &mut Vec<(String, C0)>) -> C0 {
    use C0::{Assign, Seq};
    match expr {
        Let(box Var(x_), box e, box body) => {
            let tail = explicate_assign(x, body, tail, locals, cfg);
            let e = explicate_assign(C0::Var(x_), e, tail, locals, cfg);
            return e;
        },
        e => {
            locals.push(x.clone());
            let e = expr_to_C0(e);
            let assign = Assign(Box::new(x), Box::new(e));
            return Seq(Box::new(assign), Box::new(tail));
        }
    }
}
```
explicate_assign 是这个 pass 中最重要的一个函数。它本身代表了一个 let 语句，它的三个参数与 let 语句一一对应。所以，它的 match 分支主要是在处理嵌套 let 语句的问题。

当遇到一个 let 时，我们有五个变量：

+ x: 原来的 let 语句的变量。
+ tail：原来的 let 的（转换后的）body。
+ expr: 分解成了嵌套 let 的三个变量
  + x_： 嵌套 let 语句的变量
  + e: 嵌套 let 的变量的值
  + body: 嵌套 let 的 body

执行的顺序是，x_ 先赋值，它的值是 e；然后 x 赋值，它的值是嵌套 let 的 body；最后是 tail 执行。但在构建的时候，我们需要先构建 tail，再构建 Assign，用代码表达如下：

```rs
            let tail = explicate_assign(x, body, tail, locals, cfg);
            let e = explicate_assign(C0::Var(x_), e, tail, locals, cfg);
```

最后，是一个简单的转换函数

```rs
fn expr_to_C0(expr: Expr) -> C0 {
    match expr {
        Int(n) => C0::Int(n),
        Var(x) => C0::Var(x),
        Prim0(op) => C0::Prim0(op),
        Prim1(op, box e) => C0::Prim1(op, Box::new( expr_to_C0(e) )),
        Prim2(op, box e1, box e2) => C0::Prim2(op, Box::new( expr_to_C0(e1) ), Box::new( expr_to_C0(e2) )),
        e => {
            println!("{:?}", e);
            panic!("No complex structure! Handle it by yourself!");
        }
    }
}
```

这一部分代码已经更新到 r1plus 文件夹下。原来的 r1 代码保持不变，以供参考。