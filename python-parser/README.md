# Python Parser

一个简而不易的 Python Syntax Parser（PSP）。支持以下语法

```py
def myfun(a, b):
    return a + b

def myfun2(a, b):
    return myfun(a, a) + myfun(b, b)

x = 10
y = 10 * (myfun(x, 42))
print(x, y)
```

+ 函数
+ 变量定义

本项目的重点是 PSP，而不是解释器和编译器！后两者推荐看看 EOC 。


本项目仍处于冻结中。


