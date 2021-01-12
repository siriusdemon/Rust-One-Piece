# type

## arithmetic and logical

+ p8, p16, p32, p64, p128, p256 ... numeric type p in [u, i, f]
+ real number
+ true false
+ #x12 #b10101010 #o121 #3r1212121 ... same as other lisp

and their operator

## char and string

+ char: #\c #\b #\a
+ string: "cba"

## function

+ lambda, schemm-style

## collections

+ array: [1 2 3]
+ vector: (vec 1 2 3) (to-vec [1 2 3]) #v(1 2 3)
+ hashmap: #h(a: b, c: d) #heq(...) #heql(...) #heqv(...)
+ hashset: #s(a b c)
+ ndarray: aka vector, matrix, tensor, especially in math