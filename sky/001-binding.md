# assignment

### global

Assignment with decorator

```lisp
(def x 1)
(def x 1 :u8)
(def x (vec 1 2 3))             ; a reference to a vector, the vector will be garbage collected
(def x (vec 1 2 3) :owned)      ; a vector which will move
(def y (ref x))                 ; y is a mutable reference to x
(def z (ref x :const))          ; z is a immutable reference to x
(def y &x)                      ; shortcut for mutable reference, not way for immutable reference
```

see also

```lisp
(ownership-default)             ; change def to define ownership object
(no-garbage-collector)          ; ensure garbage-collector is remove
*ownership-default*             ; true or false
*garbage-collector*             ; true or false
defowned
defrc
```

### local

(let (bindings)
    body)

(let* (bindings)
    body)

### function

```lisp
(defn f (a b c)
  (...))

(declare
    f :Fn((args) ret)
    a :u8
    b :&u16 
    c :&real :const
)
(defn f (a :u8 b :&u16 c :&real :const) -> :Fn((args) ret)
    (dfdf)
    (fdfd)
    (fdfdf)
)

(defn f (a) -> _
   (..) )
```

see also

```lisp
(force-return-type)
*force-return-type*
(force-type-declaretion)
*force-type-declaretion*
```
