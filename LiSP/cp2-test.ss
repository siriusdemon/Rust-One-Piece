(load "cp2.ss")


; (define test-atom-symbol (evaluate 'ha env-init))
(define (helper e)
  (let ((res (evaluate e env-global fenv-global)))
    (display res)
    (newline))
)

(helper "hah")
(helper "42")
(helper #\c)
(helper (vector #\c 42))
(helper '(quote (this is my quote)))
(helper '(quote this))
(helper '(if #t #f #t))
(helper '(begin "haha" #\c (if #f #t #f)))
(helper '((lambda (x) x) 10))
(helper '((lambda (x) (car x)) '(10 20)))
(helper '(labels ((square (x) (+ x x)))
            (square 10)))
; (helper '(funcall '((function +) 10 20))))