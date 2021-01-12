; continuation
(define atom?
  (lambda (exp)
    (not (pair? exp))))

(define (evaluate e r k)
  (if (atom? e)
    (cond 
      [(symbol? e) (evaluate-variable e r k)]
      [else        (evaluate-quote e r k)])
    (case (car e)
      ['qoute   (evaluate-quote e r k)]
      ['if      (evaluate-if (cadr e) (caddr e) (cadddr e) r k)]
      ['begin   (evaluate-begin (cdr e) r k)]
      ['set!    (evaluate-set! (cadr e) (caddr e) r k)]
      ['lambda  (evaluate-lambda (cadr e) (cddr e) r k)]
      [else     (evaluate-applicate (car e) (cdr e) r k)])))
  