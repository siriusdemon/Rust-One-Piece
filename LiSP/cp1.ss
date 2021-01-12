(define atom?
  (lambda (exp)
    (not (pair? exp))))


; the goal of a program is not so much to avoid committing errors 
; but rather to fulfil its duty
(define (lookup id env)
  (if (pair? env)
    (if (eq? (caar env) id)
      (cdar env)
      (lookup id (cdr env)))
    (error "Undefine variable" id)))

(define (update! id env value)
  (if (pair? env)
    (if (eq? (caar env) id)
      (begin (set-cdr! (car env) value)
             value) ; update return the value
      (update! id (cdr env) value))
    (error "Undefine variable" id)))

(define autoquote?
  (lambda (e)
    (or (number? e) (string? e) (char? e) (boolean? e) (vector? e))))

(define (eprogn exps env)
  (if (pair? exps)
    (if (pair? (cdr exps))
      (begin (evaluate (car exps) env)
             (eprogn (cdr exps) env))
      (evaluate (car exps) env)) ; only one e in exps
    '()))

; make argument evaluate order explicit
(define (evlis exps env)
  (if (pair? exps)
    (let ((arg1 (evaluate (car exps) env)))
      (cons arg1 (evlis (cdr exps) env)))
    '()))

;; environment
(define env-init '())
(define (extend env variables values)
  (cond ((pair? variables)
         (if (pair? values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (error "too less values" 'M)))
        ((null? variables)
         (if (null? values)
             env
             (error "too much values" 'M))) 
        ; dot-pair
        ((symbol? variables) (cons (cons variables values) env))))

;; function
(define (invoke fn args)
  (if (procedure? fn)
    (fn args)
    (error "Not a function" fn)))

;; here, we have global env, and extend global env
(define (make-function variables body env)
  (lambda (values)
    (eprogn body (extend env variables values))))

(define (evaluate e env)
  (if (atom? e)
    (cond 
      ((symbol? e) (lookup e env))
      ((autoquote? e) e)
      (else (error "unknown expr" e))   
    )
    (case (car e)
      ('quote (cadr e))
      ('if (if (evaluate (cadr e) env)
               (evaluate (caddr e) env)
               (evaluate (cadddr e) env)))
      ('begin (eprogn (cdr e) env))
      ('set! (update! (cadr e) env (evaluate (caddr e) env)))
      ; lack test
      ('lambda (make-function (cadr e) (cddr e) env))
      (else 
        (let ((fn (evaluate (car e) env))
              (args (evlis (cdr e) env)))
          (invoke fn args))))))


;; symbol: syntactic entity, Just text itself
;; variable: semantic entity, Mean something in the program

;; In a lexical Lisp, a function evaluates its body in its own definition environment 
;; extended by its variables, whereas in a dynamic Lisp, a function extends the current 
;; environment, that is, the environment of the application.

;; useful macro

(define env-global env-init)
(define-syntax definitial
  (syntax-rules ()
    ((_ name)
     (begin (set! env-global (cons (cons 'name 'void) env-global))
        'name))
    ((_ name value)
     (begin (set! env-global (cons (cons 'name value) env-global))
        'name))))

(define-syntax defprimitive
  (syntax-rules ()
    ((_ name value arity)
     (definitial name
        (lambda (values)
          (if (= arity (length values))
              (apply value values)   ;; apply function body with args
              (error "Incorrect arity"  arity)))))))

; define constant
(define the-false-value (cons #f 813))
(definitial t #t)
(definitial f the-false-value)
(definitial nil '())

; define function
(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive eq? eq? 2)
; exe 1.5
; (defprimitive < < 2)
(defprimitive < 
  (lambda (a b)
    (if (< a b) #t the-false-value))
  2)

; exe 1.6
(defprimitive list 
  (lambda (a b)
    (cons a (cons b '())))
  2)

; exe 1.8
(defprimitive apply
  (lambda (fn args)
    (fn args))
  2)


(define (chapter1-scheme) 
  (define (toplevel)
    (display ">>> ")
    (display (evaluate (read) env-global))
    (display "\n")
    (toplevel))
  (toplevel))

; to start the interpreter
; (chapter1-scheme)

; evaluate itself -- still not work
; (define eval-test '(evaluate (quote (list 1 2)) env-global))
; (evaluate eval-test env-global)