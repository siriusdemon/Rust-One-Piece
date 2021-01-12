; For the interpreter defined in last chapter, the salient points in it are these:
; 1. lambda is a special form creating first class objects; closures capture their
; definition environment.
; 2. All the terms in an application are evaluated by the same evaluator, namely,
; evaluate; evlis is simply evaluate mapped over a list of expressions.
; That second characteristic makes Scheme into LisP1.

(define atom?
  (lambda (exp)
    (not (pair? exp))))


(define (lookup id env)
  (if (pair? env)
    (if (eq? (caar env) id)
      (cdar env)
      (lookup id (cdr env)))
    (error "lookup: Undefine variable" id)))

(define (update! id env value)
  (if (pair? env)
    (if (eq? (caar env) id)
      (begin (set-cdr! (car env) value)
             value) ; update return the value
      (update! id (cdr env) value))
    (error "update! Undefine variable" id)))

(define autoquote?
  (lambda (e)
    (or (number? e) (string? e) (char? e) (boolean? e) (vector? e))))

(define (eprogn exps env fenv)
  (if (pair? exps)
    (if (pair? (cdr exps))
      (begin (evaluate (car exps) env fenv)
             (eprogn (cdr exps) env fenv))
      (evaluate (car exps) env fenv)) ; only one e in exps
    '()))

(define (evlis exps env fenv)
  (if (pair? exps)
    (let ((arg1 (evaluate (car exps) env fenv)))
      (cons arg1 (evlis (cdr exps) env fenv)))
    '()))

(define env-init '())
(define (extend env variables values)
  (cond ((pair? variables)
         (if (pair? values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (error "extend: too less values" 'M)))
        ((null? variables)
         (if (null? values)
             env
             (error "extend: too much values" 'M))) 
        ; dot-pair
        ((symbol? variables) (cons (cons variables values) env))))

;; function
(define (invoke fn args)
  (if (procedure? fn)
    (fn args)
    (error "invoke: Not a function" fn)))

(define (make-function variables body env fenv)
  (lambda (values)
    (eprogn body (extend env variables values) fenv)))

(define (evaluate-application fn args env fenv)
  (cond ((symbol? fn)
         ((lookup fn fenv) args))
        ((and (pair? fn) (eq? (car fn) 'lambda))
         (eprogn (cddr fn) 
                 (extend env (cadr fn) args)
                 fenv))
        (else (error "evaluate-application: Incorrect functional term" fn))))

(define (evaluate e env fenv)
  (if (atom? e)
    (cond 
      ((symbol? e) (lookup e env))
      ((autoquote? e) e)
      (else (error "evaluate: unknown expr" e))   
    )
    (case (car e)
      ('quote (cadr e))
      ('if (if (evaluate (cadr e) env fenv)
               (evaluate (caddr e) env fenv)
               (evaluate (cadddr e) env fenv)))
      ('begin (eprogn (cdr e) env fenv))
      ('set! (update! (cadr e) env (evaluate (caddr e) env fenv)))
      ; lack test
      ('lambda (make-function (cadr e) (cddr e) env fenv))
      ('function (cond ((symbol? (cadr e))
                        (lookup (cadr e) fenv))
                       ((and (pair? (cadr e)) (eq? 'lambda (caadr e)))
                        (evaluate (cadr e) env fenv))
                       (else (error "evaluate function: Incorrect function" (cadr e)))))
      ('labels 
        (eprogn (cddr e) env (extend fenv
                                (map car (cadr e)) ; function name list
                                (map (lambda (def) (make-function 
                                                      (cadr def) ; args
                                                      (cddr def) ; body
                                                      env fenv))
                                      (cadr e)))))
      (else (evaluate-application (car e)
                                  (evlis (cdr e) env fenv)
                                  env
                                  fenv)))))
                  
(define (funcall args)
  (if (> (length args) 1)
      (invoke (car args) (cdr args))
      (error "funcall Incorrect arity" (car args))))

; using lisp2
(define env-global env-init)
(define-syntax definitial
  (syntax-rules ()
    ((_ name)
     (begin (set! env-global (cons (cons 'name 'void) env-global))
        'name))
    ((_ name value)
     (begin (set! env-global (cons (cons 'name value) env-global))
        'name))))


(define fenv-global '())
(define-syntax definitial-function
  (syntax-rules ()
    ((_ name)
     (begin (set! fenv-global (cons (cons 'name 'void) fenv-global)) 
            'name))
    ((_ name value)
     (begin (set! fenv-global (cons (cons 'name value) fenv-global))
            'name))))

(define-syntax defprimitive
  (syntax-rules ()
    ((_ name value arity)
     (definitial-function name 
      (lambda (values)
        (if (= arity (length values))
            (apply value values)
            (error "Incorrect arity" (list 'name values))))))))

(defprimitive car car 1)
(defprimitive cons cons 2)
(defprimitive + + 2)
; (defprimitive funcall funcall 1)
; (defprimitive function function 1)
       