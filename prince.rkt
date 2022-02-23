#lang racket


(require "compiler.rkt")
(require "utilities.rkt")

(define src-primitives
  '(read + - * eq? < <= > >= and or not
         vector vector-ref vector-set! vector-length
         procedure-arity
         boolean? integer? vector? procedure? void?
         any-vector-ref any-vector-set! any-vector-length
         make-vector))

(define (parse-exp e)
  (match e
    [(? symbol?) (Var e)]
    [(? fixnum?) (Int e)]
    [(? boolean?) (Bool e)]
    [`(void) (Void)]
    [`(let ([,x ,rhs]) ,body) (Let x (parse-exp rhs) (parse-exp body))]
    [`(if ,cnd ,thn ,els) (If (parse-exp cnd) (parse-exp thn) (parse-exp els))]
    [`(lambda: ,ps : ,rt ,body)
     (Lambda ps rt (parse-exp body))]
    [`(lambda: ,ps ,body)
     (Lambda ps 'Any (parse-exp body))]
    [`(lambda ,ps ,body) ;; dynamically typed lambda
     (Lambda ps 'Any (parse-exp body))]
    [`(project ,e ,t)
     (Project (parse-exp e) t)]
    [`(inject ,e ,t)
     (Inject (parse-exp e) t)]
    [`(while ,cnd ,body)
     (WhileLoop (parse-exp cnd) (parse-exp body))]
    [`(set! ,x ,rhs)
     (SetBang x (parse-exp rhs))]
    [`(begin ,es ... ,e)
     (Begin (for/list ([e es]) (parse-exp e)) (parse-exp e))]
    [`(has-type ,e ,t)
     (HasType (parse-exp e) t)]
    [`(,op ,es ...)
     #:when (set-member? src-primitives op)
     (Prim op (for/list ([e es]) (parse-exp e)))]

    ))



; (explicate_tail (parse-exp '(let ([x (let ([y (- 42)]) y) ]) (- x))))

;; (Seq (Assign (Var 'y) (Prim '- (list (Int 42)))) (Seq (Assign (Var 'x) (Var 'y)) (Return (Prim '- (list (Var 'x))))))

;; (explicate_tail (parse-exp '(let ([x 10]) x)))

;; (explicate_tail (parse-exp '(let ([x 41]) (+ x 1))))





; (map-instrs (create-env '(x temp) -8) 
;               (list 
;               (Instr 'addq (list (Imm 10) (Var 'x))) 
;               (Instr 'movq (list (Var 'x) (Var 'temp)))))

; (explicate_tail (parse-exp '(read)))

; (rco-exp (parse-exp '(let ([x (read)]) x)))
; (rco-exp (parse-exp '(read)))

(parse-exp '(+ (+ (+ 42 (- 10)) 15) 23))
