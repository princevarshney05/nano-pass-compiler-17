#lang racket
(require "../utilities.rkt")

(provide expose-allocation)

(define (expose-exp e)
    (match e
        [(Int n) (Int n)]
        [(Var x) (Var x)]
        [(Bool b) (Bool b)]
        [(Void) (Void)]
        [(SetBang x e) (SetBang x (expose-exp e))]
        [(Let x e1 e2) (Let x (expose-exp e1) (expose-exp e2))]
        [(If e1 e2 e3) (If (expose-exp e1) (expose-exp e2) (expose-exp e3))]
        [(Prim op es) 
            (map expose-exp es)]
        [(Begin es e) (Begin (map expose-exp es) (expose-exp e))]
        [(WhileLoop e1 e2) 
            (WhileLoop (expose-exp e1) (expose-exp e2))]
        [(HasType (Prim 'vector es) t) 
            (define exposed-elements (map expose-exp es))
            (define tmp (gensym 'vec))
            (define vars (map (lambda (e) (gensym 'tmpv)) exposed-elements))
            (make-has-type exposed-elements tmp vars t)]))
        
(define (make-has-type exposed-elements vecsym vars type)
    (match exposed-elements
        [(cons e es)
                (Let (car vars) e (make-has-type es vecsym (cdr vars)))]
        ['() (allocate-init vecsym vars type)]))

(define (allocate-init vecsym vars type)


(define (expose-allocation p)
  (match p
    [(Program info e) (Program info (expose-exp e))]))