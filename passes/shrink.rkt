#lang racket

(require "../utilities.rkt")

(provide shrink)

(define (shrink-exp exp)
  (match exp
    ['() '()]
    [(Prim 'and (list e1 e2)) (If (shrink-exp e1) (shrink-exp e2) (Bool #f))]
    [(Prim 'or (list e1 e2)) (If (shrink-exp e1) (Bool #t) (shrink-exp e2))]
    [(Int n) (Int n)]
    [(Var x) (Var x)]
    [(Bool t) (Bool t)]
    [(Let x e1 e2) (Let x (shrink-exp e1) (shrink-exp e2))]
    [(If e1 e2 e3) (If (shrink-exp e1) (shrink-exp e2) (shrink-exp e3))]
    [(Begin es exp) (Begin (map shrink-exp es) (shrink-exp exp))]
    [(SetBang x e) (SetBang x (shrink-exp e))]
    [(WhileLoop e1 e2) (WhileLoop (shrink-exp e1) (shrink-exp e2))]
    [(Prim '- (list e1 e2)) (Prim '+ (list (shrink-exp e1) (Prim '- (list (shrink-exp e2)))))]
    [(Prim '> (list e1 e2))
     (define v (gensym))
     (Let v (shrink-exp e1) (Prim '< (list (shrink-exp e2) (Var v))))]
    [(Prim '<= (list e1 e2))
     (define v (gensym))
     (Let v (shrink-exp e1) (Prim 'not (list (Prim '< (list (shrink-exp e2) (Var v))))))]
    [(Prim '>= (list e1 e2)) (Prim 'not (list (Prim '< (list (shrink-exp e1) (shrink-exp e2)))))]
    [(HasType e type) (HasType (shrink-exp e) type)]
    [(Prim op es)
     (Prim op
           (for/list ([e es])
             (shrink-exp e)))]))

(define (shrink p)
  (match p
    [(Program info e) (Program info (shrink-exp e))]))
