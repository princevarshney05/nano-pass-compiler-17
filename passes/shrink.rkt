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
    [(Void) (Void)]
    [(Let x e1 e2) (Let x (shrink-exp e1) (shrink-exp e2))]
    [(If e1 e2 e3) (If (shrink-exp e1) (shrink-exp e2) (shrink-exp e3))]
    [(Begin '() exp) (shrink-exp exp)]
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
    [(Apply e es) (Apply (shrink-exp e) (map shrink-exp es))]
    [(Prim op es)
     (Prim op
           (for/list ([e es])
             (shrink-exp e)))]))

(define (apply-shrink def)
  (match def
    [(Def label args rtype info body) (Def label args rtype info (shrink-exp body))]))

(define (shrink p)
  (match p
    [(ProgramDefsExp info defs e)
     (ProgramDefs info (map apply-shrink (append defs (list (Def 'main '() 'Integer '() e)))))]))
