#lang racket

(require "../utilities.rkt")

(provide reveal-functions)

(define (set-func-names def-arg-cnt-map)
  (lambda (exp)
    (match exp
      [(Var x) (if (hash-has-key? def-arg-cnt-map x) (FunRef x (hash-ref def-arg-cnt-map x)) exp)]
      [(Int x) (Int x)]
      [(Bool x) (Bool x)]
      [(Void) (Void)]
      [(GetBang x) (GetBang x)]
      [(Prim op es) (Prim op (map (set-func-names def-arg-cnt-map) es))]
      [(If e1 e2 e3)
       (If ((set-func-names def-arg-cnt-map) e1)
           ((set-func-names def-arg-cnt-map) e2)
           ((set-func-names def-arg-cnt-map) e3))]
      [(Let x e body)
       (Let x ((set-func-names def-arg-cnt-map) e) ((set-func-names def-arg-cnt-map) body))]
      [(SetBang x e) (SetBang x ((set-func-names def-arg-cnt-map) e))]
      [(Begin es rest-exp)
       (Begin (map (set-func-names def-arg-cnt-map) es) ((set-func-names def-arg-cnt-map) rest-exp))]
      [(HasType e type) (HasType ((set-func-names def-arg-cnt-map) e) type)]
      [(WhileLoop cnd body)
       (WhileLoop ((set-func-names def-arg-cnt-map) cnd) ((set-func-names def-arg-cnt-map) body))]
      [(Apply func args)
       (Apply ((set-func-names def-arg-cnt-map) func) (map (set-func-names def-arg-cnt-map) args))])))

(define (reveal-functions-defs def-arg-cnt-map)
  (lambda (def)
    (match def
      [(Def name param rty info body)
       (Def name param rty info ((set-func-names def-arg-cnt-map) body))])))

(define (reveal-functions p)
  (match p
    [(ProgramDefs info defs)
     (define def-arg-cnt-map (make-hash))
     (for ([def defs])
       (match def
         [(Def name param rty info body) (dict-set! def-arg-cnt-map name (length param))]))
     (ProgramDefs info (map (reveal-functions-defs def-arg-cnt-map) defs))]))
