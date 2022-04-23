#lang racket

(require "../utilities.rkt")

(provide limit-functions)

; maintain a mapping between the remaining args and their indexes
; create a variable name for var-vector

(define CNT 6)

(define ((limit-subfuncs var-ind-map vector-var-name) exp)
  (match exp
    [(Void) (Void)]
    [(Bool x) (Bool x)]
    [(Int x) (Int x)]
    [(FunRef x y) (FunRef x y)]
    [(Var x)
     (if (hash-has-key? var-ind-map x)
         (Prim 'vector-ref (list (Var vector-var-name) (Int (hash-ref var-ind-map x))))
         exp)]
    [(GetBang x)
     (if (hash-has-key? var-ind-map x)
         (Prim 'vector-ref (list (Var vector-var-name) (Int (hash-ref var-ind-map x))))
         exp)]
    [(SetBang x rhs)
     (if (hash-has-key? var-ind-map x)
         (Prim 'vector-set!
               (list (Var vector-var-name)
                     (Int (hash-ref var-ind-map x))
                     ((limit-subfuncs var-ind-map vector-var-name) rhs)))
         (SetBang x ((limit-subfuncs var-ind-map vector-var-name) rhs)))]
    [(HasType e type) (HasType ((limit-subfuncs var-ind-map vector-var-name) e) type)]
    [(Let x e body)
     (Let x
          ((limit-subfuncs var-ind-map vector-var-name) e)
          ((limit-subfuncs var-ind-map vector-var-name) body))]
    [(If e1 e2 e3)
     (If ((limit-subfuncs var-ind-map vector-var-name) e1)
         ((limit-subfuncs var-ind-map vector-var-name) e2)
         ((limit-subfuncs var-ind-map vector-var-name) e3))]
    [(Prim op es) (Prim op (map (limit-subfuncs var-ind-map vector-var-name) es))]
    [(WhileLoop cnd body)
     (WhileLoop ((limit-subfuncs var-ind-map vector-var-name) cnd)
                ((limit-subfuncs var-ind-map vector-var-name) body))]
    [(Begin es last-exp)
     (Begin (map (limit-subfuncs var-ind-map vector-var-name) es)
            ((limit-subfuncs var-ind-map vector-var-name) last-exp))]
    [(Apply func args)
     (if (> (length args) CNT)
         (Apply ((limit-subfuncs var-ind-map vector-var-name) func)
                (let ([tmp-args (map (limit-subfuncs var-ind-map vector-var-name) args)])
                  (define var-list (take tmp-args (- CNT 1)))
                  (define vec-list (list (Prim 'vector (drop tmp-args (- CNT 1)))))
                  (append var-list vec-list)))
         (Apply ((limit-subfuncs var-ind-map vector-var-name) func)
                (map (limit-subfuncs var-ind-map vector-var-name) args)))]))

(define (limit-functions-def def)
  (match def
    [(Def name param rty info body)
     (define vector-var-name (gensym 'var-vec))
     (define new-param
       (if (> (length param) CNT)
           (match param
             [`([,xs : ,ts] ...)
              (append (take param (- CNT 1)) `([,vector-var-name : (Vector ,@(drop ts (- CNT 1)))]))])
           param))

     (define var-ind-map (make-hash))
     (when (> (length param) CNT)
       (for ([i (in-naturals)] [(x _) (in-dict (drop param (- CNT 1)))])
         (dict-set! var-ind-map x i)))

     (Def name new-param rty info ((limit-subfuncs var-ind-map vector-var-name) body))]))

(define (limit-functions p)
  (match p
    [(ProgramDefs info defs) (ProgramDefs info (map limit-functions-def defs))]))
