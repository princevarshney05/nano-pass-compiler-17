#lang racket
(require "../utilities.rkt")
(provide uniquify)

; environment contains mapping between
(define (uniquify-exp env)
  (lambda (e)
    (match e
      ; Variable exists, but we need a one to one mapping from dictionary
      [(Var x) (Var (dict-ref env x))]
      [(Int n) (Int n)]
      [(Bool t) (Bool t)]
      [(Void) (Void)]
      [(Let x e body)
       (let ([newenv (dict-set env x (gensym x))])
         (Let (dict-ref newenv x) ((uniquify-exp env) e) ((uniquify-exp newenv) body)))]
      [(If e1 e2 e3) (If ((uniquify-exp env) e1) ((uniquify-exp env) e2) ((uniquify-exp env) e3))]
      [(SetBang x e) 
        (let ([newenv (dict-set env x (gensym x))])
            (SetBang (dict-ref newenv x) ((uniquify-exp env) e)))]
      [(Begin es exp)
        (Begin (map (uniquify-exp env) es) ((uniquify-exp env) exp))]
      [(WhileLoop e1 e2) 
        (WhileLoop ((uniquify-exp env) e1) ((uniquify-exp env) e2))]
      [(Prim op es)
       (Prim op
             (for/list ([e es])
               ((uniquify-exp env) e)))])))

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))