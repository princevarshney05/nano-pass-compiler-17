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
       (define newenv (dict-copy env))
       (dict-set! newenv x (gensym x))
       (Let (dict-ref newenv x) ((uniquify-exp env) e) ((uniquify-exp newenv) body))]
      [(If e1 e2 e3) (If ((uniquify-exp env) e1) ((uniquify-exp env) e2) ((uniquify-exp env) e3))]
      [(SetBang x e) (SetBang (dict-ref env x) ((uniquify-exp env) e))]
      [(Begin es exp) (Begin (map (uniquify-exp env) es) ((uniquify-exp env) exp))]
      [(WhileLoop e1 e2) (WhileLoop ((uniquify-exp env) e1) ((uniquify-exp env) e2))]
      [(HasType e type) (HasType ((uniquify-exp env) e) type)]
      [(Apply e es) (Apply ((uniquify-exp env) e) (map (uniquify-exp env) es))]
      [(Prim op es)
       (Prim op
             (for/list ([e es])
               ((uniquify-exp env) e)))])))

(define (rename-func-defs defs)
  (define def-map (make-hash))
  (for ([def defs])
    (match def
      [(Def label args rtype info body)
       (if (eq? label 'main)
           (dict-set! def-map label 'main)
           (dict-set! def-map label (gensym label)))]))
  def-map)

(define (uniquify-defs def-map)
  (lambda (def)
    (match def
      [(Def label args rtype info body)
       (define new-env def-map)
       (define new-label (dict-ref def-map label))
       (define new-args '())
       (for ([arg args])
         (dict-set! new-env (car arg) (gensym (car arg)))
         (set! new-args (append new-args (list (cons (dict-ref new-env (car arg)) (cdr arg))))))
       (Def new-label new-args rtype info ((uniquify-exp new-env) body))])))

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(ProgramDefs info defs)
     (define def-map (rename-func-defs defs))
     (ProgramDefs info (map (uniquify-defs def-map) defs))]))
