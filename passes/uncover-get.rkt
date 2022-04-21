#lang racket
(require "../utilities.rkt")
(provide uncover-get!)

; (define (uncover-get! p)
;   (match p
;     [(Program info e)
;      (define collect-set-vars (collect-set! e))
;      (Program info ((uncover-get!-exp collect-set-vars) e))]))

(define (uncover-get!-defs def)
    (match def 
      [(Def label args rtype info body) 
       (define collect-set-vars (collect-set! body))
       (Def label args rtype info ((uncover-get!-exp collect-set-vars) body))
      ]
    )
)

(define (uncover-get! p)
  (match p
    [(ProgramDefs info defs) (ProgramDefs info (map uncover-get!-defs defs)) ]))

(define (collect-set! e)
  (match e
    [(Var x) (set)]
    [(Int n) (set)]
    [(Bool b) (set)]
    [(Void) (set)]
    [(Allocate e1 e2) (set)]
    [(GlobalValue e1) (set)]
    [(Collect e1) (set)]
    ; Handling all empty arguments separately because we can't use apply on empty list
    [(Prim op '()) (set)]
    [(Prim op es)
     (apply set-union
            (for/list ([e es])
              (collect-set! e)))]
    [(If e1 e2 e3) (set-union (collect-set! e1) (collect-set! e2) (collect-set! e3))]
    [(Let x rhs body) (set-union (collect-set! rhs) (collect-set! body))]
    [(WhileLoop e1 e2) (set-union (collect-set! e1) (collect-set! e2))]
    [(SetBang x e) (set-union (set x) (collect-set! e))]
    [(Apply func args) (apply set-union (for/list ([e (append (list func) args)]) (collect-set! e) ))]
    ; Do for begin
    [(Begin es exp)
     (apply set-union
            (for/list ([e (append es (list exp))])
              (collect-set! e)))]))

(define ((uncover-get!-exp set!-vars) e)
  (match e
    [(Int n) (Int n)]
    [(Bool b) (Bool b)]
    [(Void) (Void)]
    [(Allocate e1 e2) (Allocate e1 e2)]
    [(GlobalValue e1) (GlobalValue e1)]
    [(Collect e1) (Collect e1)]
    [(Prim op es)
     (Prim op
           (for/list ([e es])
             ((uncover-get!-exp set!-vars) e)))]
    [(If e1 e2 e3)
     (If ((uncover-get!-exp set!-vars) e1)
         ((uncover-get!-exp set!-vars) e2)
         ((uncover-get!-exp set!-vars) e3))]
    [(Let x rhs body) (Let x ((uncover-get!-exp set!-vars) rhs) ((uncover-get!-exp set!-vars) body))]
    [(WhileLoop e1 e2)
     (WhileLoop ((uncover-get!-exp set!-vars) e1) ((uncover-get!-exp set!-vars) e2))]
    [(SetBang x e) (SetBang x ((uncover-get!-exp set!-vars) e))]
    [(Apply func args) (Apply ((uncover-get!-exp set!-vars) func) (map (uncover-get!-exp set!-vars) args))]

    [(Begin es exp)
     (Begin (for/list ([e es])
              ((uncover-get!-exp set!-vars) e))
            ((uncover-get!-exp set!-vars) exp))]
    [(Var x) (if (set-member? set!-vars x) (GetBang x) (Var x))]))
