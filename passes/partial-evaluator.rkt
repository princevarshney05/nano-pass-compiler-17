#lang racket

(require "../utilities.rkt")
(require racket/set
         racket/stream)
(require racket/fixnum)

(provide pe_Lif)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bonus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pe_neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe_add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe_sub r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx- n1 n2))]
    [(_ _) (Prim '- (list r1 r2))]))

(define (pe_exp e)
  (match e
    [(Int n) (Int n)]
    [(Var x) (Var x)]
    [(Bool t) (Bool t)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe_neg (pe_exp e1))]
    [(Prim '+ (list (Int e1) e2)) (pe_add (Int e1) (pe_exp e2))]
    [(Prim '+ (list e1 (Int e2))) (pe_add (Int e2) (pe_exp e1))]
    [(Prim '+ (list e1 e2)) (pe_add (pe_exp e1) (pe_exp e2))]
    [(Prim op es)
     (Prim op
           (for/list ([e es])
             (pe_exp e)))]
    [(Let x e1 e2) (Let x (pe_exp e1) (pe_exp e2))]
    [(If e1 e2 e3) (If (pe_exp e1) (pe_exp e2) (pe_exp e3))]))

; Add a pe_intelligent_eval that evaluates the function from left to right
; and then evaluates the result
(define (pe_intelligent_exp e)
  (match e
    [(Int n) (Int n)]
    [(Var x) (Var x)]
    [(Bool t) (Bool t)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe_neg (pe_intelligent_exp e1))]
    [(Prim '+ (list (Int e1) (Prim '+ (list (Int e2) e3))))
     (pe_add (pe_add (Int e1) (Int e2)) (pe_intelligent_exp e3))]
    [(Prim '+ (list (Prim '+ (list (Int e1) e2)) (Prim '+ (list (Int e3) e4))))
     (pe_add (pe_add (Int e1) (Int e3)) (pe_add (pe_intelligent_exp e2) (pe_intelligent_exp e4)))]
    [(Prim '+ (list e1 e2)) (pe_add (pe_intelligent_exp e1) (pe_intelligent_exp e2))]
    [(Prim op es)
     (Prim op
           (for/list ([e es])
             (pe_intelligent_exp e)))]
    [(Let x e1 e2) (Let x (pe_intelligent_exp e1) (pe_intelligent_exp e2))]
    [(If e1 e2 e3) (If (pe_intelligent_exp e1) (pe_intelligent_exp e2) (pe_intelligent_exp e3))]))

(define (pe_Lif p)
  (match p
    [(Program '() e) (Program '() (pe_intelligent_exp (pe_exp e)))]))
