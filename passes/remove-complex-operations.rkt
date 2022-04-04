#lang racket
(require "../utilities.rkt")
(provide remove-complex-opera*)

(define (rco-atom e)
  (match e
    [(Int n) (values (Int n) '())]
    [(Var x) (values (Var x) '())]
    [(Bool t) (values (Bool t) '())]
    [(Let key val body)
     (define-values (body-atom body-env) (rco-atom body))
     (values body-atom (cons (list key (rco-exp val)) body-env))]
    ; It is particularly important to not replace its condition with a temporary variable because
    ; that would interfere with the generation of high-quality output in the upcoming explicate_control pass
    [(If e1 e2 e3)
     (define key (gensym))
     (values (Var key) `((,key ,(rco-exp e))))]
    [(Prim op es)
     (define-values (new-exp new-env) (for/lists (l1 l2) ([e es]) (rco-atom e)))
     (define key (gensym))
     (define new-key-val-list (list (list key (Prim op new-exp))))
     (define combined-list-env (append (append* new-env) new-key-val-list))
     (values (Var key) combined-list-env)]))

(define (normalise-env-exp env exp)
  (match env
    ['() exp]
    ; [(list (list key val)) exp]
    [`(,(list key val) . ,rest-list) (Let key val (normalise-env-exp rest-list exp))]))

(define (rco-exp e)
  (match e
    [(Int n) (Int n)]
    [(Var x) (Var x)]
    [(Bool t) (Bool t)]
    [(Let key val body) (Let key (rco-exp val) (rco-exp body))]
    ; Should I add rco-exp for e1 too?
    [(If e1 e2 e3) (If (rco-exp e1) (rco-exp e2) (rco-exp e3))]
    [(Prim op es)
     (define-values (new-exp combined-list-env) (for/lists (l1 l2) ([e es]) (rco-atom e)))
     (normalise-env-exp (append* combined-list-env) (Prim op new-exp))]))

;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (match p
    [(Program info e) (Program info (rco-exp e))]))
