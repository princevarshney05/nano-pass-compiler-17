#lang racket
(require "../utilities.rkt")

(provide expose-allocation)

(define (expose-exp e)
  (match e
    [(Int n) (Int n)]
    [(Var x) (Var x)]
    [(Bool b) (Bool b)]
    [(Void) (Void)]
    [(FunRef x y) (FunRef x y)]
    [(GetBang x) (GetBang x)]
    [(SetBang x e) (SetBang x (expose-exp e))]
    [(Let x e1 e2) (Let x (expose-exp e1) (expose-exp e2))]
    [(If e1 e2 e3) (If (expose-exp e1) (expose-exp e2) (expose-exp e3))]
    [(Prim op es) (Prim op (map expose-exp es))]
    [(Begin es e) (Begin (map expose-exp es) (expose-exp e))]
    [(WhileLoop e1 e2) (WhileLoop (expose-exp e1) (expose-exp e2))]
    [(Apply func args) (Apply (expose-exp func) (map expose-exp args))]
    [(HasType (Prim 'vector es) t)
     (define exposed-elements (map expose-exp es))
     (define tmp (gensym 'vec))
     (define vars (map (lambda (e) (gensym 'tmpv)) exposed-elements))
     (make-has-type exposed-elements tmp vars vars t)]))

(define (make-has-type exposed-elements vecsym vars vars-pers type)
  (match exposed-elements
    [(cons e es) (Let (car vars) e (make-has-type es vecsym (cdr vars) vars-pers type))]
    ['() (allocate-init vecsym vars-pers type)]))

(define (allocate-init vecsym vars type)
  (define len (length vars))
  (define bytes (Int (* 8 (+ len 1))))
  (Begin (list (If (Prim '<
                         (list (Prim '+ (list (GlobalValue 'free_ptr) bytes))
                               (GlobalValue 'fromspace_end)))
                   (Void)
                   (Collect bytes)))    
        (if (= len 0)
         (Var vecsym)
         (Let vecsym
              (Allocate len type)
              ; (Begin (for/list ([i (in-naturals)] [v vars])
              ;          (Prim 'vector-set! (list (Var vecsym) (Int i) (Var v))))
              ;        (Var vecsym))))))
              (helper vars 0 vecsym)))))

(define (expose-allocation-defs def)
    (match def 
      [(Def label args rtype info body) 
       (Def label args rtype info (expose-exp body))
      ]
    )
)

(define (expose-allocation p)
  (match p
    [(ProgramDefs info defs) (ProgramDefs info (map expose-allocation-defs defs)) ]))


(define (helper vars i vecsym)
  (match vars
  ['() (Var vecsym)]
  [(cons x y) (Let (gensym '_) (Prim 'vector-set! (list (Var vecsym) (Int i) (Var x))) (helper y (+ i 1) vecsym))]))

