#lang racket
(require "../utilities.rkt")
(provide basic-blocks
         explicate-control)

;; explicate-control : R1 -> C0
;; Function to create block
(define basic-blocks '())

(define (create_block tail)
  (match tail
    [(Goto label) (Goto label)]
    [else
     (let ([label (gensym 'block)])
       (set! basic-blocks (cons (cons label tail) basic-blocks))
       (Goto label))]))

(define (explicate_tail e)
  (match e
    [(Var x) (values (Return (Var x)) '())]
    [(Int n) (values (Return (Int n)) '())]
    [(Bool b) (values (Return (Bool b)) '())]
    [(Let x rhs body)
     (let*-values ([(intmd-seq1 intmd-vars1) (explicate_tail body)]
                   [(intmd-seq2 intmd-vars2) (explicate_assign rhs x intmd-seq1)])
       (values intmd-seq2 (append intmd-vars1 intmd-vars2 `(,x))))]
    [(Prim op es) (values (Return (Prim op es)) '())]
    [(If cnd^ thn^ els^)
     (let*-values ([(intmd-seqthn intmd-vars1) (explicate_tail thn^)]
                   [(intmd-seqels intmd-vars2) (explicate_tail els^)]
                   [(intmd-seqcnd intmd-vars3) (explicate_pred cnd^ intmd-seqthn intmd-seqels)])
       (values intmd-seqcnd (remove-duplicates (append intmd-vars1 intmd-vars2 intmd-vars3))))]
    [else (error "explicate_tail unhandled case" e)]))

(define (explicate_assign e x cont)
  (match e
    [(Var y) (values (Seq (Assign (Var x) (Var y)) cont) '())]
    [(Int n) (values (Seq (Assign (Var x) (Int n)) cont) '())]
    [(Bool b) (values (Seq (Assign (Var x) (Bool b)) cont) '())]
    [(Let y rhs body)
     (let*-values ([(intmd-seq1 intmd-vars1) (explicate_assign body x cont)]
                   [(intmd-seq2 intmd-vars2) (explicate_assign rhs y intmd-seq1)])
       (values intmd-seq2 (append intmd-vars1 intmd-vars2 `(,y))))]
    [(If cnd^ thn^ els^)
     (let*-values ([(intmd-seqthn intmd-vars1) (explicate_assign thn^ x cont)]
                   [(intmd-seqels intmd-vars2) (explicate_assign els^ x cont)]
                   [(intmd-seqcnd intmd-vars3) (explicate_pred cnd^ intmd-seqthn intmd-seqels)])
       (values intmd-seqcnd (remove-duplicates (append intmd-vars1 intmd-vars2 intmd-vars3))))]
    [(Prim op es) (values (Seq (Assign (Var x) (Prim op es)) cont) '())]
    [else (error "explicate_assign unhandled case" e)]))

(define (explicate_pred cnd thn els)
  (match cnd
    [(Var x)
     (values (IfStmt (Prim 'eq? (list (Var x) (Bool #t))) (create_block thn) (create_block els)) '())]
    [(Let x rhs body)
     (let*-values ([(intmd-seq1 intmd-vars1)
                    (explicate_pred body (create_block thn) (create_block els))]
                   [(intmd-seq2 intmd-vars2) (explicate_assign rhs x intmd-seq1)])
       (values intmd-seq2 (remove-duplicates (append intmd-vars1 intmd-vars2 `(,x)))))]
    [(Prim 'not (list e))
     (values (IfStmt (Prim 'eq? (list e (Bool #t))) (create_block els) (create_block thn)) '())]
    [(Prim op es)
     #:when (or (eq? op 'eq?) (eq? op '<))
     (values (IfStmt (Prim op es) (create_block thn) (create_block els)) '())]
    [(Bool b)
     (values (IfStmt (Prim 'eq? (list cnd (Bool #t))) (create_block thn) (create_block els)) '())]
    [(If cnd^ thn^ els^)
     (define thn-block (create_block thn))
     (define els-block (create_block els))
     (let*-values ([(intmd-seqthn intmd-vars1) (explicate_pred thn^ thn-block els-block)]
                   [(intmd-seqels intmd-vars2) (explicate_pred els^ thn-block els-block)]
                   [(intmd-seqcnd intmd-vars3) (explicate_pred cnd^ intmd-seqthn intmd-seqels)])
       (values intmd-seqcnd (remove-duplicates (append intmd-vars1 intmd-vars2 intmd-vars3))))]
    [else (error "explicate_pred unhandled case" cnd)]))

(define (explicate-control p)
  (set! basic-blocks '())
  (match p
    [(Program info body)
     (let-values ([(intmd-seq intmd-vars) (explicate_tail body)])
       ; (CProgram intmd-vars `((start . ,intmd-seq))))]))
       ;(CProgram (dict-set #hash() 'locals intmd-vars) `((start . ,intmd-seq))))]))
       (CProgram (dict-set #hash() 'locals intmd-vars)
                 (cons (cons 'start intmd-seq) basic-blocks)))]))
