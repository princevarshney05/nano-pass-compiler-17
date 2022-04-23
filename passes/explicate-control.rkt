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
    [(Void) (values (Return (Void)) '())]
    [(Var x) (values (Return (Var x)) '())]
    [(Int n) (values (Return (Int n)) '())]
    [(Bool b) (values (Return (Bool b)) '())]
    [(FunRef a b) (values (Return (FunRef a b)) '())]
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
    [(Begin (list expr) expr_n)
     (let*-values ([(intmd-seq1 intmd-vars1) (explicate_tail expr_n)]
                   [(intmd-seq2 intmd-vars2) (explicate_effect expr intmd-seq1)])
       (values intmd-seq2 (remove-duplicates (append intmd-vars1 intmd-vars2))))]
    [(Begin (list expr rest-expr ...) expr_n)
     (let*-values ([(intmd-seq1 intmd-vars1) (explicate_tail (Begin rest-expr expr_n))]
                   [(intmd-seq2 intmd-vars2) (explicate_effect expr intmd-seq1)])
       (values intmd-seq2 (remove-duplicates (append intmd-vars1 intmd-vars2))))
     ;  (explicate_effect expr intmd-seq) ; todo : supply for vars in explicate_effect
     ]
    [(WhileLoop cnd^ expr)
     (define label-loop 'loop)
     (let*-values ([(intmd-seqexpr intmd-vars1) (explicate_effect expr (Goto label-loop))]
                   [(intmd-seqcnd^ intmd-vars2) (explicate_pred cnd^ intmd-seqexpr (Void))])
       (set! basic-blocks (cons (cons label-loop intmd-seqcnd^) basic-blocks))
       (define net-vars (append intmd-vars1 intmd-vars2))
       (values (Goto label-loop) net-vars))]
    [(Apply func args) (values (TailCall func args) '())]
    [else (error "explicate_tail unhandled case" e)]))

(define (explicate_assign e x cont)
  (match e
    [(Void) (values (Seq (Assign (Var x) (Void)) cont) '())]
    [(Var y) (values (Seq (Assign (Var x) (Var y)) cont) '())]
    [(Int n) (values (Seq (Assign (Var x) (Int n)) cont) '())]
    [(Bool b) (values (Seq (Assign (Var x) (Bool b)) cont) '())]
    [(FunRef a b) (values (Seq (Assign (Var x) (FunRef a b)) cont) '())]
    [(Allocate e1 e2) (values (Seq (Assign (Var x) (Allocate e1 e2)) cont) '())]
    [(Collect e1) (values (Seq (Assign (Var x) (Collect e1)) cont) '())]
    [(GlobalValue e1) (values (Seq (Assign (Var x) (GlobalValue e1)) cont) '())]
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
    [(SetBang var rhs)
     (let*-values ([(intmd-seq1 intmd-vars1) (explicate_assign (Void) x cont)]
                   [(intmd-seq2 intmd-vars2) (explicate_assign rhs var intmd-seq1)])
       (values intmd-seq2 (remove-duplicates (append intmd-vars1 intmd-vars2))))
     ; (explicate_assign rhs var intmd-seq) ; todo : supply for vars in explicate_effect
     ]
    ; [(Begin (list expr) expr_n)
    ;  (let*-values ([(intmd-seq intmd-vars) (explicate_assign expr_n x cont)]))
    ;  (explicate_effect expr intmd-seq) ; todo : supply for vars in explicate_effect
    ; ]

    ; [(Begin (list expr rest-expr ...) expr_n)
    ;  (let*-values ([(intmd-seq intmd-vars) (explicate_assign (Begin rest-expr expr_n))]))
    ;  (explicate_effect expr intmd-seq) ; todo : supply for vars in explicate_effect
    ; ]

    [(Begin (list expr) expr_n)
     (let*-values ([(intmd-seq1 intmd-vars1) (explicate_assign expr_n x cont)]
                   [(intmd-seq2 intmd-vars2) (explicate_effect expr intmd-seq1)])
       (values intmd-seq2 (remove-duplicates (append intmd-vars1 intmd-vars2))))]
    [(Begin (list expr rest-expr ...) expr_n)
     (let*-values ([(intmd-seq1 intmd-vars1) (explicate_assign (Begin rest-expr expr_n) x cont)]
                   [(intmd-seq2 intmd-vars2) (explicate_effect expr intmd-seq1)])
       (values intmd-seq2 (remove-duplicates (append intmd-vars1 intmd-vars2))))
     ;  (explicate_effect expr intmd-seq) ; todo : supply for vars in explicate_effect
     ]

    [(WhileLoop cnd body)
     (define label-loop (gensym 'loop))

     (let*-values ([(intmd-seqcnt intmd-vars1) (explicate_assign (Void) x cont)]
                   [(intmd-seqbdy intmd-vars2) (explicate_effect body (Goto label-loop))]
                   [(intmd-seqcnd intmd-vars3) (explicate_pred cnd intmd-seqbdy intmd-seqcnt)])
       (set! basic-blocks (cons (cons label-loop intmd-seqcnd) basic-blocks))
       (values
        (Goto label-loop)
        (remove-duplicates
         (append intmd-vars1 intmd-vars2 intmd-vars3))))] ; todo : supply for vars in explicate_effect

    [(Apply func args) (values (Seq (Assign (Var x) (Call func args)) cont) '())]

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
     #:when (or (eq? op 'eq?) (eq? op '<) (eq? op 'vector-ref) (eq? op 'vector-set!))
     (values (IfStmt (Prim op es) (create_block thn) (create_block els)) '())]
    [(Bool b)
     (values (IfStmt (Prim 'eq? (list cnd (Bool #t))) (create_block thn) (create_block els)) '())]
    [(FunRef a b)
     (values (IfStmt (Prim 'eq? (list cnd (Bool #t))) (create_block thn) (create_block els)) '())]
    [(If cnd^ thn^ els^)
     (define thn-block (create_block thn))
     (define els-block (create_block els))
     (let*-values ([(intmd-seqthn intmd-vars1) (explicate_pred thn^ thn-block els-block)]
                   [(intmd-seqels intmd-vars2) (explicate_pred els^ thn-block els-block)]
                   [(intmd-seqcnd intmd-vars3) (explicate_pred cnd^ intmd-seqthn intmd-seqels)])
       (values intmd-seqcnd (remove-duplicates (append intmd-vars1 intmd-vars2 intmd-vars3))))]

    [(Begin (list expr) expr_n)
     (let*-values ([(intmd-seq1 intmd-vars1) (explicate_pred expr_n thn els)]
                   [(intmd-seq2 intmd-vars2) (explicate_effect expr intmd-seq1)])
       (values intmd-seq2 (remove-duplicates (append intmd-vars1 intmd-vars2))))]

    [(Begin (list expr rest-expr ...) expr_n)
     (let*-values ([(intmd-seq1 intmd-vars1) (explicate_pred (Begin rest-expr expr_n) thn els)]
                   [(intmd-seq2 intmd-vars2) (explicate_effect expr intmd-seq1)])
       (values intmd-seq2 (remove-duplicates (append intmd-vars1 intmd-vars2))))]

    [(Apply func args)
     (define tmp (gensym 'tmp-var))
     (explicate_pred (Let tmp cnd (Var tmp)) thn els)]

    [else (error "explicate_pred unhandled case" cnd)]))

(define (explicate_effect e cont)
  (match e
    [(Void) (values cont '())]
    [(Bool x) (values cont '())]
    [(Var x) (values cont '())]
    [(Int x) (values cont '())]
    [(FunRef a b) (values cont '())]
    [(GlobalValue e1) (values cont '())]
    [(Allocate e1 e2) (values (Seq (Allocate e1 e2) cont) '())]
    [(Collect e1) (values (Seq (Collect e1) cont) '())]
    [(Prim 'vector-set! e1) (values (Seq e cont) '())]
    [(Prim 'read '()) (values (Seq e cont) '())]
    [(Prim op es) (values cont '())]
    [(Let x rhs body)
     (let*-values ([(intmd-seq1 intmd-vars1) (explicate_effect body cont)]
                   [(intmd-seq2 intmd-vars2) (explicate_assign rhs x intmd-seq1)])
       (values intmd-seq2 (remove-duplicates (append intmd-vars1 intmd-vars2 `(,x)))))]
    [(If cnd thn els)
     (let*-values ([(intmd-seq1 intmd-vars1) (explicate_effect thn cont)]
                   [(intmd-seq2 intmd-vars2) (explicate_effect els cont)]
                   [(intmd-seq3 intmd-vars3) (explicate_pred cnd intmd-seq1 intmd-seq2)])
       (values intmd-seq3 (remove-duplicates (append intmd-vars1 intmd-vars2 intmd-vars3))))]
    [(SetBang x es) (explicate_assign es x cont)]

    [(Begin (list expr) expr_n)
     (let*-values ([(intmd-seq1 intmd-vars1) (explicate_effect expr_n cont)]
                   [(intmd-seq2 intmd-vars2) (explicate_effect expr intmd-seq1)])
       (values intmd-seq2 (remove-duplicates (append intmd-vars1 intmd-vars2))))]

    [(Begin (list expr rest-expr ...) expr_n)
     (let*-values ([(intmd-seq1 intmd-vars1) (explicate_effect (Begin rest-expr expr_n) cont)]
                   [(intmd-seq2 intmd-vars2) (explicate_effect expr intmd-seq1)])
       (values intmd-seq2 (remove-duplicates (append intmd-vars1 intmd-vars2))))]
    [(WhileLoop cnd body)
     (define label-loop (gensym 'loop))

     (let*-values ([(intmd-seqbdy intmd-vars1) (explicate_effect body (Goto label-loop))]
                   [(intmd-seqcnd intmd-vars2) (explicate_pred cnd intmd-seqbdy cont)] ;;todo cont
                   )
       (set! basic-blocks (cons (cons label-loop intmd-seqcnd) basic-blocks))
       (values (Goto label-loop) (remove-duplicates (append intmd-vars1 intmd-vars2))))]
    [(Apply func args) (values (Seq (Call func args) cont) '())]
    [else (error "explicate_effect unhandled case" e)]))

; (define (explicate-control p)
;   (set! basic-blocks '())
;   (match p
;     [(Program info body)
;      (let-values ([(intmd-seq intmd-vars) (explicate_tail body)])
;     ;  (display "\n\n")
;     ;  (print p)
;     ;  (display "\n----\n")
;     ;  (print intmd-seq)
;     ;  (display "\n----\n")
;     ;  (print intmd-vars)
;     ;  (display "\n----\n")
;     ;  (print basic-blocks)
;     ;  (display "\n\n")
;        ; (CProgram intmd-vars `((start . ,intmd-seq))))]))
;        ;(CProgram (dict-set #hash() 'locals intmd-vars) `((start . ,intmd-seq))))]))
;        (CProgram (dict-set #hash() 'locals intmd-vars)
;                  (cons (cons 'start intmd-seq) basic-blocks)))]))

(define (exp-ctrl-def def)
  (match def
    [(Def name param rty info body)
     (set! basic-blocks '())
     (let-values ([(intmd-seq intmd-vars) (explicate_tail body)])
       (Def name
            param
            rty
            (dict-set info 'locals intmd-vars)
            (cons (cons (symbol-append name 'start) intmd-seq) basic-blocks)))]))

(define (explicate-control p)
  (match p
    [(ProgramDefs info defs) (ProgramDefs info (map exp-ctrl-def defs))]))
