#lang racket
(require racket/set
         racket/stream)
(require racket/fixnum)
(require graph)
(require "graph-printing.rkt")
(require "interp-Lwhile.rkt")
(require "interp-Cwhile.rkt")
(require "interp.rkt")
(require "priority_queue.rkt")
(require "utilities.rkt")
(require "type-check-Lwhile.rkt")
(require "type-check-Cwhile.rkt")
(require graph)
(require "graph-printing.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lint examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy
(define (flip-exp e)
  (match e
    [(Var x) e]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (Prim '- (list (flip-exp e1)))]
    [(Prim '+ (list e1 e2)) (Prim '+ (list (flip-exp e2) (flip-exp e1)))]))

(define (flip-Lint e)
  (match e
    [(Program info e) (Program info (flip-exp e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; explicate-control : R1 -> C0

;; Functoin to create block
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
(define (int-to-imm e)
  (match e
    [(Int a) (Imm a)]
    [(Var a) (Var a)]
    [(Bool #t) (Imm 1)]
    [(Bool #f) (Imm 0)]))

(define (instruction-to-x86 e)
  (match e
    [(Assign var (Prim '+ (list a b)))
     (list (Instr 'movq (list (int-to-imm a) var)) (Instr 'addq (list (int-to-imm b) var)))]
    [(Assign var (Prim '- (list a)))
     (list (Instr 'movq (list (int-to-imm a) var)) (Instr 'negq (list var)))]
    [(Assign var (Prim 'read '()))
     (cons (Callq 'read_int 0) ; 0 is a number of arguments
           (if (equal? var (Reg 'rax)) '() (list (Instr 'movq (list (Reg 'rax) var)))))]
    [(Assign var (Prim 'not (list a)))
     (append (if (equal? var a) `() (list (Instr 'movq (list (int-to-imm a) var))))
             (list (Instr 'xorq (list (Imm 1) var))))]
    [(Assign var (Prim 'eq? (list e1 e2)))
     (list (Instr 'cmpq (list (int-to-imm e2) (int-to-imm e1)))
           (Instr 'set (list 'e (ByteReg 'al)))
           (Instr 'movzbq (list (ByteReg 'al) var)))]
    [(Assign var (Prim '< (list e1 e2)))
     (list (Instr 'cmpq (list (int-to-imm e2) (int-to-imm e1)))
           (Instr 'set (list 'l (ByteReg 'al)))
           (Instr 'movzbq (list (ByteReg 'al) var)))]
    [(Assign var var2) (if (equal? var var2) '() (list (Instr 'movq (list (int-to-imm var2) var))))]))

(define (resolve-select-instructions e)
  (match e
    [(Seq instr rest-seq) (append (instruction-to-x86 instr) (resolve-select-instructions rest-seq))]
    [(IfStmt (Prim 'eq? (list e1 e2)) (Goto l1) (Goto l2))
     (list (Instr 'cmpq (list (int-to-imm e2) (int-to-imm e1))) (JmpIf 'e l1) (Jmp l2))]
    [(IfStmt (Prim '< (list e1 e2)) (Goto l1) (Goto l2))
     (list (Instr 'cmpq (list (int-to-imm e2) (int-to-imm e1))) (JmpIf 'l l1) (Jmp l2))]
    [(Goto label) (list (Jmp label))]
    [(Return e)
     (define instr (Assign (Reg 'rax) e))
     (append (instruction-to-x86 instr) (list (Jmp 'conclusion)))]))

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (match p
    [(CProgram info body)
     (X86Program info
                 (for/list ([frame body])
                   (cons (car frame) (Block '() (resolve-select-instructions (cdr frame))))))]))

; (list
;   (cons 'start (IfStmt (Prim '< (list (Int 5) (Int 10))) (Goto 'block17869) (Goto 'block17868)))
;   (cons 'block17869 (Seq (Assign (Var 'g17863) (Int 10)) (Seq (Assign (Var 'g17864) (Prim 'read '())) (IfStmt (Prim '< (list (Var 'g17864) (Var 'g17863))) (Goto 'block17867) (Goto 'block17868)))))
;   (cons 'block17868 (Seq (Assign (Var 'g17862) (Int 5)) (IfStmt (Prim 'eq? (list (Var 'g17862) (Int 5))) (Goto 'block17865) (Goto 'block17866))))
;   (cons 'block17867 (Seq (Assign (Var 'g17862) (Int 10)) (IfStmt (Prim 'eq? (list (Var 'g17862) (Int 5))) (Goto 'block17865) (Goto 'block17866))))
;   (cons 'block17866 (Return (Int 5)))
;   (cons 'block17865 (Return (Int 10))))
;; assign-homes : pseudo-x86 -> pseudo-x86

(define (create-env lst offset)
  (match lst
    ['() #hash()]
    [(cons x y) (dict-set (create-env y (- offset 8)) x (Deref 'rbp offset))]))

(define (arg->memory arg env)
  (match arg
    [(Var x) (dict-ref env x)]
    [else arg]))

(define (map-instr env instr)
  (match instr
    [(Instr op lst)
     (Instr op
            (for/list ([e lst])
              (arg->memory e env)))]

    [else instr]))

(define (map-instrs env lst)
  (match lst
    ['() '()]
    [(cons x y) (cons (map-instr env x) (map-instrs env y))]))

; local variables are now moved to Program info  and removed from Block Info
(define (assign-homes p)
  (match p
    [(X86Program info body)
     (X86Program info
                 (match body
                   [`((,label . ,block))
                    (match block
                      [(Block info instrs)
                       `((,label . ,(Block info
                                           (map-instrs (create-env (dict-ref info 'locals) -8)
                                                       instrs))))])]))]))

; (X86Program '#hash()
;   (list
;     (cons 'start
;       (Block '#hash((locals . (g10901))) (list (Instr 'movq (list (Imm 10) (Var 'g10901))) (Instr 'movq (list (Var 'g10901) (Reg 'rax))) (Jmp 'conclusion))))))

(define (valid-set x)
  (match x
    [(Imm t) (set)]
    [(ByteReg t) (set (byte-reg->full-reg t))]
    [else (set x)]))

(define (get-read-write-sets instr)
  (match instr
    [(Instr 'movq (list A B))
     (define read-set (valid-set A))
     (define write-set (valid-set B))
     (values read-set write-set)]
    [(Instr 'movzbq (list A B))
     (define read-set (valid-set A))
     (define write-set (valid-set B))
     (values read-set write-set)]
    [(Instr 'addq (list A B))
     (define read-set (set-union (valid-set A) (valid-set B)))
     (define write-set (valid-set B))
     (values read-set write-set)]
    [(Instr 'subq (list A B))
     (define read-set (set-union (valid-set A) (valid-set B)))
     (define write-set (valid-set B))
     (values read-set write-set)]
    [(Instr 'negq (list A))
     (define read-set (valid-set A))
     (define write-set (valid-set A))
     (values read-set write-set)]
    [(Jmp x) (values (dict-ref labels->live x) (set))]
    [(JmpIf c x) (values (dict-ref labels->live x) (set))]
    [(Instr 'xorq (list A B))
     (define read-set (set-union (valid-set A) (valid-set B)))
     (define write-set (valid-set B))
     (values read-set write-set)]
    [(Instr 'cmpq (list A B))
     (define read-set (set-union (valid-set A) (valid-set B)))
     (define write-set (set))
     (values read-set write-set)]
    ;;; (values (set) caller-save)
    [(Callq label n)
     (values (list->set (take callee-save n))
             (list->set (map (lambda (r) (Reg r)) (set->list caller-save))))]
    [(Instr 'set (list A B))
     (define read-set (valid-set B))
     (define write-set (valid-set B))
     (values read-set write-set)]

    [else (error "read-write-sets: Unhandled case")]))

(define (get-live-vars lst)
  (match lst
    ['() (list (set))]
    [(cons x y)
     (define live-vars (get-live-vars y))
     (define last-set (car live-vars))
     (define-values (read-set write-set) (get-read-write-sets x))

     (define new-set (set-union (set-subtract last-set write-set) read-set))

     ;;; (define new-live-vars (
     ;;;   if (null? y)
     ;;;   (cons new-set '())
     ;;;   (cons new-set live-vars)
     ;;;   ))

     (cons new-set live-vars)]))

(define (uncover-live-block p)
  ;;; (display "inside uncover-block\n")
  ;;; (display "print p\n")
  ;;; (print p)
  ;;; (display "\n")
  ;;; (print (list? p))
  ;;; (display "end\n")
  (match p
    [(cons label (Block info instrlist))
     (define live-vars (get-live-vars instrlist))
     (set! labels->live (dict-set labels->live label (car live-vars)))
     (define new-info (dict-set info 'live-vars (cdr live-vars)))

     (cons label (Block new-info instrlist))]))

(define labels->live '())

(define (uncover-live p)
  (set! labels->live (hash 'conclusion (set (Reg 'rax) (Reg 'rsp))))
  (match p
    [(X86Program info body)
     (X86Program info
                 (for/list ([label (tsort (transpose (dict-ref info 'cfg)))])
                   (uncover-live-block (cons label (dict-ref body label)))))]))

;; build-interference-graph
;; write a code to build interference graph
;; Store the graph in info field of the program under the key 'conflicts'
;;; (define (build-interference-graph p)
;;;   (define interference-graph (undirected-graph `()))
;;;   (match p
;;;     [(X86Program pinfo (list (cons 'start block)))
;;;       (define local-vars (dict-ref pinfo 'locals))
;;;       ;;; (for/list ([item local-vars]) (print item)
;;;       ;;;   (add-vertex! interference-graph (Var item)))
;;;       (match block
;;;         [(Block binfo instrs)
;;;           (define live-vars (dict-ref binfo 'live-vars))
;;;           (add-interfering-edges instrs live-vars interference-graph)
;;;           ])
;;;       (define new-pinfo (dict-set pinfo 'conflicts interference-graph))
;;;       (X86Program new-pinfo (list (cons 'start block)))]))

(define (build-interference-graph p)
  (define interference-graph (undirected-graph `()))
  (match p
    [(X86Program pinfo body)
     (define local-vars (dict-ref pinfo 'locals))
     ;  (display "\n====\nbody: \n")
     ;  (print body)
     ;  (display "\n")
     (for/list ([block body])
       (match block
         [(cons label (Block binfo instrs))
          (define live-vars (dict-ref binfo 'live-vars))
          (add-interfering-edges instrs live-vars interference-graph)]))
     (print-dot interference-graph "interference-graph")

     (define new-pinfo (dict-set pinfo 'conflicts interference-graph))
     (X86Program new-pinfo body)]))

(define (add-interfering-edges instrs live-vars interference-graph)
  ; (display "\n====\ninstrs: \n")
  ; (print instrs)
  ; (display "\n")
  (for ([inst instrs] [live-var live-vars])
    (match inst
      [(or (Instr 'movq (list s d)) (Instr 'movzbq (list s d)))
       (for/list ([v live-var])
         (when (and (and (not (equal? v s)) (not (equal? v d)))
                    (not (has-edge? interference-graph v d)))
           ; (display "\n----\nAdding Edge: ")
           ; (display v)
           ; (display " -> ")
           ; (display d)
           (add-edge! interference-graph v d)))]
      [_
       (define-values (_ write-vars) (get-read-write-sets inst))
       ;  Print inst and write-vars
       ; (display "\n======\ninst: ")
       ; (print inst)
       ; (display "\nwrite-vars: ")
       ; (print write-vars)
       ; (display "\n======\n")
       (for/list ([d write-vars])
         (for/list ([v live-var])
           (when (not (or (equal? v d) (has-edge? interference-graph v d)))
             ; (display "\n----\nAdding Edge: ")
             ; (display v)
             ; (display " -> ")
             ; (display d)
             (add-edge! interference-graph v d))))])))

(define num-to-reg
  (dict-set* #hash()
             -2
             'rsp
             -1
             'rax
             0
             'r8
             1
             'r9
             2
             'r10
             3
             'r11
             4
             'r12
             5
             'r13
             6
             'r14
             7
             'rbx
             8
             'rcx
             9
             'rdx
             10
             'rsi
             11
             'rdi))

(define reg-to-num
  (dict-set* #hash()
             'rsp
             -2
             'rax
             -1
             'r8
             0
             'r9
             1
             'r10
             2
             'r11
             3
             'r12
             4
             'r13
             5
             'r14
             6
             'rbx
             7
             'rcx
             8
             'rdx
             9
             'rsi
             10
             'rdi
             11))

(define (map-registers color-map)
  (define spill-count 0)
  (define used-callee (set))
  (dict-for-each color-map
                 (lambda (k v)
                   (when (< v 12)
                     (set! used-callee (set-add used-callee (dict-ref num-to-reg v))))))
  (set! used-callee (set-intersect callee-save used-callee))
  (dict-for-each
   color-map
   (lambda (k v)
     (match (< v 12)
       [#t
        (dict-set! color-map k (Reg (dict-ref num-to-reg v)))
        ; (set! used-callee (set-add used-callee (dict-ref num-to-reg v)))
        ]
       [#f
        (dict-set! color-map k (Deref 'rbp (- (* (- 8) (- v 11)) (* 8 (set-count used-callee)))))
        (set! spill-count (+ spill-count 1))])))
  (values color-map spill-count (set-intersect callee-save used-callee)))

(define (allocate-registers p)
  (match p
    [(X86Program info body)
     (define interference-graph (dict-ref info 'conflicts))
     (define color-map (color-graph interference-graph))
     (define-values (color-reg spill-count used-callee) (map-registers color-map))
     (X86Program (dict-set (dict-set info 'spill-count spill-count) 'used-callee used-callee)
                 (for/list ([block body])
                   (match block
                     [(cons label (Block binfo instrs))
                      (cons label (Block binfo (map-instrs color-reg instrs)))])))]))

; Helper function for graph coloring
(define (color-graph interference-graph)
  (define all-vars (for/list ([node (filter Var? (get-vertices interference-graph))]) (match node [(Var x) x])))
  (define regs-in-graph (filter Reg? (get-vertices interference-graph)))
  ; (print "-----")
  ; (print regs-in-graph)
  ; initialising already assigned colors for each var
  (define already_assigned_colors (make-hash))
  (for ([var all-vars])
    (dict-set! already_assigned_colors var '()))

  (for ([reg regs-in-graph])
    (for ([node (in-neighbors interference-graph reg)])
      (match node
        [(Var child_var)
         (dict-set! already_assigned_colors
                    child_var
                    (set-add (dict-ref already_assigned_colors child_var)
                             (dict-ref reg-to-num
                                       (match reg
                                         [(Reg r) r]))))]
        [_ #f])))
  ; inserting in priority queue
  (define pq
    (make-pqueue (lambda (a b)
                   (> (length (dict-ref already_assigned_colors a))
                      (length (dict-ref already_assigned_colors b))))))

  (define node_references (make-hash))
  (for/list ([var all-vars])
    (define node_ref (pqueue-push! pq var))
    (dict-set! node_references var node_ref))

  (define result (make-hash))
  ; traverse priority queue
  (for ([i (pqueue-count pq)])
    (let ([var (pqueue-pop! pq)])
      (define cols (dict-ref already_assigned_colors var))

      (define assigned-color (get-min-color cols 0))

      (dict-set! result var assigned-color)
      (for ([node (in-neighbors interference-graph (Var var))])
        (match node
          [(Var child_var) ; doing only for (Var something) struct
           (dict-set! already_assigned_colors
                      child_var
                      (set-add (dict-ref already_assigned_colors child_var) assigned-color))
           (pqueue-decrease-key! pq (dict-ref node_references child_var))]
          [_ #f]))))

  ; (dict-set! result 'rax -1)
  ; (dict-set! result 'rsp -2)

  ; (print node_references)

  result
  ; (for/list ([e all-vars]) ())
  )

(define (get-min-color color-set n)
  (if (equal? (member n color-set) #f) n (get-min-color color-set (+ n 1))))

; ; function to get the next available color for a vertex
; (define (next-available-color unavail-colors num)
;   ; if num in unavail-colors, return next available color num+1
;   (if (dict-has-key unavail-colors num)
;     (next-available-color unavail-colors (+ num 1))
;     num))

(define (patch-instr instr)
  (match instr
    [(Instr 'movq args)
     (match args
       [(list a a) '()]
       [(list (Deref r1 o1) (Deref r2 o2))
        (list (Instr 'movq (list (Deref r1 o1) (Reg 'rax)))
              (Instr 'movq (list (Reg 'rax) (Deref r2 o2))))]
       [else (list instr)])]
    [(Instr 'cmpq args)
     (match args
       [(list (Deref r1 o1) (Deref r2 o2))
        (list (Instr 'movq (list (Deref r1 o1) (Reg 'rax)))
              (Instr 'cmpq (list (Reg 'rax) (Deref r2 o2))))]
       [(list arg1 (Imm v))
        (list (Instr 'movq (list (Imm v) (Reg 'rax))) (Instr 'cmpq (list arg1 (Reg 'rax))))]
       [else (list instr)])]
    [(Instr 'movzbq (list arg1 (Imm v)))
     (list (Instr 'movq (list (Imm v) (Reg 'rax))) (Instr 'movzbq (list arg1 (Reg 'rax))))]

    [(Instr op (list (Deref r1 o1) (Deref r2 o2)))
     (list (Instr 'movq (list (Deref r1 o1) (Reg 'rax))) (Instr op (list (Reg 'rax) (Deref r2 o2))))]
    [else (list instr)]))

(define (patch-instrs lst-instrs)
  (foldr append '() (map patch-instr lst-instrs)))

;; patch-instructions : psuedo-x86 -> x86
;;; (define (patch-instructions p)
;;;   (match p
;;;     [(X86Program info body)
;;;      (X86Program info
;;;                  (match body
;;;                    [`((,label . ,block))
;;;                     (match block
;;;                       [(Block info instrs) `((,label . ,(Block info (patch-instrs instrs))))])]))]))

(define (patch-instructions p)
  (match p
    [(X86Program info body)
     (X86Program info
                 (for/list ([block body])
                   (match block
                     [(cons label (Block binfo instrs))
                      (cons label (Block binfo (patch-instrs instrs)))])))]))

(define (generate-main body offset used-callee)
  (dict-set
   body
   'main
   (Block '()
          (append (list (Instr 'pushq (list (Reg 'rbp))) (Instr 'movq (list (Reg 'rsp) (Reg 'rbp))))
                  (for/list ([reg used-callee])
                    (Instr 'pushq (list (Reg reg))))
                  (list (Instr 'subq (list (Imm offset) (Reg 'rsp))) (Jmp 'start))))))

(define (generate-conclusion body offset rev-used-callee)
  (dict-set body
            'conclusion
            (Block '()
                   (append (list (Instr 'addq (list (Imm offset) (Reg 'rsp))))
                           (for/list ([reg rev-used-callee])
                             (Instr 'popq (list (Reg reg))))
                           (list (Instr 'popq (list (Reg 'rbp))) (Retq))))))

(define (mult-16 n)
  (if (= (modulo n 16) 0) n (mult-16 (add1 n))))

;; prelude-and-conclusion : x86 -> x86
(define (prelude-and-conclusion p)
  (match p
    [(X86Program info body)

     (define used-callee (set->list (dict-ref info 'used-callee)))
     (define offset
       (- (mult-16 (+ (* 8 (dict-ref info 'spill-count)) (* 8 (length used-callee))))
          (* 8 (length used-callee))))

     (define body-with-main (generate-main body offset used-callee))
     (define body-complete (generate-conclusion body-with-main offset (reverse used-callee)))
     (X86Program info body-complete)]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; shrink replaces and and or with if
(define (shrink-exp exp)
  (match exp
    ['() '()]
    [(Prim 'and (list e1 e2)) (If (shrink-exp e1) (shrink-exp e2) (Bool #f))]
    [(Prim 'or (list e1 e2)) (If (shrink-exp e1) (Bool #t) (shrink-exp e2))]
    [(Int n) (Int n)]
    [(Var x) (Var x)]
    [(Bool t) (Bool t)]
    [(Let x e1 e2) (Let x (shrink-exp e1) (shrink-exp e2))]
    [(If e1 e2 e3) (If (shrink-exp e1) (shrink-exp e2) (shrink-exp e3))]
    [(Begin es exp) 
      (Begin (map shrink-exp es) (shrink-exp exp))]
    [(SetBang x e) (SetBang x (shrink-exp e))]
    [(WhileLoop e1 e2) (WhileLoop (shrink-exp e1) (shrink-exp e2))]
    [(Prim '- (list e1 e2)) (Prim '+ (list e1 (Prim '- (list e2))))]
    [(Prim '> (list e1 e2))
     (define v (gensym))
     (Let v (shrink-exp e1) (Prim '< (list (shrink-exp e2) (Var v))))]
    [(Prim '<= (list e1 e2))
     (define v (gensym))
     (Let v (shrink-exp e1) (Prim 'not (list (Prim '< (list (shrink-exp e2) (Var v))))))]
    [(Prim '>= (list e1 e2)) (Prim 'not (list (Prim '< (list (shrink-exp e1) (shrink-exp e2)))))]
    [(Prim op es)
     (Prim op
           (for/list ([e es])
             (shrink-exp e)))]))

(define (shrink p)
  (match p
    [(Program info e) (Program info (shrink-exp e))]))

(define (build-cfg-instrs label instrs g bs)
  (match instrs
    ['() g]
    [(cons x y)
     (match x
       [(JmpIf c label-target)
        (add-directed-edge! g label label-target)
        (build-cfg-instrs label y (build-cfg-block label-target g bs) bs)]
       [(Jmp label-target)
        (match label-target
          ['conclusion g]
          [else
           (add-directed-edge! g label label-target)
           (build-cfg-instrs label y (build-cfg-block label-target g bs) bs)])]
       [else (build-cfg-instrs label y g bs)])]))

(define (build-cfg-block label g bs)
  (match (dict-ref bs label)
    [(Block info instrs) (build-cfg-instrs label instrs g bs)]))

(define (build-cfg p)
  (match p
    [(X86Program info body)
     (define g (unweighted-graph/directed '()))
     (add-vertex! g 'start)
     (X86Program (dict-set info 'cfg (build-cfg-block 'start g body)) body)]))

(define (print-cfg p)
  (match p
    [(X86Program info body)
     (print-graph (dict-ref info 'cfg))
     p]))

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(("shrink" ,shrink ,interp-Lwhile ,type-check-Lwhile)
    ("uniquify" ,uniquify ,interp-Lwhile ,type-check-Lwhile)
    ; ;;; ("patial evaluator Lvar" ,pe_Lif ,interp-Lif ,type-check-Lif)
    ; ;; Uncomment the following passes as you finish them.
    ; ("remove complex opera*" ,remove-complex-opera* ,interp-Lif ,type-check-Lif)
    ; ("explicate control" ,explicate-control ,interp-Cif ,type-check-Cif)
    ; ("instruction selection" ,select-instructions ,interp-pseudo-x86-1)
    ; ("build cfg" ,build-cfg ,interp-pseudo-x86-1)
    ; ; ("print cfg" ,print-cfg ,interp-pseudo-x86-1)
    ; ("uncover live" ,uncover-live ,interp-pseudo-x86-1)
    ; ("build interference graph" ,build-interference-graph ,interp-pseudo-x86-1)
    ; ("allocate registers" ,allocate-registers ,interp-pseudo-x86-1)
    ; ("patch instructions" ,patch-instructions ,interp-x86-1)
    ; ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-1)
    ))
