#lang racket
(require racket/set
         racket/stream)
(require racket/fixnum)
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "interp-Lif.rkt")
(require "interp-Cif.rkt")
(require "interp.rkt")
(require "utilities.rkt")
(require "type-check-Lif.rkt")
(require "type-check-Cif.rkt")
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
      [(Let x e body)
       (let ([newenv (dict-set env x (gensym))])
         (Let (dict-ref newenv x) ((uniquify-exp env) e) ((uniquify-exp newenv) body)))]
      [(Prim op es)
       (Prim op
             (for/list ([e es])
               ((uniquify-exp env) e)))]
      [(If e1 e2 e3) (If ((uniquify-exp env) e1) ((uniquify-exp env) e2) ((uniquify-exp env) e3))])))

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
  (let ([thn-block (create_block thn)] [els-block (create_block els)])
    (match cnd
      [(Var x) (values (IfStmt (Prim 'eq? (list (Var x) (Bool #t))) thn-block els-block) '())]
      [(Let x rhs body)
       (let*-values ([(intmd-seq1 intmd-vars1) (explicate_pred body thn-block els-block)]
                     [(intmd-seq2 intmd-vars2) (explicate_assign rhs x intmd-seq1)])
         (values intmd-seq2 (remove-duplicates (append intmd-vars1 intmd-vars2 `(,x)))))]
      [(Prim 'not (list e)) (values (IfStmt (Prim 'eq? (list e (Bool #t))) els-block thn-block) '())]
      [(Prim op es)
       #:when (or (eq? op 'eq?) (eq? op '<))
       (values (IfStmt (Prim op es) thn-block els-block) '())]
      [(Bool b) (values (if b thn els) '())]
      [(If cnd^ thn^ els^)
       (let*-values ([(intmd-seqthn intmd-vars1) (explicate_pred thn^ thn-block els-block)]
                     [(intmd-seqels intmd-vars2) (explicate_pred els^ thn-block els-block)]
                     [(intmd-seqcnd intmd-vars3) (explicate_pred cnd^ intmd-seqthn intmd-seqels)])
         (values intmd-seqcnd (remove-duplicates (append intmd-vars1 intmd-vars2 intmd-vars3))))]
      [else (error "explicate_pred unhandled case" cnd)])))

(define (explicate-control p)
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
           (Instr 'set (list 'e (Reg 'al)))
           (Instr 'movzbq (list (Reg 'al) var)))]
    [(Assign var (Prim '< (list e1 e2)))
     (list (Instr 'cmpq (list (int-to-imm e2) (int-to-imm e1)))
           (Instr 'set (list 'l (Reg 'al)))
           (Instr 'movzbq (list (Reg 'al) var)))]
    [(Assign var var2) (if (equal? var var2) '() (list (Instr 'movq (list (int-to-imm var2) var))))]))

(define (resolve-select-instructions e)
  (match e
    [(Seq instr rest-seq) (append (instruction-to-x86 instr) (resolve-select-instructions rest-seq))]
    [(IfStmt (Prim 'eq? (list e1 e2)) (Goto l1) (Goto l2))
     (list (Instr 'cmpq (list (int-to-imm e2) (int-to-imm e1))) (JmpIf 'e l1) (Jmp l2))]
    [(IfStmt (Prim '< (list e1 e2)) (Goto l1) (Goto l2))
     (list (Instr 'cmpq (list (int-to-imm e2) (int-to-imm e1))) (JmpIf 'l l1) (Jmp l2))]
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
    [(Imm i) (Imm i)]
    [(Reg r) (Reg r)]))

(define (map-instr env instr)
  (match instr
    [(Instr op lst)
     (Instr op
            (for/list ([e lst])
              (arg->memory e env)))]
    [(Jmp label) (Jmp label)]
    [(Callq label arity) (Callq label arity)]))

(define (map-instrs env lst)
  (match lst
    ['() '()]
    [(cons x y) (cons (map-instr env x) (map-instrs env y))]))

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

(define (patch-instr instr)
  (match instr
    [(Instr op (list (Deref r1 o1) (Deref r2 o2)))
     (list (Instr 'movq (list (Deref r1 o1) (Reg 'rax))) (Instr op (list (Reg 'rax) (Deref r2 o2))))]
    [else (list instr)]))

(define (patch-instrs lst-instrs)
  (foldr append '() (map patch-instr lst-instrs)))

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  (match p
    [(X86Program info body)
     (X86Program info
                 (match body
                   [`((,label . ,block))
                    (match block
                      [(Block info instrs) `((,label . ,(Block info (patch-instrs instrs))))])]))]))

(define (generate-main body offset)
  (dict-set body
            'main
            (Block '()
                   (list (Instr 'pushq (list (Reg 'rbp)))
                         (Instr 'movq (list (Reg 'rsp) (Reg 'rbp)))
                         (Instr 'subq (list (Imm offset) (Reg 'rsp)))
                         (Jmp 'start)))))

(define (generate-conclusion body offset)
  (dict-set body
            'conclusion
            (Block '()
                   (list (Instr 'addq (list (Imm offset) (Reg 'rsp)))
                         (Instr 'popq (list (Reg 'rbp)))
                         (Retq)))))

(define (mult-16 n)
  (if (= (modulo n 16) 0) n (mult-16 (add1 n))))

;; prelude-and-conclusion : x86 -> x86
(define (prelude-and-conclusion p)
  (match p
    [(X86Program info body)
     (define var-list
       (match (dict-ref body 'start)
         [(Block info body-2) (dict-ref info 'locals)]))
     ;(define offset (* 8 (length var-list)))
     (define offset (mult-16 (* 8 (length var-list))))
     (define body-with-main (generate-main body offset))
     (define body-complete (generate-conclusion body-with-main offset))
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
    [(Prim 'and (list e1 e2)) (If (shrink-exp e1) (shrink-exp e2) (Bool #f))]
    [(Prim 'or (list e1 e2)) (If (shrink-exp e1) (Bool #t) (shrink-exp e2))]
    [(Int n) (Int n)]
    [(Var x) (Var x)]
    [(Bool t) (Bool t)]
    [(Let x e1 e2) (Let x (shrink-exp e1) (shrink-exp e2))]
    [(If e1 e2 e3) (If (shrink-exp e1) (shrink-exp e2) (shrink-exp e3))]
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

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(("shrink" ,shrink ,interp-Lif ,type-check-Lif)
    ("uniquify" ,uniquify ,interp-Lif ,type-check-Lif)
    ("patial evaluator Lvar" ,pe_Lif ,interp-Lif ,type-check-Lif)
    ;; Uncomment the following passes as you finish them.
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lif ,type-check-Lif)
    ("explicate control" ,explicate-control ,interp-Cif ,type-check-Cif)
    ("instruction selection" ,select-instructions ,interp-pseudo-x86-1)
    ;  ("assign homes" ,assign-homes ,interp-x86-0)
    ;  ("patch instructions" ,patch-instructions ,interp-x86-0)
    ;  ("prelude-and-conclusion" ,prelud  e-and-conclusion ,interp-x86-0)
    ))
