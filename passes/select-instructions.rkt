#lang racket
(require "../utilities.rkt")
(provide select-instructions
         int-to-imm)

(define (int-to-imm e)
  (match e
    [(Int a) (Imm a)]
    [(Var a) (Var a)]
    [(Bool #t) (Imm 1)]
    [(Bool #f) (Imm 0)]
    [(Void) (Imm 0)]))

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
    ; Read can be standalone on left
    [(Assign var (Prim 'read '())) (list (Callq 'read_int 0))]
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
