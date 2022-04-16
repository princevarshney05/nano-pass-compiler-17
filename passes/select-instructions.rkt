#lang racket
(require "../utilities.rkt")
(provide select-instructions
         int-to-imm)

(define (int-to-imm e)
  (match e
    [(Int a) (Imm a)]
    [(Var a) (Var a)]
    [(GlobalValue g) (Global g)]
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
    ; [(Assign var (Prim 'read '())) (list (Callq 'read_int 0))]
    [(Assign var (Prim 'vector-set! (list tup (Int n) rhs)))
     (list (Instr 'movq (list (int-to-imm tup) (Reg 'r11)))
           (Instr 'movq (list (int-to-imm rhs) (Deref 'r11 (* 8 (+ n 1)))))
           (Instr 'movq (list (Imm 0) var)))]
    [(Prim 'vector-set! (list tup (Int n) rhs))
     (list (Instr 'movq (list (int-to-imm tup) (Reg 'r11)))
           (Instr 'movq (list (int-to-imm rhs) (Deref 'r11 (* 8 (+ n 1))))))]
    [(Assign var (Prim 'vector-ref (list tup (Int n))))
     (list (Instr 'movq (list (int-to-imm tup) (Reg 'r11)))
           (Instr 'movq (list (Deref 'r11 (* 8 (+ n 1))) var)))]
    [(Assign var (Prim 'vector-length (list tup)))
     (list (Instr 'movq (list (int-to-imm tup) (Reg 'r11)))
           (Instr 'movq (list (Deref 'r11 0) (Reg 'rax)))
           (Instr 'sarq (list (Imm 1) (Reg 'rax)))
           (Instr 'andq (list (Imm 63) (Reg 'rax)))
           (Instr 'movq (list (Reg 'rax) var)))]
    [(Assign var (Allocate len type))
     (define tag (calculate-tag (cdr type) len))
     (list (Instr 'movq (list (Global 'free_ptr) (Reg 'r11)))
           (Instr 'addq (list (Imm (* 8 (+ len 1))) (Global 'free_ptr)))
           (Instr 'movq (list (Imm tag) (Deref 'r11 0)))
           (Instr 'movq (list (Reg 'r11) var)))]
    [(Assign var var2) (if (equal? var var2) '() (list (Instr 'movq (list (int-to-imm var2) var))))]
    [(Collect bytes)
     (list (Instr 'movq (list (Reg 'r15) (Reg 'rdi)))
           (Instr 'movq (list (int-to-imm bytes) (Reg 'rsi)))
           (Callq 'collect 2))]))

(define (resolve-select-instructions e)
  (match e
    [(Seq instr rest-seq) (append (instruction-to-x86 instr) (resolve-select-instructions rest-seq))]
    [(IfStmt (Prim 'eq? (list e1 e2)) (Goto l1) (Goto l2))
     (list (Instr 'cmpq (list (int-to-imm e2) (int-to-imm e1))) (JmpIf 'e l1) (Jmp l2))]
    [(IfStmt (Prim '< (list e1 e2)) (Goto l1) (Goto l2))
     (list (Instr 'cmpq (list (int-to-imm e2) (int-to-imm e1))) (JmpIf 'l l1) (Jmp l2))]
    [(IfStmt (Prim 'vector-ref args) (Goto l1) (Goto l2))
     (append (instruction-to-x86 (Assign (Reg 'rax) (Prim 'vector-ref args)))
             (list (Instr 'cmpq (list (Reg 'rax) (Imm 1))) (JmpIf 'e l1) (Jmp l2)))]
    [(Goto label) (list (Jmp label))]
    [(Return e)
     (define instr (Assign (Reg 'rax) e))
     (append (instruction-to-x86 instr) (list (Jmp 'conclusion)))]))

(define (calculate-tag types len)
  (define ptr-tag
    (for/fold ([tag 0]) ([t (in-list types)] [i (in-naturals 7)])
      (match t
        [`(Vector ,T ...) (bitwise-ior tag (arithmetic-shift 1 i))]
        [_ tag])))
  (bitwise-ior ptr-tag (arithmetic-shift len 1) 1))

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (match p
    [(CProgram info body)
     (X86Program info
                 (for/list ([frame body])
                   (cons (car frame) (Block '() (resolve-select-instructions (cdr frame))))))]))
