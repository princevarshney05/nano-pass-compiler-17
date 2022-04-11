#lang racket

(require "../utilities.rkt")

(provide patch-instructions)

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

(define (patch-instructions p)
  (match p
    [(X86Program info body)
     (X86Program info
                 (for/list ([block body])
                   (match block
                     [(cons label (Block binfo instrs))
                      (cons label (Block binfo (patch-instrs instrs)))])))]))
