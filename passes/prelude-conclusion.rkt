#lang racket
(require "../utilities.rkt")

(provide prelude-and-conclusion)
(define (generate-main body offset used-callee num-root-spills)
  (dict-set body
            'main
            (Block '()
                   (append (list (Instr 'pushq (list (Reg 'rbp)))
                                 (Instr 'movq (list (Reg 'rsp) (Reg 'rbp))))
                           (for/list ([reg used-callee])
                             (Instr 'pushq (list (Reg reg))))
                           (list (Instr 'subq (list (Imm offset) (Reg 'rsp))))
                           (list
                              (Instr 'movq (list (Imm 16384) (Reg 'rdi)))
                              (Instr 'movq (list (Imm 16384) (Reg 'rsi)))
                              (Callq 'initialize 2)
                              (Instr 'movq (list (Global 'rootstack_begin) (Reg 'r15)))) 
                            (for/list ([i (in-range num-root-spills)])
                                (Instr 'movq (list (Imm 0) (Deref 'r15 (* 8 i)))))
                            (list (Instr 'addq (list (Imm (* 8 num-root-spills)) (Reg 'r15))))
                           (list (Jmp 'start))))))

(define (generate-conclusion body offset rev-used-callee num-root-spills)
  (dict-set body
            'conclusion
            (Block '()
                   (append
                          (list (Instr 'subq (list (Imm (* 8 num-root-spills)) (Reg 'r15)))) 
                          (list (Instr 'addq (list (Imm offset) (Reg 'rsp))))
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

     (define num-root-spills (dict-ref info 'num-root-spills))

     (define body-with-main (generate-main body offset used-callee num-root-spills))
     (define body-complete (generate-conclusion body-with-main offset (reverse used-callee) num-root-spills))
     (X86Program info body-complete)]))
