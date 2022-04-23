#lang racket
(require "../utilities.rkt")

(provide prelude-and-conclusion)
(define (generate-main name body offset used-callee num-root-spills)
  (dict-set body
            name
            (Block '()
                   (append (list (Instr 'pushq (list (Reg 'rbp)))
                                 (Instr 'movq (list (Reg 'rsp) (Reg 'rbp))))
                           (for/list ([reg used-callee])
                             (Instr 'pushq (list (Reg reg))))
                           (list (Instr 'subq (list (Imm offset) (Reg 'rsp))))
                           
                           (if (equal? name 'main) 
                           (list (Instr 'movq (list (Imm 16384) (Reg 'rdi)))
                                 (Instr 'movq (list (Imm 16384) (Reg 'rsi)))
                                 (Callq 'initialize 2)
                                 (Instr 'movq (list (Global 'rootstack_begin) (Reg 'r15)))) '())
                           (for/list ([i (in-range num-root-spills)])
                             (Instr 'movq (list (Imm 0) (Deref 'r15 (* 8 i)))))
                           (list (Instr 'addq (list (Imm (* 8 num-root-spills)) (Reg 'r15))))
                           (list (Jmp (symbol-append name 'start)))))))

(define (generate-conclusion-instrs offset rev-used-callee num-root-spills)
  (append (list (Instr 'subq (list (Imm (* 8 num-root-spills)) (Reg 'r15))))
                           (list (Instr 'addq (list (Imm offset) (Reg 'rsp))))
                           
                           (for/list ([reg rev-used-callee])
                             (Instr 'popq (list (Reg reg))))
                           (list (Instr 'popq (list (Reg 'rbp)))))
)

(define (generate-conclusion name body offset rev-used-callee num-root-spills)
  (dict-set body
            (symbol-append name 'conclusion)
            (Block '()
                   (append (generate-conclusion-instrs offset rev-used-callee num-root-spills) (list (Retq))))))

(define (mult-16 n)
  (if (= (modulo n 16) 0) n (mult-16 (add1 n))))

;; prelude-and-conclusion : x86 -> x86
;;; (define (prelude-and-conclusion p)
;;;   (match p
;;;     [(X86Program info body)

;;;      (define used-callee (set->list (dict-ref info 'used-callee)))
;;;      (define offset
;;;        (- (mult-16 (+ (* 8 (dict-ref info 'spill-count)) (* 8 (length used-callee))))
;;;           (* 8 (length used-callee))))

;;;      (define num-root-spills (dict-ref info 'num-root-spills))

;;;      (define body-with-main (generate-main body offset used-callee num-root-spills))
;;;      (define body-complete
;;;        (generate-conclusion body-with-main offset (reverse used-callee) num-root-spills))
;;;      (X86Program info body-complete)]))


(define (prelude-and-conclusion-defs def)
  (match def
  [(Def name params rtype info blocks-cfg)
  (define used-callee (set->list (dict-ref info 'used-callee)))
  (define offset
       (- (mult-16 (+ (* 8 (dict-ref info 'spill-count)) (* 8 (length used-callee))))
          (* 8 (length used-callee))))

  (define num-root-spills (dict-ref info 'num-root-spills))

  (define new-blocks (for/list ([e blocks-cfg])
    (match e
      [(cons block (Block info instrs))
        (cons block (Block info 
      (append*
       (for/list ([instr instrs])
          (match instr
           [(TailJmp arg arity)
            (append (generate-conclusion-instrs offset (reverse used-callee) num-root-spills)
                    (list (IndirectJmp arg)))]
           [_ (list instr)])))))
       ])))
  (define body-with-main (generate-main name new-blocks offset used-callee num-root-spills))
  (define body-complete
       (generate-conclusion name body-with-main offset (reverse used-callee) num-root-spills))
  (Def name params rtype info body-complete)
  ]))

(define (prelude-and-conclusion p)
  (match p
  [(ProgramDefs info defs) 
  (define new-defs (map prelude-and-conclusion-defs defs))
  (X86Program info (append-map Def-body new-defs))]))