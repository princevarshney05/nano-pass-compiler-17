#lang racket
(require "../utilities.rkt")

(provide prelude-and-conclusion)
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
