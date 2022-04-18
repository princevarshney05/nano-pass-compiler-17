#lang racket

(require "../utilities.rkt")
(require graph)

(provide build-cfg)

(define (add-cfg-edges g bs)
  (for ([e bs])
    (match e
      [(cons block (Block info instrs))
       (for ([instr instrs])
         (match instr
           [(JmpIf c label-target) (add-directed-edge! g block label-target)]
           [(Jmp label-target)
            (when (not (equal? 'conclusion label-target))
              (add-directed-edge! g block label-target))]
           [else g]))])))

(define (build-cfg p)
  (match p
    [(X86Program info body)
     (define g (unweighted-graph/directed '()))
     (add-vertex! g 'start)
     (add-cfg-edges g body)
     (X86Program (dict-set info 'cfg g) body)]))
