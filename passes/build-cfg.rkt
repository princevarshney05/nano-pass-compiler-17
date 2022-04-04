#lang racket

(require "../utilities.rkt")
(require graph)

(provide build-cfg)

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
