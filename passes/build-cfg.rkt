#lang racket

(require "../utilities.rkt")
(require graph)

(provide build-cfg)

(define (add-cfg-edges g bs name)
  (for ([e bs])
    (match e
      [(cons block (Block info instrs))
       (for ([instr instrs])
         (match instr
           [(JmpIf c label-target) (add-directed-edge! g block label-target)]
           [(Jmp label-target)
            (when (not (equal? (symbol-append name 'conclusion) label-target))
              (add-directed-edge! g block label-target))]
           [else g]))])))

;;; (define (build-cfg p)
;;;   (match p
;;;     [(X86Program info body)
;;;      (define g (unweighted-graph/directed '()))
;;;      (add-vertex! g 'start)
;;;      (add-cfg-edges g body)
;;;      (X86Program (dict-set info 'cfg g) body)]))

(define (build-cfg-defs def)
  (match def
    [(Def name params rtype info blocks)
     (define g (unweighted-graph/directed '()))
     ;;; (add-vertex! g 'start)
     (add-cfg-edges g blocks name)
     (Def name params rtype (dict-set info 'cfg g) blocks)]))

(define (build-cfg p)

  (match p
    [(ProgramDefs info defs) (ProgramDefs info (map build-cfg-defs defs))]))
