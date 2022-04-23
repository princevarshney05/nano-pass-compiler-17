#lang racket
(require "../utilities.rkt")
(require "../graph-printing.rkt")
(require "../custom_utilities.rkt")
(require graph)

(provide build-interference-graph)



(define (add-interfering-edges instrs live-vars interference-graph locals-types)
  (for ([inst instrs] [live-var live-vars])
    (match inst
      [(or (Callq label n) (IndirectCallq label n))
       (define-values (_ write-vars) (get-read-write-sets inst))
       (define write-vars-vec (set-union write-vars callee-save))
       (for/list ([v live-var])
         (for/list ([d (if (is-vector v locals-types) write-vars-vec write-vars)])

           (when (not (or (equal? v d) (has-edge? interference-graph v d)))
             (add-edge! interference-graph v d))))]

      [(or (Instr 'movq (list s d)) (Instr 'movzbq (list s d)))
       (for/list ([v live-var])
         (when (and (not (set-member? (valid-set s) v)) (not (set-member? (valid-set d) v)))
           (for ([d_ (valid-set d)])
             (add-edge! interference-graph v d_))))]
      [_
       (define-values (_ write-vars) (get-read-write-sets inst))
       (for/list ([d write-vars])
         (for/list ([v live-var])
           (when (not (or (equal? v d) (has-edge? interference-graph v d)))
             (add-edge! interference-graph v d))))])))





(define (build-interference-graph-defs def)
  (define interference-graph (undirected-graph `()))
  (match def
    [(Def name params rtype info blocks-cfg)
     (define locals-types (dict-ref info 'locals-types))
     (custom-live-labels-set! (dict-ref info 'labels->live))
     (for/list ([block blocks-cfg])
       (match block
         [(cons label (Block binfo instrs))
          (define live-vars (dict-ref binfo 'live-vars))
          (add-interfering-edges instrs live-vars interference-graph locals-types)]))
      (print-dot interference-graph "../interference-graph")
      (Def name params rtype (dict-set info 'conflicts interference-graph) blocks-cfg)
      ]))

(define (build-interference-graph p)
  (match p
    [(ProgramDefs info defs) (ProgramDefs info (map build-interference-graph-defs defs))]))


