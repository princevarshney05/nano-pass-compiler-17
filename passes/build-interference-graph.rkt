#lang racket
(require "../utilities.rkt")
(require "../graph-printing.rkt")
(require "../custom_utilities.rkt")
(require graph)

(provide build-interference-graph)
(define (build-interference-graph p)
  (define interference-graph (undirected-graph `()))
  (match p
    [(X86Program pinfo body)
     (define locals-types (dict-ref pinfo 'locals-types))
     (for/list ([block body])
       (match block
         [(cons label (Block binfo instrs))
          (define live-vars (dict-ref binfo 'live-vars))
          (add-interfering-edges instrs live-vars interference-graph locals-types)]))
     (print-dot interference-graph "../interference-graph")

     (define new-pinfo (dict-set pinfo 'conflicts interference-graph))
     (X86Program new-pinfo body)]))

(define (add-interfering-edges instrs live-vars interference-graph locals-types)
  (for ([inst instrs] [live-var live-vars])
    (match inst
      [(Callq 'collect n)
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
