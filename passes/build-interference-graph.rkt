
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
     (define local-vars (dict-ref pinfo 'locals))
     (for/list ([block body])
       (match block
         [(cons label (Block binfo instrs))
          (define live-vars (dict-ref binfo 'live-vars))
          (add-interfering-edges instrs live-vars interference-graph)]))
     (print-dot interference-graph "../interference-graph")

     (define new-pinfo (dict-set pinfo 'conflicts interference-graph))
     (X86Program new-pinfo body)]))

(define (add-interfering-edges instrs live-vars interference-graph)
  (for ([inst instrs] [live-var live-vars])
    (match inst
      [(or (Instr 'movq (list s d)) (Instr 'movzbq (list s d)))
       (for/list ([v live-var])
         (when (and (and (not (equal? v s)) (not (equal? v d)))
                    (not (has-edge? interference-graph v d)))
           (add-edge! interference-graph v d)))]
      [_
       (define-values (_ write-vars) (get-read-write-sets inst))
       (for/list ([d write-vars])
         (for/list ([v live-var])
           (when (not (or (equal? v d) (has-edge? interference-graph v d)))
             (add-edge! interference-graph v d))))])))
