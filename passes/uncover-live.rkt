#lang racket
(require "../utilities.rkt")
(require "../custom_utilities.rkt")
(require graph)

(provide uncover-live)

(define (uncover-live-block p)
  (match p
    [(cons label (Block info instrlist))
     (define live-vars (get-live-vars instrlist))
     (custom-live-labels-set! (dict-set labels->live label (car live-vars)))
     (define new-info (dict-set info 'live-vars (cdr live-vars)))

     (cons label (Block new-info instrlist))]))

(define (uncover-live p)
  (custom-live-labels-set! (hash 'conclusion (set (Reg 'rax) (Reg 'rsp))))
  (match p
    [(X86Program info body)
     (X86Program info
                 (for/list ([label (tsort (transpose (dict-ref info 'cfg)))])
                   (uncover-live-block (cons label (dict-ref body label)))))]))
