#lang racket
(require "../utilities.rkt")
(require "../priority_queue.rkt")
(require graph)

(provide allocate-registers)


(define (arg->memory arg env)
  (match arg
    [(Var x) (dict-ref env x)]
    [else arg]))

(define (map-instr env instr)
  (match instr
    [(Instr op lst)
     (Instr op
            (for/list ([e lst])
              (arg->memory e env)))]
    [else instr]))

(define (map-instrs env lst)
  (match lst
    ['() '()]
    [(cons x y) (cons (map-instr env x) (map-instrs env y))]))

(define (map-registers color-map)
  (define spill-count 0)
  (define used-callee (set))
  (dict-for-each color-map
                 (lambda (k v)
                   (when (< v 12)
                     (set! used-callee (set-add used-callee (dict-ref num-to-reg v))))))
  (set! used-callee (set-intersect callee-save used-callee))
  (dict-for-each
    color-map
    (lambda (k v)
     (match (< v 12)
       [#t
        (dict-set! color-map k (Reg (dict-ref num-to-reg v)))
        ]
       [#f
        (dict-set! color-map k (Deref 'rbp (- (* (- 8) (- v 11)) (* 8 (set-count used-callee))))) 
        (set! spill-count (+ spill-count 1))])))
  (values color-map spill-count (set-intersect callee-save used-callee))
)


(define (allocate-registers p)
  (match p
    [(X86Program info body)
     (define interference-graph (dict-ref info 'conflicts))
     (define color-map (color-graph interference-graph))
     (define-values (color-reg spill-count used-callee) (map-registers color-map))
     (X86Program (dict-set (dict-set info 'spill-count spill-count) 'used-callee used-callee)
        (for/list ([block body])
            (match block
                [(cons label (Block binfo instrs))
                 (cons label (Block binfo (map-instrs color-reg instrs)))])))
    ]
  )
)

(define all-registers (set-union caller-save callee-save))

(define num-to-reg
  (dict-set* #hash()
             -2
             'rsp
             -1
             'rax
             0
             'r8
             1
             'r9
             2
             'r10
             3
             'r11
             4
             'r12
             5
             'r13
             6
             'r14
             7
             'rbx
             8
             'rcx
             9
             'rdx
             10
             'rsi
             11
             'rdi))

(define reg-to-num
  (dict-set* #hash()
             'rsp
             -2
             'rax
             -1
             'r8
             0
             'r9
             1
             'r10
             2
             'r11
             3
             'r12
             4
             'r13
             5
             'r14
             6
             'rbx
             7
             'rcx
             8
             'rdx
             9
             'rsi
             10
             'rdi
             11))


(define (color-graph interference-graph)
    (define nodes (list->set (get-vertices interference-graph)))
    (define regs-in-graph (set-intersect all-registers (list->set nodes)))
    (define all-vars (set-subtract nodes regs-in-graph))
    (define already_assigned_colors (make-hash))

    ; initialising the already-assigned-colors for each node
    (for ([vertex nodes])
       (dict-set! already_assigned_colors vertex '()))
    
    ; assigning exception numbers to neighbours of registers
    (for ([reg regs-in-graph])
        (for ([node (in-neighbors interference-graph reg)])
            (dict-set! already_assigned_colors
                node 
                (set-add (dict-ref already_assigned_colors node)
                    (dict-ref reg-to-num reg)
                )
            )
        )
    )

     ; inserting in priority queue
    (define pq
        (make-pqueue (lambda (a b)
                    (> (length (dict-ref already_assigned_colors a))
                        (length (dict-ref already_assigned_colors b))))))
    
    (define node_references (make-hash))

    (for/list ([var all-vars])
        (define node_ref (pqueue-push! pq var))
        (dict-set! node_references var node_ref)
    )

    (define result (make-hash))

    ; traverse priority queue
    (for ([i (pqueue-count pq)])
        (let ([var (pqueue-pop! pq)])
            (define cols (dict-ref already_assigned_colors var))
            (define assigned-color (get-min-color cols 0))
            (dict-set! result var assigned-color)
            (define neighbor-vars (set-intersect (list->set (get-neighbors interference-graph var)) all-vars))
            (for ([node neighbor-vars])
                (dict-set! already_assigned_colors node
                            (set-add (dict-ref already_assigned_colors node) assigned-color))
                (pqueue-decrease-key! pq (dict-ref node_references node))
            )
        )
    )
    result
)

(define (get-min-color color-set n)
  (if (equal? (member n color-set) #f) n (get-min-color color-set (+ n 1))))
