#lang racket
(require racket/set
         racket/stream)
(require racket/fixnum)
(require graph)
(require "graph-printing.rkt")
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "interp-Lif.rkt")
(require "interp-Cif.rkt")
(require "interp.rkt")
(require "utilities.rkt")
(require "custom_utilities.rkt")
(require "type-check-Lif.rkt")
(require "type-check-Cif.rkt")
(require graph)
(provide (all-defined-out))

; Passes
(require "passes/shrink.rkt")
(require "passes/uniquify.rkt")
(require "passes/partial-evaluator.rkt")
(require "passes/remove-complex-operations.rkt")
(require "passes/explicate-control.rkt")
(require "passes/select-instructions.rkt")
(require "passes/build-cfg.rkt")
(require "passes/uncover-live.rkt")
(require "passes/build-interference-graph.rkt")
(require "passes/allocate-registers.rkt")
(require "passes/patch-instructions.rkt")
(require "passes/prelude-conclusion.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lint examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy
(define (flip-exp e)
  (match e
    [(Var x) e]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (Prim '- (list (flip-exp e1)))]
    [(Prim '+ (list e1 e2)) (Prim '+ (list (flip-exp e2) (flip-exp e1)))]))

(define (flip-Lint e)
  (match e
    [(Program info e) (Program info (flip-exp e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; assign-homes : pseudo-x86 -> pseudo-x86

; local variables are now moved to Program info  and removed from Block Info
; (define (assign-homes p)
;   (match p
;     [(X86Program info body)
;      (X86Program info
;                  (match body
;                    [`((,label . ,block))
;                     (match block
;                       [(Block info instrs)
;                        `((,label . ,(Block info
;                                            (map-instrs (create-env (dict-ref info 'locals) -8)
;                                                        instrs))))])]))]))

; (X86Program '#hash()
;   (list
;     (cons 'start
;       (Block '#hash((locals . (g10901))) (list (Instr 'movq (list (Imm 10) (Var 'g10901))) (Instr 'movq (list (Var 'g10901) (Reg 'rax))) (Jmp 'conclusion))))))


;; build-interference-graph
;; write a code to build interference graph
;; Store the graph in info field of the program under the key 'conflicts'
;;; (define (build-interference-graph p)
;;;   (define interference-graph (undirected-graph `()))
;;;   (match p
;;;     [(X86Program pinfo (list (cons 'start block)))
;;;       (define local-vars (dict-ref pinfo 'locals))
;;;       ;;; (for/list ([item local-vars]) (print item)
;;;       ;;;   (add-vertex! interference-graph (Var item)))
;;;       (match block
;;;         [(Block binfo instrs)
;;;           (define live-vars (dict-ref binfo 'live-vars))
;;;           (add-interfering-edges instrs live-vars interference-graph)
;;;           ])
;;;       (define new-pinfo (dict-set pinfo 'conflicts interference-graph))
;;;       (X86Program new-pinfo (list (cons 'start block)))]))


;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(("shrink" ,shrink ,interp-Lif ,type-check-Lif)
    ("uniquify" ,uniquify ,interp-Lif ,type-check-Lif)
    ;;; ("patial evaluator Lvar" ,pe_Lif ,interp-Lif ,type-check-Lif)
    ;; Uncomment the following passes as you finish them.
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lif ,type-check-Lif)
    ("explicate control" ,explicate-control ,interp-Cif ,type-check-Cif)
    ("instruction selection" ,select-instructions ,interp-pseudo-x86-1)
    ("build cfg" ,build-cfg ,interp-pseudo-x86-1)
    ; ("print cfg" ,print-cfg ,interp-pseudo-x86-1)
    ("uncover live" ,uncover-live ,interp-pseudo-x86-1)
    ("build interference graph" ,build-interference-graph ,interp-pseudo-x86-1)
    ("allocate registers" ,allocate-registers ,interp-pseudo-x86-1)
    ("patch instructions" ,patch-instructions ,interp-x86-1)
    ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-1)))
