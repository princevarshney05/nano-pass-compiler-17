#lang racket
(require racket/set
         racket/stream)
(require racket/fixnum)
(require graph)
(require "graph-printing.rkt")
(require "interp-Lvec.rkt")
(require "interp-Lvec-prime.rkt")
(require "interp-Cvec.rkt")
(require "interp.rkt")
(require "utilities.rkt")
(require "custom_utilities.rkt")
(require "type-check-Lvec.rkt")
(require "type-check-Cvec.rkt")
(require graph)
(provide (all-defined-out))

; Passes
(require "passes/shrink.rkt")
(require "passes/uniquify.rkt")
(require "passes/expose-allocation.rkt")
(require "passes/uncover-get.rkt")
(require "passes/partial-evaluator.rkt")
(require "passes/remove-complex-operations.rkt")
(require "passes/explicate-control.rkt")
; (require "passes/test-locals.rkt")
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

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(("shrink" ,shrink ,interp-Lvec ,type-check-Lvec)
    ("uniquify" ,uniquify ,interp-Lvec ,type-check-Lvec)
    ("expose-allocation" ,expose-allocation ,interp-Lvec-prime ,type-check-Lvec)
    ("uncover get" ,uncover-get! ,interp-Lvec-prime,type-check-Lvec)
    ; ;;; ("patial evaluator Lvar" ,pe_Lif ,interp-Lif ,type-check-Lif)
    ; ;; Uncomment the following passes as you finish them.
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lvec-prime ,type-check-Lvec)
    ("explicate control" ,explicate-control ,interp-Cvec ,type-check-Cvec)
    ;;; ("test locals" ,test-locals ,interp-Cvec ,type-check-Cvec)
    ("instruction selection" ,select-instructions ,interp-pseudo-x86-2)
    ("build cfg" ,build-cfg ,interp-pseudo-x86-2)
    ; ("print cfg" ,print-cfg ,interp-pseudo-x86-1)
    ("uncover live" ,uncover-live ,interp-pseudo-x86-2)
    ("build interference graph" ,build-interference-graph ,interp-pseudo-x86-2)
    ("allocate registers" ,allocate-registers ,interp-pseudo-x86-2)
    ("patch instructions" ,patch-instructions ,interp-x86-1)
    ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-1)
    ))
