#lang racket
(require racket/set
         racket/stream)
(require racket/fixnum)
(require graph)
(require "graph-printing.rkt")
(require "interp-Lfun.rkt")
(require "interp-Lfun-prime.rkt")
(require "interp-Cfun.rkt")
(require "interp.rkt")
(require "utilities.rkt")
(require "custom_utilities.rkt")
(require "type-check-Lfun.rkt")
(require "type-check-Cfun.rkt")
(require graph)
(provide (all-defined-out))

; Passes
(require "passes/shrink.rkt")
(require "passes/uniquify.rkt")
(require "passes/expose-allocation.rkt")
(require "passes/uncover-get.rkt")
(require "passes/reveal-functions.rkt")
(require "passes/limit-functions.rkt")
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
  `(("shrink" ,shrink ,interp-Lfun ,type-check-Lfun)
    ("uniquify" ,uniquify ,interp-Lfun ,type-check-Lfun)
    ("uncover get" ,uncover-get! ,interp-Lfun ,type-check-Lfun)
    ("reveal functions" ,reveal-functions ,interp-Lfun-prime ,type-check-Lfun)
    ("limit functions" ,limit-functions ,interp-Lfun-prime ,type-check-Lfun)
    ("expose-allocation" ,expose-allocation ,interp-Lfun-prime ,type-check-Lfun)
    ; ; ;;; ("patial evaluator Lvar" ,pe_Lif ,interp-Lif ,type-check-Lif)
    ; ; ;; Uncomment the following passes as you finish them.
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lfun-prime ,type-check-Lfun)
    ("explicate control" ,explicate-control ,interp-Cfun ,type-check-Cfun)
    ("instruction selection" ,select-instructions ,interp-pseudo-x86-3)
    ("build cfg" ,build-cfg ,interp-pseudo-x86-3)
    ("uncover live" ,uncover-live ,interp-pseudo-x86-3)
    ("build interference graph" ,build-interference-graph ,interp-pseudo-x86-3)
    ("allocate registers" ,allocate-registers ,interp-pseudo-x86-3)
    ; ; ("patch instructions" ,patch-instructions ,interp-x86-1)
    ; ; ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-1)
    ))
