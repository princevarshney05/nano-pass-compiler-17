#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-Lvec.rkt")
(require "interp-Cvec.rkt")
(require "interp.rkt")
(require "compiler.rkt")
(require "type-check-Lvec.rkt")
; (debug-level 1)
; (AST-output-syntax 'concrete-syntax)

;; all the files in the tests/ directory with extension ".rkt".
(define all-tests
  (map (lambda (p) (car (string-split (path->string p) ".")))
       (filter (lambda (p)
                 (string=? (cadr (string-split (path->string p) ".")) "rkt"))
               (directory-list (build-path (current-directory) "tests")))))

(define (tests-for r)
  (map (lambda (p)
         (caddr (string-split p "_")))
       (filter
        (lambda (p)
          (string=? r (car (string-split p "_"))))
        all-tests)))

;;; (interp-tests "var" #f compiler-passes interp-Lvec "var_test" (tests-for "var"))
;;; (interp-tests "cond" type-check-Lvec compiler-passes interp-Lvec "cond_test" (tests-for "cond"))
;;; (interp-tests "my" type-check-Lvec compiler-passes interp-Lvec "my_test" (tests-for "my"))
;;; (interp-tests "while" type-check-Lvec compiler-passes interp-Lvec "while_test" (tests-for "while"))
;;; (interp-tests "vectors" type-check-Lvec compiler-passes interp-Lvec "vectors_test" (tests-for "vectors"))

; Single Interp test
; (interp-tests "single" type-check-Lvec compiler-passes interp-Lvec "single_test" (tests-for "single"))
; Single Compiler test
(compiler-tests "single" type-check-Lvec compiler-passes "single_test" (tests-for "single"))

; Uncomment the following when all the passes are complete to
; test the final x86 code.
(compiler-tests "var" type-check-Lvec compiler-passes "var_test" (tests-for "var"))
(compiler-tests "my" type-check-Lvec compiler-passes "my_test" (tests-for "my"))
(compiler-tests "cond" type-check-Lvec compiler-passes "cond_test" (tests-for "cond"))
(compiler-tests "while" type-check-Lvec compiler-passes "while_test" (tests-for "while"))
(compiler-tests "vectors" type-check-Lvec compiler-passes "vectors_test" (tests-for "vectors"))
