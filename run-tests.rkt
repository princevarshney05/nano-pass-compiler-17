#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-Lif.rkt")
(require "interp-Cif.rkt")
(require "interp.rkt")
(require "compiler.rkt")
(require "type-check-Lif.rkt")
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

;(interp-tests "var" #f compiler-passes interp-Lif "var_test" (tests-for "var"))
;(interp-tests "cond" type-check-Lif compiler-passes interp-Lif "cond_test" (tests-for "cond"))
;(interp-tests "my" type-check-Lif compiler-passes interp-Lif "my_test" (tests-for "my"))
; Uncomment the following when all the passes are complete to
; test the final x86 code.
(compiler-tests "var" type-check-Lif compiler-passes "var_test" (tests-for "var"))
(compiler-tests "cond" type-check-Lif compiler-passes "cond_test" (tests-for "cond"))
(compiler-tests "my" type-check-Lif compiler-passes "my_test" (tests-for "my"))

