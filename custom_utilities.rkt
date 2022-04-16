#lang racket
(require "utilities.rkt")
(require graph)

(provide (all-defined-out))

(define labels->live '())
(define (custom-live-labels-set! labels)
    (set! labels->live labels))

(define (valid-set x)
  (match x
    [(Imm t) (set)]
    [(ByteReg t) (set (byte-reg->full-reg t))]
    [else (set x)]))

(define (get-read-set-from-label label)
  (match label 
  ['collect (set 'rdi 'rsi)]
  [else (set)]
  ))

(define (get-read-write-sets instr)
  (match instr
    [(Instr 'movq (list A B))
     (define read-set (valid-set A))
     (define write-set (valid-set B))
     (values read-set write-set)]
    [(Instr 'movzbq (list A B))
     (define read-set (valid-set A))
     (define write-set (valid-set B))
     (values read-set write-set)]
    [(Instr 'addq (list A B))
     (define read-set (set-union (valid-set A) (valid-set B)))
     (define write-set (valid-set B))
     (values read-set write-set)]
    [(Instr 'subq (list A B))
     (define read-set (set-union (valid-set A) (valid-set B)))
     (define write-set (valid-set B))
     (values read-set write-set)]
    [(Instr 'negq (list A))
     (define read-set (valid-set A))
     (define write-set (valid-set A))
     (values read-set write-set)]
    [(Jmp x) (values (dict-ref labels->live x) (set))]
    [(JmpIf c x) (values (dict-ref labels->live x) (set))]
    [(Instr 'xorq (list A B))
     (define read-set (set-union (valid-set A) (valid-set B)))
     (define write-set (valid-set B))
     (values read-set write-set)]
    [(Instr 'cmpq (list A B))
     (define read-set (set-union (valid-set A) (valid-set B)))
     (define write-set (set))
     (values read-set write-set)]
    ;;; (values (set) caller-save)
    [(Callq label n)
     (values (list->set (map (lambda (r) (Reg r)) (set->list (get-read-set-from-label label))))
             (list->set (map (lambda (r) (Reg r)) (set->list caller-save))))]
    [(Instr 'set (list A B))
     (define read-set (valid-set B))
     (define write-set (valid-set B))
     (values read-set write-set)]

    [else (error "read-write-sets: Unhandled case")]))

(define (get-live-vars lst)
  (match lst
    ['() (list (set))]
    [(cons x y)
     (define live-vars (get-live-vars y))
     (define last-set (car live-vars))
     (define-values (read-set write-set) (get-read-write-sets x))

     (define new-set (set-union (set-subtract last-set write-set) read-set))
    (cons new-set live-vars)]))

(define (print-cfg p)
  (match p
    [(X86Program info body)
     (print-graph (dict-ref info 'cfg))
     p]))