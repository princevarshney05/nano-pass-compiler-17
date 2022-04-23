#lang racket
(require "utilities.rkt")
(require graph)

(provide (all-defined-out))

;;; (define (is-vector v locals-types)
;;;   (list? (dict-ref locals-types v #f)))

(define (is-vector v locals-types)
  (match (dict-ref locals-types v #f)
    [`(Vector ,T ...) #t]
    [else #f]))

(define labels->live '())
(define (custom-live-labels-set! labels)
  (set! labels->live labels))

(define (valid-set x)
  (match x
    [(Imm t) (set)]
    [(Var x) (set x)]
    [(Reg reg) (set reg)]
    [(ByteReg t) (set (byte-reg->full-reg t))]
    [(Deref reg offset) (set reg)]
    [(Global _) (set)]))

(define (get-read-set-from-label label)
  (match label
    ['collect (set 'rdi 'rsi)]
    [else (set)]))

(define (get-argument-registers n)
  (list->set (take (vector->list arg-registers) n)))

(define (get-read-write-sets instr)
  (match instr
    [(Instr 'leaq (list A B))
     (define read-set (valid-set A))
     (define write-set (valid-set B))
     (values read-set write-set)
    ]
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
    ;;; [(Callq label n) (values (get-read-set-from-label label) caller-save)]
    [(Callq label n) (values (get-argument-registers n) caller-save)]
    [ (IndirectCallq label n)  (values (set-union (valid-set label) (get-argument-registers n)) caller-save)]
    [(TailJmp label n) (values (set-union (valid-set label) (set 'rax 'rsp) (get-argument-registers n)) caller-save)]
    [(Instr 'set (list A B))
     (define read-set (valid-set B))
     (define write-set (valid-set B))
     (values read-set write-set)]
    [(Instr 'andq (list A B))
     (define read-set (set))
     (define write-set (valid-set B))
     (values read-set write-set)]
    [(Instr 'sarq (list A B))
     (define read-set (set))
     (define write-set (valid-set B))
     (values read-set write-set)]
    [else
     (print instr)
     (error "read-write-sets: Unhandled case")]))

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
