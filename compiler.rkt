#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "interp.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

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


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Lint p)
  (match p
    [(Program info e) (Program info (pe-exp e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; environment contains mapping between 
(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x) ; Variable exists, but we need a one to one mapping from dictionary
        (Var (dict-ref env x))
       ]
      [(Int n) (Int n)]
      [(Let x e body)
        (let ([newenv (dict-set env x (gensym))])
          (Let (dict-ref newenv x) ((uniquify-exp env) e) ((uniquify-exp newenv) body))
        )
      ]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))

(define (rco-atom e) 
    (
      match e
      [(Int n) (values (Int n) '())]
      [(Var x) (values (Var x) '())]
      [(Let key val body)
        (define-values (body-atom body-env) (rco-atom body))
        (values body-atom (cons (list key (rco-exp val)) body-env ))
      ]
      [(Prim op es)
        (define-values (new-exp new-env)
          (for/lists (l1 l2) ([e es]) (rco-atom e)))
          (define key (gensym))
          (define new-key-val-list (list (list key (Prim op new-exp))))
          (define combined-list-env (append (append* new-env) new-key-val-list))
          (values (Var key) combined-list-env)
      ]
    )
  )

(define (normalise-env-exp env exp)
  (match env
    ['() exp]
    ; [(list (list key val)) exp]
    [`(,(list key val) . ,rest-list)
      (Let key val (normalise-env-exp rest-list exp))
    ] 
    ))


(define (rco-exp e) 
    (
      match e
      [(Int n) (Int n)]
      [(Var x) (Var x)]
      [(Let key val body)
        (Let key (rco-exp val) (rco-exp body))
      ]
      [(Prim op es)
        (define-values (new-exp combined-list-env)
          (for/lists (l1 l2) ([e es]) (rco-atom e)))
          (normalise-env-exp (append* combined-list-env) (Prim op new-exp))
      ]
    )
  )

;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (match p
    [(Program info e) (Program info (rco-exp  e))]
  ))

;; explicate-control : R1 -> C0

(define (explicate_tail e)
  (match e
    [(Var x) (values (Return (Var x)) '())]
    [(Int n) (values (Return (Int n)) '())]
    [(Let x rhs body) (let*-values ([(intmd-seq1 intmd-vars1) (explicate_tail body)]
                                    [(intmd-seq2 intmd-vars2) (explicate_assign rhs x intmd-seq1)])
                                    (values intmd-seq2 (append intmd-vars1 intmd-vars2 `(,x))) )]
    [(Prim '- (list atom)) (values (Return (Prim '- (list atom))) '())]
    [(Prim '+ (list atom1 atom2)) (values (Return (Prim '+ (list atom1 atom2))) '())]
    [(Prim 'read '()) (values (Return (Prim 'read '())) '())]
    [else (error "explicate_tail unhandled case" e)]))


(define (explicate_assign e x cont)
  (match e
    [(Var y) (values (Seq (Assign (Var x) (Var y)) cont) '())]
    [(Int n) (values (Seq (Assign (Var x) (Int n)) cont) '())]
    [(Let y rhs body) (let*-values ([(intmd-seq1 intmd-vars1) (explicate_assign body x cont)]
                                    [(intmd-seq2 intmd-vars2) (explicate_assign rhs y intmd-seq1)])
                                    (values intmd-seq2 (append intmd-vars1 intmd-vars2 `(,y))))]
    [(Prim '- (list atom)) (values (Seq (Assign (Var x) (Prim '- (list atom))) cont) '())]
    [(Prim '+ (list atom1 atom2)) (values (Seq (Assign (Var x) (Prim '+  (list atom1 atom2))) cont) '())]
    [(Prim 'read '()) (values (Seq (Assign (Var x) (Prim 'read '())) cont) '())]
    [else (error "explicate_assign unhandled case" e)]))
  
(define (explicate-control p)
  (match p
    [(Program info body) (let-values ([(intmd-seq intmd-vars) (explicate_tail body)]) 
                                    ; (CProgram intmd-vars `((start . ,intmd-seq))))]))
                                    (CProgram (dict-set #hash() 'locals intmd-vars) `((start . ,intmd-seq))))]))

(define (int-to-imm e)
  (match e
    [(Int a) (Imm a)]
    [(Var a) (Var a)]
  )
)

(define (instruction-to-x86 e)
  (match e 
    [(Assign var (Prim '+ (list a b)))
      (list 
        (Instr 'movq (list (int-to-imm a) var))
        (Instr 'addq (list (int-to-imm b) var))
      )
    ]
    [(Assign var (Prim '- (list a)))
      (list 
        (Instr 'movq (list (int-to-imm a) var))
        (Instr 'negq (list var))
      )
    ]
    [(Assign var (Prim 'read '()))
      (cons (Callq 'read_int 0) ; 0 is a number of arguments
        (if (equal? var (Reg 'rax)) 
          '() 
          (list (Instr 'movq (list (Reg 'rax) var)))))
    ]
    [(Assign var var2)
      (if (equal? var var2) '()
        (list (Instr 'movq (list (int-to-imm var2) var))))]
  ))

(define (resolve-select-instructions e)
  (match e
    [(Seq instr rest-seq) 
      (append (instruction-to-x86 instr) (resolve-select-instructions rest-seq))
    ]
    [(Return e)
      (define instr (Assign (Reg 'rax) e))
      (append (instruction-to-x86 instr) (list (Jmp 'conclusion)))
    ]
    )
)

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (match p
    [
      (CProgram info body)
      (let ([frame-1 (car body)]) ; removing locals from info in program because we are now storing it in blocks
        (X86Program (dict-remove info 'locals) (list  (cons (car frame-1) (Block info (resolve-select-instructions (cdr frame-1))))  ) )
      )
    ])
  )

;; assign-homes : pseudo-x86 -> pseudo-x86

(define (create-env lst offset)
  (match lst
  ['() #hash()]
  [(cons x y) (dict-set (create-env y (- offset 8)) x (Deref 'rbp offset))]))

(define (arg->memory arg env)
  (match arg
  [(Var x) (dict-ref env x)]
  [(Imm i) (Imm i)]
  [(Reg r) (Reg r)]))

(define (map-instr env instr)
  (match instr
  [(Instr op lst) (Instr op (for/list ([e lst]) (arg->memory e env)))]
  [(Jmp label) (Jmp label)]
  [(Callq label arity) (Callq label arity)]))

(define (map-instrs env lst)
  (match lst
  ['() '()]
  [(cons x y) (cons (map-instr env x) (map-instrs env y))]))

(define (assign-homes p)
  (match p
  [(X86Program info body) (X86Program info (match body
                                          [`((,label . ,block))
                                            (match block
                                            [(Block info instrs) 
                                            `((,label . ,(Block info (map-instrs (create-env (dict-ref info 'locals) -8) instrs))))
                                            ]) 
                                          ]))]))

(define (patch-instr instr)
  (match instr
  [(Instr op (list (Deref r1 o1) (Deref r2 o2))) 
    (list (Instr 'movq (list (Deref r1 o1) (Reg 'rax))) (Instr op (list (Reg 'rax) (Deref r2 o2))))]
    [else (list instr)]))

(define (patch-instrs lst-instrs)
  (foldr append '() (map patch-instr lst-instrs)))

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
(match p
  [(X86Program info body) (X86Program info (match body
                                          [`((,label . ,block))
                                            (match block
                                              [(Block info instrs) 
                                              `((,label . ,(Block info (patch-instrs instrs))))
                                              ]) 
                                          ]))]))

(define (generate-main body offset) 
  (dict-set body 'main 
    (
      Block '()
      (list  
        (Instr 'pushq (list (Reg 'rbp))) 
        (Instr 'movq (list (Reg 'rsp) (Reg 'rbp))) 
        (Instr 'subq (list (Imm offset) (Reg 'rsp))) 
        (Jmp 'start) )
    )
  )
)

(define (generate-conclusion body offset) 
  (dict-set body 'conclusion 
    (Block '()
      (list 
        (Instr 'addq (list (Imm offset) (Reg 'rsp))) 
        (Instr 'popq (list (Reg 'rbp))) 
        (Retq))
      )
  )
)

(define (mult-16 n)
  (if (= (modulo n 16) 0) n (mult-16 (add1 n))))

;; prelude-and-conclusion : x86 -> x86
(define (prelude-and-conclusion p)
(match p
  [(X86Program info body) 
    (define var-list (match (dict-ref body 'start)
      [
        (Block info body-2) (dict-ref info 'locals)
      ])) 
    ;(define offset (* 8 (length var-list)))
    (define offset (mult-16 (* 8 (length var-list))))
    (define body-with-main (generate-main body offset))
    (define body-complete (generate-conclusion body-with-main offset))
    (X86Program info body-complete) 
  ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bonus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pe_neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe_add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe_sub r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx- n1 n2))]
    [(_ _) (Prim '- (list r1 r2))]))

(define (pe_exp e)
  (match e
    [(Int n) (Int n)]
    [(Var x) (Var x)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe_neg (pe_exp e1))]
    [(Prim '+ (list (Int e1) e2)) (pe_add (Int e1) (pe_exp e2))]
    [(Prim '+ (list e1 (Int e2) )) (pe_add (Int e2) (pe_exp e1))]
    [(Prim '+ (list e1 e2)) (pe_add (pe_exp e1) (pe_exp e2))]
    [(Let x e1 e2) (Let x (pe_exp e1) (pe_exp e2))]))


; Add a pe_intelligent_eval that evaluates the function from left to right
; and then evaluates the result
(define (pe_intelligent_exp e)
  (match e
    [(Int n) (Int n)]
    [(Var x) (Var x)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe_neg (pe_intelligent_exp e1))]
    [(Prim '+ (list (Int e1) (Prim '+ (list (Int e2) e3)))) (pe_add (pe_add (Int e1) (Int e2)) (pe_intelligent_exp e3))]
    [(Prim '+ (list (Prim '+ (list (Int e1) e2)) (Prim '+ (list (Int e3) e4)))) (pe_add (pe_add (Int e1) (Int e3)) (pe_add (pe_intelligent_exp e2) (pe_intelligent_exp e4)))]
    [(Prim '+ (list e1 e2)) (pe_add (pe_intelligent_exp e1) (pe_intelligent_exp e2))]
    [(Let x e1 e2) (Let x (pe_intelligent_exp e1) (pe_intelligent_exp e2))]))

(define (pe_Lvar p)
  (match p
    [(Program '() e) (Program '() (pe_intelligent_exp (pe_exp e)))]))

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `( 
    ;  ("patial evaluator Lvar" ,pe_Lvar ,interp-Lvar)
      ("uniquify" ,uniquify ,interp-Lvar)
     ;; Uncomment the following passes as you finish them.
     ("remove complex opera*" ,remove-complex-opera* ,interp-Lvar)
    ;  ("explicate control" ,explicate-control ,interp-Cvar)
    ;  ("instruction selection" ,select-instructions ,interp-x86-0)
    ;  ("assign homes" ,assign-homes ,interp-x86-0)
    ;  ("patch instructions" ,patch-instructions ,interp-x86-0)
    ;  ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-0)
     ))

