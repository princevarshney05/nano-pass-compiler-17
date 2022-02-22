#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
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
        (values body-atom (append (list key (rco-exp val)) body-env ))
      ]
      [ (Prim '+ (list a b)) 
        (define-values (a-atom a-list-env) (rco-atom a))
        (define-values (b-atom b-list-env) (rco-atom b))
        (define key (gensym))

        (define new-exp (Prim '+ (list a-atom b-atom)))
        (define new-key-val-list (list (list key new-exp)))
        (define combined-list-env (append a-list-env b-list-env new-key-val-list))
        
        ; (values (Var key) (list (list key (normalise-env-exp combined-list-env new-exp))))
        (values (Var key) combined-list-env)
      ]
      [ (Prim '- (list a)) 
        (define-values (a-atom a-list-env) (rco-atom a))
        (define key (gensym))

        (define new-exp (Prim '- (list a-atom)))
        (define new-key-val-list (list (list key new-exp)))
        (define combined-list-env (append a-list-env new-key-val-list))
        
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
      [(Prim '+ (list a b)) 
        (define-values (a-atom a-list-env) (rco-atom a))
        (define-values (b-atom b-list-env) (rco-atom b))
        (define new-exp (Prim '+ (list a-atom b-atom)))
        (define combined-list-env (append a-list-env b-list-env))
        (normalise-env-exp combined-list-env new-exp)
      ]
      [(Prim '+ (list a)) 
        (define-values (a-atom a-list-env) (rco-atom a))
        (define new-exp (Prim '- (list a-atom)))
        (normalise-env-exp a-list-env new-exp)
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
    [else (error "explicate_assign unhandled case" e)]))
    
(define (explicate-control p)
  (match p
    [(Program info body) (let-values ([(intmd-seq intmd-vars) (explicate_tail body)]) 
                                    ; (CProgram intmd-vars `((start . ,intmd-seq))))]))
                                    (CProgram (dict-set #hash() 'var intmd-vars) `((start . ,intmd-seq))))]))

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (error "TODO: code goes here (select-instructions)"))

;; assign-homes : pseudo-x86 -> pseudo-x86
(define (assign-homes p)
  (error "TODO: code goes here (assign-homes)"))

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  (error "TODO: code goes here (patch-instructions)"))

;; prelude-and-conclusion : x86 -> x86
(define (prelude-and-conclusion p)
  (error "TODO: code goes here (prelude-and-conclusion)"))

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `( ("uniquify" ,uniquify ,interp-Lvar)
     ;; Uncomment the following passes as you finish them.
     ("remove complex opera*" ,remove-complex-opera* ,interp-Lvar)
     ("explicate control" ,explicate-control ,interp-Cvar)
     ;; ("instruction selection" ,select-instructions ,interp-x86-0)
     ;; ("assign homes" ,assign-homes ,interp-x86-0)
     ;; ("patch instructions" ,patch-instructions ,interp-x86-0)
     ;; ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-0)
     ))

