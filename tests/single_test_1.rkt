; (let ([t (vector 40 #t (vector 2))]) (if (vector-ref t 1)
;          (+ (vector-ref t 0)
;             (vector-ref (vector-ref t 2) 0))
; 44))

;;;  (let ([v1 (vector 1 2 3)]) 
;;;      (let ([v2 (vector 3 4 5)])
;;;      (+ (vector-ref v1 0) (vector-ref v2 0))))
;;; (if (> 1 2) (+ 1 2) (+ 3 4))

;;; (let ([v (vector 1 20 30 40 1)])
;;;     (begin 
;;;         (vector-set! v 4 2)
;;;         (+ (vector-ref v 3) (vector-ref v 4))))


;;; (let ([v (vector 42)])
;;;     (if (< (vector-length v) 2) 5 10))

;;; (let ([v (vector 1 2)]) 42)

;;;  (define (id [x : Integer]) : Integer 
;;;  (if (> x 1) x x))
;;;  (id 42)

;  (define (id [x : Integer]) : Integer x)
;  (id 42)

(define (id [a : Integer] [b : Integer] [c : Integer] [d : Integer] [e : Integer] [f : Integer] [g : Integer] [h : Integer]) : Integer (
    + h 
        (- b
            (+ c 
                (+ d
                    (- e
                        (+ f
                            (- g a))))))
 ))
 (id 1 2 3 4 5 6 20 1)

;  (define (id [a : Integer] [b : Integer] [c : Integer] [d : Integer] [e : Integer] [f : Integer] [g : Integer] [h : Integer]) : Integer (
;     + a 
;         (+ b
;             (+ c 
;                 (+ d
;                     (+ e
;                         (+ f
;                             (+ g h))))))
;  ))
;  (id 1 2 3 4 5 6 20 1)
