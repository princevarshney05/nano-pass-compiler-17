(define (f [x : Integer])
  :
  Integer
  (+ x 32))

(let ([x 1])
  (begin
    (while (< x 10) (set! x (+ x 1)))
    (f x)))
