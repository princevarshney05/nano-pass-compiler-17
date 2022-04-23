(define (f [x : Integer])
  :
  Integer
  (+ x (read)))

(let ([y 0])
  (let ([x 1])
    (begin
      (while (< x 4)
             (begin
               (set! y (f x))
               (set! x (+ x 1))))
      y)))
