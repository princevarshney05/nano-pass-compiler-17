(let ([x 0])
  (begin
    (while (< x 42) (set! x (+ x 1)))
    x))
