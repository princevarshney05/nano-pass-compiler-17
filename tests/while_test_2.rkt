(let ([x 1])
    (begin 
    (while (< x 42)
        (begin
          (set! x (+ x 1))))
    x))