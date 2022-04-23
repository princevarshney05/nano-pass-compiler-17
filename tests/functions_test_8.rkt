(define (mult [a : Integer] [b : Integer])
    : Integer
    (let ([x a]) 
        (begin 
            (while (> b 0)
                (begin
                (set! x (+ x a))
                (set! b (- b 1))))
        x)))

(mult 21 2)