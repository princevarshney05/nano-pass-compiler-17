(define (mult [a : Integer] [b : Integer])
    : Integer
    (let ([x a]) 
        (begin 
            (while (> b 1)
                (begin
                (set! x (+ x a))
                (set! b (- b 1))))
        x)))

(define (factorial [n : Integer])
    : Integer
    (if (eq? n 1)
        1
        (mult (factorial (- n 1)) n)))

(factorial 5)