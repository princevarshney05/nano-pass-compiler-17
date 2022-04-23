(define (mult [a : Integer] [b : Integer])
    : Integer
    (let ([x a]) 
        (begin 
            (while (> b 0)
                (begin
                (set! x (+ x a))
                (set! b (- b 1))))
        x)))

(define (factorial [n : Integer])
    : Integer
    (if (eq? n 0)
        1
        (mult n (factorial (- n 1)))))

(factorial 5)