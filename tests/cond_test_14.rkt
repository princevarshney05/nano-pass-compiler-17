; Ex 20 -1
(let ([x 4]) (let ([y 4]) (if (or (and (< x 5) (> y 3)) (and (> x 5) (<= y 3))) (+ x y) (- x y))))
