(let ([y 10]) (let ([x (if (< y 10) 10 5)]) (if (not (eq? x 10)) (+ x 10) (- x))))
