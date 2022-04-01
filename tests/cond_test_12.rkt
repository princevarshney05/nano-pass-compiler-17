(let ([x 10]) (if (> x 5) (+ x (if (eq? x 5) 10 5)) (+ x (if (not (eq? x 5)) 20 1))))
