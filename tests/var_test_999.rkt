(let ([x 10]) (if (or (< x 10) (> x 10)) x (if (and (eq? x 10) (<= x 10)) x 5)))
