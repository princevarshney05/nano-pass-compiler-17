; Ex 23 - 3
(if (if (if (if #f #t (not (< 2 3))) (> (read) 2) (eq? 1 1))
        (let ([y (read)]) (eq? y 2))
        (not (< 3 2)))
    1
    2)
