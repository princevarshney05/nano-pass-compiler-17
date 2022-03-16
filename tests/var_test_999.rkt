(let ([x 10])
(if (or #t #f)
    x
    (if (and #t #t)
        x
        5)))
