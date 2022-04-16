; (let ([v (vector 1 20 30 40 1)])
;   (let ([x (vector-set! v 4 2)]) (+ (vector-ref v 3) (vector-ref v 4))))

(let ([v (vector 1 20 30 40 1)])
    (begin 
        (vector-set! v 4 2)
        (+ (vector-ref v 3) (vector-ref v 4))))