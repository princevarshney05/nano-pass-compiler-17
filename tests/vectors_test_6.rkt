(let ([v1 (vector 1 2 3 4)])
  (let ([v2 (vector 50 40 30 20 10)])
    (let ([v3 (vector 5 10 15 20 25 30)])
      (+ (+ (vector-ref v1 1) (vector-ref v2 2)) (vector-ref v3 1)))))
