(let ([v (vector 1 2 3 4 (vector 1 2 3 4 5 (vector (vector 20 30 40) 1 2 3 4)))])
  (+ (+ (vector-ref v 0) (vector-ref (vector-ref v 4) 0))
     (vector-ref (vector-ref (vector-ref (vector-ref v 4) 5) 0) 2)))
