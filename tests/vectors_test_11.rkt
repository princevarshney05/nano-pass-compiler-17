(let ([x (if (< (read) 10) (vector 1 2 3 4 5 6 7 8 10 42) (vector 42 2 3 4 5 6 7 8 10 42))])
  (let ([y (vector x x 1 42 #t)])
    (if (vector-ref y 4) (vector-ref (vector-ref y 0) 0) (vector-ref y 3))))
