(let ([n (- (+ 40 2))])
  (begin
    (if (< n 0) (set! n (- n)) (set! n n))
    n))
