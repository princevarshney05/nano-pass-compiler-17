; test-2 uniquify
(let ([x (let ([x (let ([x 10]) (+ 1 x))]) (+ 2 x))]) (+ x 10))
