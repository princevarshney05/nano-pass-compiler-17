; test-3 patch-instructions
(let ([x 10]) (let ([y (+ x 10)]) (let ([x (+ y x)]) (+ x y))))
