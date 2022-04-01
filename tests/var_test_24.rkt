; test-1 patch-instructions
(let ([x 10]) (let ([y 30]) (let ([y (let ([y x]) y)]) (+ y x))))
