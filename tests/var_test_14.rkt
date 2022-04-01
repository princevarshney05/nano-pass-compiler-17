; test-5 uniquify, multiple variables
(let ([x 10]) (let ([y 20]) (let ([x 30]) (let ([x 40]) (let ([y x]) (+ x y))))))
