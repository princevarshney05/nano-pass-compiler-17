; test-4 uniquify, x in both defination and body
(let ([x (let ([x 1]) x)])
  (let ([x (+ x (let ([x 2]) x))])
    (let ([x (let ([x 3]) (+ x x))]) (let ([x (let ([x 4]) (+ x x))]) (+ x x)))))
