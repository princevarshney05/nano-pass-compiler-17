; test-3 slect-instructions
(let ([x 5]) (+ 1 (+ (read) (let ([x (+ x 10)]) (+ x 20)))))
