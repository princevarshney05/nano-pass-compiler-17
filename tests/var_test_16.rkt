; test-2 RCO
; Test removing complex operand
(+ 10 (+ 1 (let ([x 1]) (+ x (+ 2 (- 10))))))
