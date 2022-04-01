; (let ([x (read)])
; (let ([y (read)])
; (if (if (< x 1)
; (eq? x 0)
; (eq? x 2))
; (+ y 2)
; (+ y 10))))

;;; (let ([x 1]) (+ x 1))

;;; (let ([v 1])
;;; (let ([w 42])
;;; (let ([x (+ v 7)])
;;; (let ([y x])
;;; (let ([z (+ x w)])
;;; (+ z (- y)))))))

; (+ (+ (read) 1) (+ (read) 1))
; // 12 nested lets a-l variables - Spilling Variables - Ex 15 - 1
(let ([a 1])
  (+ a
     (let ([b 2])
       (+ b
          (let ([c 3])
            (+ c
               (let ([d 4])
                 (+ d
                    (let ([e 5])
                      (+ e
                         (let ([f 6])
                           (+ f
                              (let ([g 7])
                                (+ g
                                   (let ([h 8])
                                     (+ h
                                        (let ([i 9])
                                          (+ i
                                             (let ([j 10])
                                               (+ j
                                                  (let ([k 11])
                                                    (+ k
                                                       (let ([l 12])
                                                         (+ l
                                                            (let ([m 13])
                                                              (+ l m))))))))))))))))))))))))))
