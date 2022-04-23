; Function with more than 6 arguments
(define (f [arg1 : Integer]
           [arg2 : Integer]
           [arg3 : Integer]
           [arg4 : Integer]
           [arg5 : Integer]
           [arg6 : Integer]
           [arg7 : Integer])
  :
  Integer
  (+ (- arg7 (+ arg1 arg6)) 42))

(f 1 2 3 4 5 6 7)
