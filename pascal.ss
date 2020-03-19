; 帕斯卡三角形
; 1 
; 1  1 
; 1  2  1 
; 1  3  3  1 
; 1  4  6  4  1 
; 1  5  10 10 5  1 
(define (pascal a b)
  (cond ((or (= b 1) (= a b)) 1)
        (else (+ (pascal (- a 1) (- b 1)) (pascal (- a 1) b)))))
