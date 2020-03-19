;sum

(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a) (sum f (next a) next b))))
;fib
(define (fib a b)
  (sum (lambda (x) x)
       a
       (lambda (x) (+ x 1))
       b))
;pi
(define (pi a b)
  (* 8 (pi-sum a b)))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

;0,1间积分 (integral cude 0 1 0.1)

(define (integral f a b dx)
  (* (sum f 
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b
          )
     dx))
(define (cude x)
  (* x x x))
