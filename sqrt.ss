;牛顿法平方根
(x/guess + guess)/2

(define (sqrt n)
  (sqrt-iter 1.0 n))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average a b)
  (/ (+ a b) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (square n)
  (* n n))
(define (even? n)
  (= (remainder n 2) 0))

;绝对值
(define (abs n)
  (if (< n 0)
      (- n)
      n))
