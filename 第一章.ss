; fib
(define (fib n) (cond ((= n 0) 0) ((= n 1) 1)
        (else (+ (fib (- n 1)) 
                 (fib (- n 2))))))


(define (fib-iter a b n)
  (cond ((= n 0) b)
        (else (fib-iter (+ a b) a (- n 1)))))

(define (fib2 n)
  (fib-iter 1 0 n))
(define (fib-iter a b n)
  (if(= n 0)
      b
      (fib-iter (+ a b) a (- n 1))))

;pascal 

(define (pascal a b)
  (cond ((= b 1) 1)
        ((= a b) 1)
        (else (+ (pascal (- a 1) (- b 1))
              (pascal (- a 1) b)))))

;幂次方
(define (square n)
  (* n n))
(define (even? n)
  (= (remainder n 2) 0))

(define (exp a n)
  (cond ((= n 0) 1)
        ((even? n) (square (exp a (/ n 2))))
        (else (* a (exp a (- n 1))))))

;最大公约数

(define (gcd a b)
  (if (= b 0) 
      a 
      (gcd b (remainder a b))))

;prime

(define (prime? n)
  (= (small n) n))

(define (small n)
  (find n 2))

(define (even? a n)
  (= (remainder a n) 0))

(define (find a n)
  (cond ((> (square n) a) a)
        ((even? a n) n)
        (else (find a (+ n 1)))))

;费马小定律
(define (even? n)
  (= (remainder n 2) 0))

(define (expmod a b n)
  (cond ((= b 0) 1)
        (else (remainder (* a (expmod a (- b 1) n)) n))))

(define (test n)
  (define (try a)
    (= (expmod a n n) a))
  (try (+ 1 (random (- n 1)))))

(define (prime2? n times)
  (cond ((= times 0) 0)
        ((test n) (prime2? n (- times 1)))
        (else 1)))

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
;(pi 1 1000)
(define (pi a b)
  (* 8 (pi-sum a b)))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

;a,b间的积分
(define (integral f a b dx)
  (* (sum f 
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b
          )
     dx))
(define (cude x)
  (* x x x))

;绝对值
(define (abs n)
  (if (< n 0)
      (- n)
      n))

;平方根
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

;零点
(define (search f a b)
  (let ((mid (average a b)))
    (if (close-enough? a b)
        mid
        (let ((test (f mid)))
          (cond ((positive? test)
                    (search f a mid))
                ((negative? test)
                    (search f mid b))
                (else mid))))))

(define (close-enough? a b)
  (< (abs (- a b)) 0.001))

(define (half f n p)
  (let ((a (f n))
       (b (f p)))
  (cond ((and (negative? a) (positive? b))
        (search f a b))
        ((and (negative? b) (positive? a))
        (search f b a))
        (else
          (error "not of opposite sign" a b)))))

;不动点
(define (fixd f guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try guess))

;不动点2
(define (try f x)
  (let ((next (f x)))
    (if (close? x next)
        next
        (try f next))))

(define (close? x y)
  (< (abs (- x y)) 0.0001))

;x + x^2的平均值
(define (average-damp f)
  (lambda (x) (average x (f x))))

;不动点求平方根
(define (sqrt2 x)
  (fixd (average-damp (lambda (y) (/ x y)))
  1.0))

(define (sqrt3 x)
  (fixd (lambda (y) (average y (/ x y))) 1.0))
;立方根
(define (cude-root x)
  (fixd (lambda (y) (average y (/ x (square y)))) 1.0))

;序对
(define (make n p)
  (cons n p))
(define (next x )(car x))
(define (prev x )(cdr x))

(define (print x)
  (newline)
  (display (next x))
  (display "/")
  (display (prev x)))

;log
(define (gen a b c n)
  (cond ((= a c) n)
        ((> c a) n c)
        (else (gen a b (double c b) (+ n 1)))))

(define (double c b)
  (* c b))

(define (log a b)
  (gen a b 1 0))

