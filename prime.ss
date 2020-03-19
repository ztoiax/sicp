; 素数
(define (find a n)
  (cond ((> (square n) a) a)
        ((even? a n) n)
        (else (find a (+ n 1)))))

(define (square n)
  (* n n))

(define (even? a n)
  (= (remainder a n) 0))

(define (prime? n)
  (= n (small n)))

(define (small n)
  (find n 2))

; 费马小定律
; 能判断一个数不是素数，概率性判断是素数
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
