; n的幂次方
(define (fast-expt a n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt a (/ n 2))))
        (else (* a (fast-expt a (- n 1))))))

(define (even? a)
  (= (remainder a 2) 0))

(define (square n)
  (* n n))
