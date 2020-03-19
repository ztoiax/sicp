(define (hannoi n i)
    (cond ((= n 0) i)
          (else (hannoi (- n 1) (+ i 1))
                (hannoi (- n 1) (+ i 1)))))

(define (hannoi n)
  (define (iter i n)
    (if (= n 1)
        i
        (iter (+ (* 2 i) 1) (- n 1))))
  (iter 1 n))
