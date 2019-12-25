; 用法:(new-if (= 2 2) 0 1)
(define (new-if p x y) (cond (p x) (else y)))
(define (new-if2 p x y) (cond ((p x y) 0) (else 1)))