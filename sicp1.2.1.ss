; *(1..6)
 (define (f x) (if (= x 1)
                    x
                    (* x (f (- x 1)))))

 (define (f2 x y z) (if (> y z)
                    x
                    (f2 (* y x) (+ y 1) z)))
 (define (f x) (f2 1 1 x))

