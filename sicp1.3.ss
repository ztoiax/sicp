; 选出x y z中最大的数
 (define (bx x y) (if (> x y)
                       x
                       y))

 (define (big x y z) (if (> (bx x y) z)
                          (bx x y)
                          z))

