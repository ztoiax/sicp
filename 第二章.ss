(define list1 (list 1 2 3 4))
(define list2 (list 0 1 2 3 4))
(define list3 (list list1 list2))
(define list4 (cons list1 list2))

;检索值
(define (lists-ref items n)
  (if (= n 0)
      (car items)
      (lists-ref (cdr items) (- n 1))))

;列表个数
    ;线性递归
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
    ;迭代
(define (length2 items)
  (define (iter items n)
    (if (null? items)
        n
        (iter (cdr items) (+ n 1))))
  (iter items 0))

;append
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


;最后一个
(define (last items)
  (let ((rest (cdr items)))
  (if (null? rest)
      (car items)
      (last rest))))

(define (last2 items)
  (if (null? (cdr items))
      (car items)
      (last2 (cdr items))))

(define(lastPair L) 
  (if (= (length L) 1)
      (car L) 
      (lastPair (cdr L)))) 

;reverse  
 (define nil '()) 
  
 (define (reverse items) 
   (if (null? (cdr items)) 
       items 
       (append (reverse (cdr items)) 
               (cons (car items) nil)))) 
    
 (define (reverse items) 
   (define (iter items result) 
     (if (null? items) 
         result 
         (iter (cdr items) (cons (car items) result)))) 
   (iter items nil))

;单数，双数
;useage (same-parity 2 3 4 4 5 5 7)

(define (same first . items)
  (define (iter items dist val)
    (if (null? items)
        dist
        (same (cdr items)
              (if (= (remainder (car items) 2) val)
                  (append dist (list (car items)))
                  dist)
              val)))
  (iter items (list first) (remainder (car items))))

;map
;useage (map (lambda (x) (* x x x)) items)
;       (map abs items)
(define (square x) (* x x))

(define (map f items)
  (if (null? items)
      nil
      (cons (f (car items)) (map f (cdr items)))))
  
(define (map-tree f tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (f tree)) 
        (else (cons (map-tree f (car tree))
                    (map-tree f (cdr tree))))))

(define (map-tree2 f tree)
  (map (lambda (tree) 
         (if (pair? tree)
             (map-tree2 f tree)
             (f tree)))
       tree))

 (define (filter f items)
    (cond ((null? items) nil)
          ((f (car items)) 
           (cons (car items) (filter f (cdr items))))
          (else (filter f (cdr items)))))

 (define (filter-tree f tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (if (f tree) tree))
          (else (cons (filter-tree f (car tree)) (filter-tree f (cdr tree))))))

(define (square-list items) 
    (define (iter l n) 
        (define r (square (car l))) 
        (if (null? (cdr l)) 
            (n (list r)) 
            (iter (cdr l) (lambda (x) (n (cons r x)))))) 
    (iter items (lambda (x) x))) 
;um
;userage (sum (lambda (x) (car x)) list1)

(define (sum f items)
  (if (null? items)
      0
      (+ (f items) 
          (sum f (cdr items)))))

(define (sum-tree f tree)
  (define (iter f tree result)
  (if (null? tree) 
      result
      (let ((first (car tree))) 
      (iter f (cdr tree) (if (not (pair? first))
                             (+ result (f first))
                             (iter f (car tree) result))))))
  (iter f tree 0))

;树统计个数
(define (count tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count (car tree))
                 (count (cdr tree))))))

;reverse-tree
    ;只反转树叶
(define (re tree)
    (if (null? tree)
        nil
        (let ((first (car tree))) 
        (if (not (pair? first)) 
            (append (re (cdr tree)) 
                    (cons first nil)) 
            (cons (re first) (re (cdr tree)))))))
    ;只反转树枝
(define (re2 tree)
    (if (null? tree)
        nil
        (let ((first (car tree))) 
        (if (not (pair? first)) 
            (cons first (re2 (cdr tree)))
            (append (re2 (cdr tree)) 
                    (cons first nil))))))

    ;全部反转
 (define (reverse-tree tree) 
   (define (iter tree result) 
     (if (null? tree) 
         result 
         (let ((first (car tree))) 
           (iter (cdr tree) 
                    (cons (if (not (pair? first)) 
                              first 
                              (reverse-tree first)) 
                          result))))) 
   (iter tree nil)) 
;列出所有树叶
(define (show tree)
  (define (iter tree result)
    (cond ((null? tree) result)
          ((not (pair? tree)) (cons tree result))
          (else (iter (car tree)
                       (iter (cdr tree) result)))))
  (iter tree nil))

(define (show tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (show (car tree))
                      (show (cdr tree))))))
;

;;;;;;;;;;;;;;;;;

(define (even? a n)
  (= (remainder a n) 0))

(define (find a n)
  (cond ((> (square n) a) a)
        ((even? a n) n)
        (else (find a (+ n 1)))))

(define (small n)
  (find n 2))

(define (prime? n)
  (= (small n) n))

(define (prime tree)
  (prime? (+ (car tree) (cadr tree))))
;;;;;;;;;;;;;;;;;
(define (flatmap proc seq)
  (acc append nil (map proc seq)))

(define (acc op n tree)
  (if (null? tree)
      n
      (op (car tree)
          (acc op n (cdr tree)))))

(define (interval low high)
  (if (> low high)
      nil
      (cons low (interval (+ low 1) high))))

(define (make tree)
  (list (car tree) (cadr tree) (+ (car tree) (cadr tree))))

(define (prime-pair n)
  (map make
       (filter prime
               (flatmap
                 (lambda (i)
                   (map (lambda (j) (list i j))
                        (interval 1 (- i 1))))
                 (interval 1 n)))))

(define (prime-pair n)
  (map make (filter prime
               (acc append nil
                    (map (lambda (i) (map (lambda (j) (list i j))
                                          (interval 1 (- i 1))))
                         (interval 1 n))))))

;列出所有排序可能
;useage （permutations list1)

(define (permutations items)
  (if (null? items)
      (list nil)
      (flatmap (lambda (x) 
                 (map (lambda (p) (cons x p))
                      (permutations (remove x items))))
               items)))

(define (remove items tree)
  (filter (lambda (x) (not (= x items)))
          tree))

(define (permutations items)
  (if (null? items)
      (list nil)
      (acc append nil (map (lambda (x) (map (lambda (p) (cons x p))
                      (permutations (remove x items)))) items))))

(define (map f items)
  (if (null? items)
      nil
      (cons (f (car items)) (map f (cdr items)))))
