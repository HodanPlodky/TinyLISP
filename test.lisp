(+ 2 3)
(- 2 3)
(* (+ 2 3) 4)
(+ (if (- 0 0) (- 2 2) (+ 2 3)) 1)
(+ (if (eq? 50 50) (+ 2 2) (+ 3 3)) 1)
(if (eq? 40 50) (cons 5 (cons 10 null)) (+ 2 3))
(if (eq? 60 50) (cons 5 (cons 10 null)) (+ 2 3))
(if (eq? 50 50) (cons 5 (cons 10 null)) (+ 2 3))


(if (eq? 40 50) 
    (cons 5 (cons 10 null)) 
    (+ 2 3))

(if (eq? 60 50) 
    (cons 5 (cons 10 null)) 
    (+ 2 3))

(cons ( cons 123 null)
    (if (eq? 50 50) 
        (cons 5 (cons 10 null)) 
        null))
(cons 123
    (if (eq? 50 50) 
        (cons 5 (cons 10 null)) 
        null))
(cons 123
    (if (eq? 40 50) 
        (cons 5 (cons 10 null)) 
        (cons (cons 1 (cons 2 null)) (cons 3 null))))
(+ 0 1)

( + 1 ((lambda (x y) (+ x y)) 10 20))
(* 2 ((lambda (x y) (if (> x y) x y)) 20 32))

(+ 2 4)
null