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
((lambda (l e) (cons e l)) (cons 2 null) 3)

((lambda (pow x) 
    (if (eq? (pow x) 100) 
        (pow (pow (pow (/ x 2))))
        (pow (pow (pow x)))
    )
)(lambda (x) (* x x)) 10)
(
    ((lambda (a b) 
        (lambda (f) 
            (f (- a b)))
    ) 20 30) (lambda (x) (* x 2))
)

(+ 2 4)
null
(letrec 
    (fact) ((lambda (n)
        (if (eq? 0 n) 1 (* n (fact (- n 1))))
    ))
    (fact 5)
)