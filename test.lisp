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
(letrec 
    (fact) ((lambda (n)
        (if (eq? 0 n) 1 (* n (fact (- n 1))))
    ))
    (letrec 
        (sum) ((lambda (n) 
            (if (eq? 1 n) 1 (+ n (sum (- n 1))))
        ))
        (fact (sum 3))
    )
)
(letrec (seq) ((lambda (from to step f)
    (if (> from to)
        null
        (cons (f from) (seq (+ from step) to step f))
    )
))
(seq 1 10 2 (lambda (x) (* x 2)))
)
(letrec (seq) ((lambda (from to step f)
    (if (> from to)
        null
        (cons (f from) (seq (+ from step) to step f))
    )
))
(letrec (fact) ((lambda (n) (if (eq? n 0) 1 (* n (fact (- n 1))))))
    (seq 1 16 1 fact)
))
'(1 2 3)
(cons 3 '(1 (2 4) 3))
((lambda (l f) (f l)) '(1 2 3) (lambda (l) (cons 3 l)))
(cdr '(1 2 3 4))
(letrec 
    (fold) ((lambda (f acc l)
        (if (eq? l null) 
            acc
            (fold f (f (car l) acc) (cdr l))
        )
    ))
    (fold (lambda (x acc) (+ x acc)) 0 '(1 2 3))
)
(letrec 
    (fold) ((lambda (f acc l)
        (if (eq? l null) 
            acc
            (f (car l) (fold f acc (cdr l)))
        )
    ))
    (letrec
        (map) ((lambda (f l)
            (fold (lambda (x acc) (cons (f x) acc)) null (cons 100 l))
        ))
        (map (lambda (x) (* x x)) '(1 2 3))
    )
)
(letrec 
    (append) ((lambda (l e)
        (if (eq? l null)
            (cons e null)
            (cons (car l) (append (cdr l) e))
        )
    ))
    (append '(1 2 3) 4)
)
(letrec 
    (foo) ((lambda (n)
        (+ n 2)
    ))
    (letrec 
        (bar) ((lambda (n) 
            (foo (- n 1))
        ))
        (bar (foo 3))
    )
)