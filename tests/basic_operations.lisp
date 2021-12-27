(letrec 
    (fold) ((lambda (f acc l)
        (if (eq? l null) 
            acc
            (f (car l) (fold f acc (cdr l)))
        )
    ))
(letrec
    (map) ((lambda (f l)
        (fold (lambda (x acc) (cons (f x) acc)) null l)
    ))
(letrec
    (seq) ((lambda (from to step)
        (if (> from to)
            null
            (cons from (seq (+ from step) to step))
        )
    ))
(letrec
    (sum) ((lambda (l)
        (fold (lambda (x acc) (+ x acc)) 0 l)
    ))
(letrec
    (len) ((lambda (l) 
        (fold (lambda (x acc) (+ 1 acc)) 0 l)
    ))
(letrec
    (append) ((lambda (l e)
        (fold (lambda (x acc) (cons x acc)) (cons e null) l)
    ))
 
(map (lambda (x) (* x x))
    (append (seq 1 10 2) 123))
))))))