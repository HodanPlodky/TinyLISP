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