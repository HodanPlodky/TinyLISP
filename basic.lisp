(letrec 
    (fact) ((lambda (n)
        (if (eq? 0 n) 1 (* n (fact (- n 1))))
    ))
    (fact 10)
)