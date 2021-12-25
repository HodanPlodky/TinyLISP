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