(letrec 
    (find) ((lambda (tree e) 
        (if (= tree null)
            0
            (if (= (car tree) e)
                1
                (if (> (car tree) e)
                    (find (car (cdr tree)) e)
                    (find (car (cdr (cdr tree))) e)
                )
            )
        )
    ))
(letrec 
    (insert) ((lambda (tree e)
        (if (= tree null)
            (cons e '(null null))
            (if (= (car tree) e)
                tree
                (if (> (car tree) e)
                    (cons 
                        (car tree) 
                        (cons 
                            (insert (car (cdr tree)) e)
                            (cdr (cdr tree))))
                    (cons 
                        (car tree) 
                        (cons 
                            (car (cdr tree)) 
                            (insert (car (cdr (cdr tree))) e)))
                )
            )
        )
    ))
    (insert '(2 (1 null null) (3 null null)) 0)
))