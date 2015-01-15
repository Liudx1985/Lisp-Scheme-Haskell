(define (make-account blance password)

    (define (withdraw amount)
        (if (>= blance amount)
            (begin (set! blance (- blance amount))
                   blance)
            "Insufficient funds"))

    (define (check)
        blance)

    (define (deposit amount)
        (set! blance (+ blance amount)))

    (define (password-match? given-password)
            (eq? given-password password))

    (define (display-wrong-password-message)
        (display "Incorrect password"))

    (define (dispatch given-password mode)
        (if (password-match? given-password)
            (cond ((eq? mode 'withdraw)
                    withdraw)
                  ((eq? mode 'deposit)
                    deposit)
                  ((eq? mode 'check)
                    check)
                  (else
                    (error "Unknow request -- MAKE-ACCOUNT" mode)))
            display-wrong-password-message))

    dispatch)