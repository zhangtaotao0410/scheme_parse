
(define add1
    (lambda (n)
        (+ n 1)))

;(((lambda (mk-length)
;    (mk-length mk-length))
; (lambda (mk-length)
;    (lambda (l)
;        (cond ((null? l) 0)
;              (else (add1 ((mk-length mk-length) (cdr l)))))))) (list 1 2))
;(((lambda (mk-length)
;    (mk-length mk-length))
; (lambda (mk-length)
;    ((lambda (length)
;        (lambda (l)
;            (cond ((null? l) 0)
;                  (else (add1 (length (cdr l)))))))
;     (lambda (x)
;        ((mk-length mk-length) x))))) (list 1 2 3 4 5 6))

;(((lambda (le)
;    ((lambda (mk-length)
;        (mk-length mk-length))
;    (lambda (mk-length)
;        (le (lambda (x) ((mk-length mk-length) x))))))
;  (lambda (length)
;      (lambda (l)
;          (cond ((null? l) 0)
;                (else (add1 (length (cdr l)))))))) (list 1 2 3 4 5 6 7 8 9 90))


(define Y
    (lambda (le)
        ((lambda (f) (f f))
         (lambda (f)
            (le (lambda (x) ((f f) x)))))))

((Y (lambda (length)
           (lambda (l)
                   (cond ((null? l) 0)
                         (else (add1 (length (cdr l)))))))) (list 1 2 3 4 5 6 6))
