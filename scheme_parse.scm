(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))
(define add1
    (lambda (n)
        (+ n 1)))
(define sub1
    (lambda (n)
        (- n 1)))
(define build
    (lambda (s1 s2)
        (cons s1 (cons s2 (quote ())))))
(define expression-to-action
    (lambda (e)
        (cond ((atom? e) (atom-to-action e))
              (else (list-to-action e)))))
(define extend-table cons)
(define new-entry build)
(define atom-to-action
    (lambda (e)
        (cond ((number? e) *const)
              ((eq? e #t) *const)
              ((eq? e #f) *const)
              ((eq? e (quote cons)) *const)
              ((eq? e (quote car)) *const)
              ((eq? e (quote cdr)) *const)
              ((eq? e (quote atom?) *const))
              ((eq? e (quote null?)) *cosnt)
              ((eq? e (quote eq?)) *const)
              ((eq? e (quote zero?)) *const)
              ((eq? e (quote add1)) *const)
              ((eq? e (quote sub1)) *const)
              ((eq? e (quote number?)) *const)
              (else *identifier))))

(define list-to-action
    (lambda (e)
        (cond ((atom? (car e))
                  (cond ((eq? (quote lambda) (car e)) *lambda)
                        ((eq? (quote cond) (car e)) *cond)
                        ((eq? (quote quote)) *quote)
                        (else *application)))
               (else *application))))


(define value
    (lambda (e)
      (meaning e (quote ()))))

(define meaning
    (lambda (e table)
        ((expression-to-action e) e table)))

(define *const
    (lambda (e table)
        (cond ((number? e) e)
              ((eq? e #t) #t)
              ((eq? e #f) #f)
              (else (build (quote primitive) e)))))

(define *identifier
    (lambda (e table)
        (lookup-in-table e table initial-table)))

(define initial-table
    (lambda (name)
        (car (quote ()))))
(define lookup-in-table
    (lambda (e table table-f)
        (cond ((null? table) (table-f e))
              (else (lookup-in-entry e (car table) (lambda (name)
                                                          (lookup-in-table name (cdr table) table-f)))))))
(define lookup-in-entry
    (lambda (name entry entry-f)
        (lookup-in-entry-help name (first entry) (second entry) entry-f)))
(define lookup-in-entry-help
    (lambda (name names values entry-f)
        (cond ((null? names) (entry-f name))
              ((eq? name (car names)) (car values))
              (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))
(define first
    (lambda (e)
        (car e)))
(define second
    (lambda (e)
        (car (cdr e))))
(define third
    (lambda (e)
        (car (cdr (cdr e)))))
(define *quote
    (lambda (e table)
        (second e)))

(define evcon
    (lambda (lines table)
        (cond ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
              ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
              (else (evcon (cdr lines) table)))))
(define else?
    (lambda (e)
        (cond ((atom? e) (eq? e (quote else)))
              (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
    (lambda (e table)
        (evcon (cdr e) table)))
(define function-of first)
(define arguments-of second)
(define function-of car)
(define arguments-of cdr)
(define evlis
    (lambda (args table)
        (cond ((null? args) (quote ()))
              (else (cons (meaning (car args) table) (evlis (cdr args) table))))))
(define *application
    (lambda (e table)
        (apply (meaning (function-of e) table)
               (evlis (arguments-of e) table))))

(define apply
    (lambda (fun vals)
        (cond ((primitive? fun) (apply-primitive (second fun) vals))
              ((non-primitive? fun) (apply-closure (second fun) vals)))))
(define primitive?
    (lambda (l)
        (eq? (first l) (quote primitive))))
(define non-primitive?
    (lambda (l)
        (eq? (first l) (quote non-primitive))))
(define apply-primitive
    (lambda (name vals)
        (cond ((eq? name (quote cons)) (cons (first vals) (second vals)))
              ((eq? name (quote car)) (car (first vals)))
              ((eq? name (quote cdr)) (cdr (first vals)))
              ((eq? name (quote zero?)) (zero? (first vals)))
              ((eq? name (quote add1)) (add1 (first vals)))
              ((eq? name (quote sub1)) (sub1 (first vals)))
              ((eq? name (quote null?)) (null? (first vals)))
              ((eq? name (quote atom?)) (atom? (first vals)))
              ((eq? name (quote number?)) (number? (first vals)))
              ((eq? name (quote eq?)) (eq? (first vals) (second vals))))))
(define apply-closure
    (lambda (closure vals)
        (meaning (body-of closure) (extend-table (new-entry (formals-of closure) vals) (table-of closure)))))


(value (((lambda (fun)
              (lambda (n)
                  (fun n)))
         (lambda (n)
              (zero? (add1 n)))) 3))
