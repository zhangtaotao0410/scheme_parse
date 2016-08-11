(define atom?
    (lambda (x)
      (and (not (pair? x)) (not (null? x)))))
(define lat?
    (lambda (l)
      (cond ((null? l) #t)
            (else (and (atom? (car l)) (lat? (cdr l)))))))
(define member?
    (lambda (a lat)
      (cond ((null? lat) #f)
            (else (or (eq? a (car lat)) (member? a (cdr lat)))))))
;删除lat中的第一个和参数a相同的原子
(define rember
    (lambda (a lat)
      (cond ((null? lat) (quote ()))
            ((eq? a (car lat)) (cdr lat))
            (else (cons (car lat) (rember a (cdr lat)))))))
(define firsts
    (lambda (l)
      (cond ((null? l) (quote ()))
            (else (cons (car (car l)) (firsts (cdr l)))))))
(define insertR
    (lambda (new old lat)
      (cond ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons old (cons new (cdr lat))))
            (else (cons (car lat) (insertR new old (cdr lat)))))))
(define insertL
    (lambda (new old lat)
      (cond ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons new lat))
            (else (cons (car lat) (insertL new old (cdr lat)))))))
;;把old替换为new
(define subst
    (lambda (new old lat)
      (cond ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons new (cdr lat)))
            (else (cons (car lat) (subst new old (cdr lat)))))))
;;把lat中所有的a删除
(define multirember
    (lambda (a lat)
      (cond ((null? lat) (quote ()))
            ((eq? a (car lat)) (multirember a (cdr lat)))
            (else (cons (car lat) (multirember a (cdr lat)))))))
(define multiinsertR
    (lambda (new old lat)
      (cond ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
            (else (cons (car lat) (multiinsertR new old (cdr lat)))))))
(define multiinsertL
    (lambda (new old lat)
      (cond ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons new (cons (car lat) (multiinsertL new old (cdr lat)))))
            (else (cons (car lat) (multiinsertL new old (cdr lat)))))))
(define multisubst
    (lambda (new old lat)
      (cond ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
            (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define add1
    (lambda (n)
      (+ n 1)))
(define sub1
    (lambda (n)
      (- n 1)))
(define o+
    (lambda (n m)
      (cond ((zero? m) n)
            (else (add1 (o+ n (sub1 m)))))))
(define o-
    (lambda (n m)
      (cond ((zero? m) n)
            (else (sub1 (o- n (sub1 m)))))))
(define addtup
    (lambda (lat)
      (cond ((null? lat) 0)
            (else (o+ (car lat) (addtup (cdr lat)))))))
(define x
    (lambda (n m)
      (cond ((zero? m) 0)
            (else (o+ n (x n (sub1 m)))))))
;(define tup+
;    (lambda (tup1 tup2)
;      (cond ((null? tup1) (quote ()))
;            (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
(define tup+
    (lambda (tup1 tup2)
      (cond ((null? tup1) tup2)
            ((null? tup2) tup1)
            (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define >
    (lambda (n m)
      (cond ((zero? n) #f)
            ((zero? m) #t)
            (else (> (sub1 n) (sub1 m))))))

(define <
    (lambda (n m)
      (cond ((zero? m) #f)
            ((zero? n) #t)
            (else (< (sub1 n) (sub1 m))))))
(define =
    (lambda (n m)
      (cond ((zero? m) (zero? n))
            ((zero? n) #f)
            (else (= (sub1 n) (sub1 m))))))
(define ^
    (lambda (n m)
      (cond ((zero? m) 1)
            (else (x n (^ n (sub1 m)))))))
(define /
    (lambda (n m)
      (cond ((< n m) 0)
            (else (add1 (/ (o- n m) m))))))
(define length
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l)))))))
(define pick
    (lambda (n lat)
      (cond ((zero? (sub1 n)) (car lat))
            (else (pick (sub1 n) (cdr lat))))))
(define rempick
    (lambda (n lat)
      (cond ((zero? (sub1 n)) (cdr lat))
            (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
(define no-nums
    (lambda (lat)
      (cond ((null? lat) (quote ()))
            ((number? (car lat)) (no-nums (cdr lat)))
            (else (cons (car lat) (no-nums (cdr lat)))))))
(define all-nums
    (lambda (lat)
      (cond ((null? lat) (quote ()))
            ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
            (else (all-nums (cdr lat))))))
;写一个函数eqan?，当两个参数a1和a2是一样的原子时为真。
(define eqan?
    (lambda (a1 a2)
      (cond ((and (number? a1) (number? a2)) (= a1 a2))
            ((or (number? a1) (number? a2)) #f)
            (else (eq? a1 a2)))))
;写一个函数occur描述一个lat中出现原子a的次数
(define occur
    (lambda (a lat)
      (cond ((null? lat) 0)
            ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
            (else (occur a (cdr lat))))))
(define one?
    (lambda (n)
      (= n 1)))
(define rember*
    (lambda (a l)
      (cond ((null? l) (quote ()))
            ((atom? (car l))
              (cond ((eqan? a (car l)) (rember* a (cdr l)))
                    (else (cons (car l) (rember* a (cdr l))))))
            (else (cons (rember* a (car l)) (rember* a (cdr l)))))))
(define insertR*
    (lambda (new old l)
      (cond ((null? l) (quote ()))
            ((atom? (car l))
              (cond ((eqan? old (car l)) (cons (car l) (cons new (insertR* new old (cdr l)))))
                    (else (cons (car l) (insertR* new old (cdr l))))))
            (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))
(define occur*
    (lambda (a l)
      (cond ((null? l) 0)
            ((atom? (car l))
              (cond ((eqan? a (car l)) (add1 (occur* a (cdr l))))
                    (else (occur* a (cdr l)))))
            (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))
(define subst*
    (lambda (new old l)
      (cond ((null? l) (quote ()))
            ((atom? (car l))
              (cond ((eqan? old (car l)) (cons new (subst* new old (cdr l))))
                    (else (cons (car l) (subst* new old (cdr l))))))
            (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))
(define insertL*
    (lambda (new old l)
      (cond ((null? l) (quote ()))
            ((atom? (car l))
              (cond ((eqan? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
                    (else (cons (car l) (insertL* new old (cdr l))))))
            (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))
(define member*
    (lambda (a l)
      (cond ((null? l) #f)
            ((atom? (car l)) (or (eqan? a (car l)) (member* a (cdr l))))
            (else (or (member* a (car l)) (member* a (cdr l)))))))
(define leftmost
    (lambda (l)
      (cond ((atom? (car l)) (car l))
            (else (leftmost (car l))))))
(define eqlist?
    (lambda (l1 l2)
      (cond ((and (null? l1) (null? l2)) #t)
            ((or (null? l1) (null? l2)) #f)
            (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))
(define equal?
    (lambda (s1 s2)
      (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
            ((or (atom? s1) (atom? s2)) #f)
            (else (eqlist? s1 s2)))))
(define numbered?
    (lambda (aexp)
      (cond ((atom? aexp) (number? aexp))
            ((eq? (quote +) (car (cdr aexp))) (and (numbered? (car aexp)) (numbered? (car (car (cdr aexp))))))
            ((eq? (quote ^) (car (cdr aexp))) (and (numbered? (car aexp)) (numbered? (car (car (cdr aexp))))))
            ((eq? (quote x) (car (cdr aexp))) (and (numbered? (car aexp)) (numbered? (car (car (cdr aexp))))))
            (else #f))))
(define 1st-sub-exp
    (lambda (nexp)
      (car (cdr nexp))))
(define 2st-sub-exp
    (lambda (nexp)
      (car (cdr (cdr nexp)))))
(define operator
    (lambda (nexp)
      (car nexp)))
(define value
    (lambda (nexp)
      (cond ((eq? (quote o+) (operator nexp)) (o+ (1st-sub-exp nexp) (2st-sub-exp nexp)))
            ((eq? (quote x) (operator nexp)) (x (1st-sub-exp nexp) (2st-sub-exp nexp)))
            ((eq? (quote ^) (operator nexp)) (^ (1st-sub-exp nexp) (2st-sub-exp nexp))))))
(define set?
    (lambda (lat)
      (cond ((null? lat) #t)
            ((member? (car lat) (cdr lat)) #f)
            (else (set? (cdr lat))))))
(define makeset
    (lambda (lat)
      (cond ((null? lat) (quote ()))
            (else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))
(define subset?
    (lambda (set1 set2)
      (cond ((null? set1) #t)
            (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))
(define eqset?
    (lambda (set1 set2)
      (and (subset? set1 set2) (subset? set2 set1))))
;交集
(define intersect?
    (lambda (set1 set2)
      (cond ((null? set1) #f)
            (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))
(define intersect
    (lambda (set1 set2)
      (cond ((null? set1) (quote ()))
            ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
            (else (intersect (cdr set1) set2)))))

;把俩个list合成一个
(define unionlist
    (lambda (lat1 lat2)
      (cond ((null? lat1) lat2)
            (else (cons (car lat1) (unionlist (cdr lat1) lat2))))))

;(define union
;    (lambda (set1 set2)
;      (cond ((null? set1) set2)
;            ((member? (car set1) set2) (union (cdr set1) set2))
;            (else (cons (car set1) (union (cdr set1) set2))))))
(define union
    (lambda (set1 set2)
      (cond ((null? set1) set2)
            (else (makeset (unionlist set1 set2))))))

;它返回在l-set的所有子set中都有的原子。
(define intersectall
    (lambda (l-set)
      (cond ((null? (cdr l-set)) (car l-set))
            (else (intersect (car l-set) (intersectall (cdr l-set)))))))
(define a-pair?
    (lambda (x)
      (cond ((null? x) #f)
            ((atom? x) #f)
            ((null? (cdr x)) #f)
            ((null? (cdr (cdr x))) #t)
            (else #f))))
(define first
    (lambda (p)
      (car p)))
(define second
    (lambda (p)
      (car (cdr p))))
(define build
    (lambda (s1 s2)
      (cons s1 (cons s2 (quote ())))))
;rel 是一个内部嵌套pair的list, 但是其所有子pair是唯一的  相当于一个pair集合
;fun? 基本同rel, 但其所有子pair的第一个元素也是唯一的
(define fun?
    (lambda (rel)
      (set? (firsts rel))))
;;revrel 将rel中所有子pair的两个元素对调
;(define revrel
;    (lambda (rel)
;      (cond ((null? rel) (quote ()))
;            (else (cons (build (second (car rel)) (first (car rel)))
;                        (revrel (cdr rel)))))))
(define revpair
    (lambda (pair)
      (build (second pair) (first pair))))
(define revrel
    (lambda (rel)
      (cond ((null? rel) (quote ()))
            (else (cons (revpair (car rel)) (revrel (cdr rel)))))))
(define seconds
    (lambda (l)
      (cond ((null? l) (quote ()))
            (else (cons (second (car l)) (seconds (cdr l)))))))
(define fullfun?
    (lambda (fun)
      (set? (seconds fun))))
;(define rember-f
;    (lambda (test? a l)
;      (cond ((null? l) (quote ()))
;            ((test? a (car l)) (cdr l))
;            (else (cons (car l) (rember-f test? a (cdr l)))))))
(define rember-f
    (lambda (test?)
      (lambda (a l)
        (cond ((null? l) (quote ()))
              ((test? a (car l)) (cdr l))
              (else (cons (car l) ((rember-f test?) a (cdr l))))))))
(define insertL-f
    (lambda (test?)
      (lambda (new old l)
        (cond ((null? l) (quote ()))
              ((test? old (car l)) (cons new (cons old ((insertL-f test?) new old (cdr l)))))
              (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))
(define insertR-f
    (lambda (test?)
      (lambda (new old l)
        (cond ((null? l) (quote ()))
              ((test? old (car l)) (cons old (cons new (cdr l))))
              (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))
(define seqL
    (lambda (new old l)
      (cons new (cons old l))))
(define insert-g
    (lambda (seq)
      (lambda (new old l)
        (cond ((null? l) (quote ()))
              ((eqan? old (car l)) (seq new old ((insertL-g seq) new old (cdr l))))
              (else (cons (car l) ((insertL-g seq) new old (cdr l))))))))
(define subst-g (insert-g (lambda (new old l)
                                    (cons new l))))

(define atom-to-function
    (lambda (a)
      (cond ((eq? a (quote o+)) o+)
            ((eq? a (quote x)) x)
            (else ^))))

(define value-f
    (lambda (nexp)
      (cond ((atom? nexp) nexp)
            (else ((atom-to-function (operator nexp)) (1st-sub-exp nexp) (2st-sub-exp nexp))))))
(define eq-C?
    (lambda (a)
      (lambda (x)
        (eq? a x))))


(define multiremberT
    (lambda (test? l)
      (cond ((null? l) (quote ()))
            ((test? (car l)) (multiremberT test? (cdr l)))
            (else (cons (car l) (multiremberT test? (cdr l)))))))
(define multirember&co
    (lambda (a lat col)
      (cond ((null? lat) (col (quote ()) (quote ())))
            ((eq? a (car lat)) (multirember&co a (cdr lat) (lambda (newlat seen)
                                                                    (col newlat (cons (car lat) seen)))))
            (else (multirember&co a (cdr lat) (lambda (newlat seen)
                                                      (col (cons (car lat) newlat) seen)))))))
(define col
  (lambda (l p s)
    (display (cons s (cons p l)))
    (newline)
    l))

(define even?
    (lambda (n)
      (= n (x 2 (/ n 2)))))
;函数 evens-only*。它删除嵌套list中所有的奇数。
(define evens-only*&co
    (lambda (l col)
      (cond ((null? l) (col (quote ()) 1 0))
            ((atom? (car l))
                (cond ((even? (car l)) (evens-only*&co (cdr l) (lambda (newlat p s)
                                                                        (col (cons (car l) newlat) (* (car l) P) s))))
                      (else (evens-only*&co (cdr l) (lambda (newlat p s)
                                                            (col newlat p (o+ (car l) s)))))))
            (else (evens-only*&co (car l) (lambda (al ap as)
                                                  (evens-only*&co (cdr l) (lambda (bl bp bs)

                                                                                  (col (cons al bl) (* ap bp) (o+ as bs))))))))))

((lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))) eternity)

  ((lambda (mk-length)
    (mk-length eternity))
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))

      ((lambda (mk-length)
       (mk-length mk-length))
      (lambda (mk-length)
         (lambda (l)
           (cond
             ((null? l) 0)
             (else (add1
                     ((mk-length eternity) (cdr l))))))))

     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1
                   (    ((lambda (mk-length)
                          (lambda (l)
                            (cond
                                ((null? l) 0)
                                  (else (add1
                                      ((mk-length mk-length) (cdr l)))))))

                                  eternity) (cdr l))))))




(
    (lambda (mk-length)
       (mk-length mk-length))

    (lambda (mk-length)

         ((lambda (length)
            (lambda (l)
              (cond
                ((null? l) 0)
                (else (add1 (length (cdr l)))))))

                (mk-length mk-length)))
                                                    )

        (define mk-length
          (lambda (length-mk)
            ((lambda (length)
              (lambda (l)
                (cond ((null? l) 0)
                  (else (+ 1 (length (cdr l)))))))
             (lambda (x)
               ((length-mk length-mk) x)))    )    )



           (define mk-length
             (lambda (length-mk)
               ((lambda (length)
                  (length (lambda (x)
                    ((length-mk length-mk) x))))
                (lambda (length)
                  (lambda (l)
                    (cond ((null? l) 0)
                          (else (+ 1 (length (cdr l)))))))))    )

        (define Y
          (lambda (f)
            (f f)))

    (define Y
      (lambda (length)
        ((lambda (f)
           (f f))
         (lambda (length-mk)
           (length (lambda (x)
             ((length-mk length-mk) x)))))))














(evens-only*&co (list (list 1) 2) col)
