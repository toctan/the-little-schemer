(define (set? lat)
  (cond ((null? lat) #t)
        ((member? (car lat) (cdr lat)) #f)
        (else (set? (cdr lat)))))


(define (makeset lat)
  (cond ((null? lat) '())
        ((member? (car lat) (cdr lat))
         (makeset (cdr lat)))
        (else (cons (car lat)
                    (makeset (cdr lat))))))

(define (makeset lat)
  (if (null? lat) '()
      (cons (car lat)
            (makeset
             (multirember (car lat) (cdr lat))))))


(define (subset? set1 set2)
  (if (null? set1) #t
      (and (member? (car set1) set2)
           (subset? (cdr set1) set2))))


(define (eqset? set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1)))


(define (intersect? set1 set2)
  (if (null? set1) '()
      (or (member? (car set1) set2)
          (intersect? (cdr set1) set2))))


(define (intersect set1 set2)
  (cond ((null? set1) '())
        ((member? (car set1) set2)
         (cons (car set1)
               (intersect (cdr set1) set2)))
        (else (intersect (cdr set1) set2))))


(define (union set1 set2)
  (cond ((null? set1) set2)
        ((member? (car set1) set2)
         (union (cdr set1) set2))
        (else (cons (car set1)
                    (union (cdr set1) set2)))))


(define (intersectall l-set)
  (if (null? (cdr l-set)) (car l-set)
      (intersect (car l-set)
                 (intersectall (cdr l-set)))))





(define (a-pair? x)
  (cond ((atom? x) #f)
        ((null? x) #f)
        ((null? (cdr x)) #f)
        ((null? (cddr x)) #t)
        (else #f)))


(define (first p) (car p))
(define (second p) (cadr p))
(define (third p) (caddr p))

(define (build s1 s2)
  (cons s1 (cons s2 '())))


(define (fun? rel)
  (set? (firsts rel)))

(define (fullfun? fun)
  (set? (seconds fun)))

(define (one-to-one? fun)
  (fun? (revrel fun)))


(define (revrel rel)
  (if (null? rel) '()
      (cons (build (second (car rel))
                   (first (car rel)))
            (revrel (cdr rel)))))

(define (revpair p)
  (build (second p) (first p)))

(define (revrel rel)
  (if (null? rel) '()
      (cons (revpair (car rel))
            (revrel (cdr rel)))))
