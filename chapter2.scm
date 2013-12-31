;; lat? - list of atom?

(define (lat? l)
  (cond ((null? l) #t)
        ((atom? (car l)) (lat? (cdr l)))
        (else #f)))

(define (lat? l)
  (if (null? l) #t
      (and (atom? (car l))
           (lat? (cdr l)))))


(define (member? a lat)
  (if (null? lat) #f
      (or (eq? a (car lat))
          (member? a (cdr lat)))))
