(define (rember-f test?)
  (lambda (a l)
    (cond ((null? l) '())
          ((test? a (car l)) (cdr l))
          (else (cons (car l)
                      (rember-f test? a (cdr l)))))))


(define (insertL-f test?)
  (lambda (new old l)
    (cond ((null? l) '())
          ((test? old (car l)) (cons new l))
          (else (cons (car l)
                      ((insertL-f test?) new old (cdr l)))))))

(define (insertR-f test?)
  (lambda (new old l)
    (cond ((null? l) '())
          ((test? old (car l)) (cons old (cons new (cdr l))))
          (else (cons (car l)
                      ((insertR-f test?) new old (cdr l)))))))


(define (insert-g seq)
  (lambda (new old l)
    (cond ((null? l) '())
          ((test? old (car l)) (seq new old (cdr l)))
          (else (cons (car l)
                      ((insert-g seq) new old (cdr l)))))))

(define (seqL new old l) (cons new (cons old l)))
(define (seqR new old l) (cons old (cons new l)))
(define (seqS new old l) (cons new l))
(define (seqrem new old l) l)

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))
(define subst (insert-g seqS))
(define (rember a l) ((insert-g seqrem) #f a l))


(define (atom-to-fuction x)
  (cond ((eq? x '+) +)
        ((eq? x '*) *)
        (else **)))

(define (value exp)
  (if (atom? exp) exp
      ((atom-to-fuction (operator exp))
       (value (1st-sub-exp exp))
       (value (2nd-sub-exp exp)))))


(define (multirember-f test?)
  (lambda (a lat)
    (cond ((null? lat) '())
          ((test? a (car lat))
           ((multirember-f test?) (cdr lat)))
          (else (cons (car lat)
                      ((multirember-f test?) a (cdr lat)))))))

(define multirember-eq? (multirember-f eq?))

(define (multiremberT test? lat)
  (cond ((null? lat) '())
        ((test? (car lat))
         (multiremberT test? (car lat)))
        (else (cons (car lat)
                    (multiremberT test? (cdr lat))))))


(define (multirember-co a lat col)
  (cond ((null? lat) (col '() '()))
        ((eq? a (car lat)
              (multirember-co a (cdr lat)
                              (lambda (newlat seen)
                                (col newlat
                                     (cons (car lat) seen))))))
        (else (multirember-co a (cdr lat)
                              (lambda (newlat seen)
                                (col (cons (car lat) newlat)
                                     seen))))))



(define (multiinsertLR new oldL oldR lat)
  (cond ((null? lat) '())
        ((eq? (car lat) oldL)
         (cons new
               (cons oldL
                     (multiinsertLR new oldL oldR (cdr lat)))))
        ((eq? (car lat) oldR)
         (cons oldR
               (cons new
                     (multiinsertLR new oldL oldR (cdr lat)))))
        (else (cons (car lat)
                    (multiinsertLR new oldL oldR (cdr lat))))))


(define (multiinsertLR-co new oldL oldR lat col)
  (cond ((null? lat) (col '() 0 0))
        ((eq? (car lat) oldL)
         (multiinsertLR-co new oldL oldR (cdr lat)
                           (lambda (newlat L R)
                             (col (cons new (cons oldL newlat)) (1+ L) R))))
        ((eq? (car lat) oldR)
         (multiinsertLR-co new oldL oldR (cdr lat)
                           (lambda (newlat L R)
                             (col (cons oldR (cons new newlat)) L (1+ R)))))
        (else (multiinsertLR-co new oldL oldR (cdr lat)
                                (lambda (newlat L R)
                                  (col (cons (car lat) newlat) L R))))))



(define (evens-only* l)
  (cond ((null? l) '())
        ((atom? (car l))
         (if (even? (car l))
             (cons (car l)
                   (evens-only* (cdr l)))
             (evens-only* (cdr l))))
        (else (cons (evens-only* (car l))
                    (evens-only* (cdr l))))))


(define (evens-only*-co l col)
  (cond ((null? l) (col '() 1 0))
        ((atom? (car l))
         (if (even? (car l))
             (evens-only* (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                                 (* (car l) p) s)))
             (evens-only* (cdr l)
                          (lambda (newl p s)
                            (col newl p (+ (car l) s))))))
        (else (evens-only* (car l)
                           (lambda (al ap as)
                             (evens-only* (cdr l)
                                          (lambda (dl dp ds)
                                            (col (cons al dl)
                                                 (* ap dp)
                                                 (+ as ds)))))))))
