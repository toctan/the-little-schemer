(define (rember* a l)
  (cond ((null? l) '())
        ((atom? (car l))
         (if (eq? a (car l))
             (rember* a (cdr l))
             (cons (car l)
                   (rember* a (cdr l)))))
        (else (cons (rember* a (car l))
                    (rember* a (cdr l))))))


(define (insertR* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (if (eq? old (car l))
             (cons old
                   (cons new (insertR* new old (cdr l))))
             (cons (car l)
                   (insertR* new old (cdr l)))))
        (else (cons (insertR* new old (car l))
                    (insertR* new old (cdr l))))))


(define (insertL* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (if (eq? old (car l))
             (cons new
                   (cons old (insertL* new old (cdr l))))
             (cons (car l)
                   (insertL* new old (cdr l)))))
        (else (cons (insertL* new old (car l))
                    (insertL* new old (cdr l))))))


(define (subst* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (if (eq? old (car l))
             (cons new (subst* new old (cdr l)))
             (cons (car l)
                   (subst* new old (cdr l)))))
        (else (cons (subst* new old (car l))
                    (subst* new old (cdr l))))))


(define (member* a l)
  (cond ((null? l) #f)
        ((atom? (car l))
         (or (eq? a (car l))
             (member* a (cdr l))))
        (else (or (member* a (car l))
                  (member* a (cdr l))))))


(define (occur* a l)
  (cond ((null? l) '())
        ((atom? (car l))
         (if (eq? a (car l))
             (1+ (occur* a (cdr l)))
             (occur* a (cdr l))))
        (else (+ (occur* a (car l))
                 (occur* a (cdr l))))))


(define (leftmost l)
  (and ((atom? (caar l))
        (leftmost (car l)))))


(define (eqlist? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        ((and (atom? l1) (atom? l2))
         (and (eq? (car l1) (car l2))
              (eq? (cdr l2) (cdr l2))))
        ((or (atom? l1) (atom? l2)) #f)
        (else (and (eqlist? (car l1) (car l2))
                   (eqlist? (cdr l2) (cdr l2))))))


(define (equal? s1 s2)
  (cond ((and (atom? l1) (atom? l2))
         (eq? s1 s2))
        ((or (atom? l1) (atom? l2)) #f)
        (else (eqlist? s1 s2))))


(define (eqlist? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        (else (and (equal? (car l1) (car l2))
                   (eqlist? (cdr l2) (cdr l2))))))


(define (rember s l)
  (cond ((null? l) '())
        ((equal? s (car l)) (cdr l))
        (else (cons (car l)
                    (rember s (cdr l))))))
