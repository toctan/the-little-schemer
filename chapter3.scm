;; rember - remove a member

(define (rember a lat)
  (if (null? lat) '()
      (if (eq? a (car lat))
          (cdr lat)
          (cons (car lat)
                (rember a (cdr lat))))))

(define (rember a lat)
  (cond ((null? lat) '())
        ((eq? a (car lat)) (cdr lat))
        (else (cons (car lat)
                    (rember a (cdr lat))))))


(define (firsts l)
  (if (null? l) '()
      (cons (car (car l))
            (firsts (cdr l)))))


(define (insertR new old lat)
  (if (null? lat) '()
      (if (eq? old (car lat))
          (cons old
                (cons new (cdr lat)))
          (cons (car l)
                (insertR new old (cdr lat))))))


(define (insertL new old lat)
  (if (null? lat) '()
      (if (eq? old (car lat))
          (cons new lat)
          (cons (car l)
                (insertL new old (cdr lat))))))


(define (subst new old lat)
  (if (null? lat) '()
      (if (eq? old (car lat))
          (cons new (cdr lat))
          (cons (car l)
                (insertL new old (cdr lat))))))


(define (subst2 new o1 o2 lat)
  (if (null? lat) '()
      (if (or (eq? o1 (car lat))
              (eq? o2 (car lat)))
          (cons new (cdr lat))
          (cons (car l)
                (subst new o1 o2 (cdr lat))))))


(define (multirember a lat)
  (if (null? lat) '()
      (if (eq? a (car lat))
          (multirember (cdr lat))
          (cons (car lat)
                (multirember a (cdr lat))))))


(define (multiinsertR new old lat)
  (if (null? lat) '()
      (if (eq? old (car lat))
          (cons old
                (cons new
                      (multiinsertR new old (cdr lat))))
          (cons (car l)
                (multiinsertR new old (cdr lat))))))


(define (multiinsertL new old lat)
  (if (null? lat) '()
      (if (eq? old (car lat))
          (cons new
                (cons old
                      (multiinsertL new old (cdr lat))))
          (cons (car l)
                (multiinsertL new old (cdr lat))))))
