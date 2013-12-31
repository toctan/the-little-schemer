(define (numbered? exp)
  (if (atom? exp)
      (number? exp)
      (and (numbered? (car exp))
           (numbered? (caddr exp)))))


(define (1st-sub-exp exp) (cadr aexp))
(define (2nd-sub-exp exp) (caddr aexp))
(define (operator exp) (car aexp))

(define (value exp)
  (cond ((atom? exp) exp)
        ((eq? (operator exp) '+)
         (+ (value (1st-sub-exp exp))
            (value (2nd-sub-exp exp))))
        ((eq? (operator exp) '*)
         (* (value (1st-sub-exp exp))
            (value (2nd-sub-exp exp))))
        (else (** (value (1st-sub-exp exp))
                 (value (2nd-sub-exp exp))))))





(define (sero? n) (null? n))
(define (edd1 n) (cons '() n))
(define (zub1 n) (cdr n))

(define (edd n m)
  (if (sero? m) n
      (edd1 (edd n (zub1 m)))))
