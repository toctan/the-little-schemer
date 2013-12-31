(define (+ n m)
  (if (zero? m) n
      (1+ (+ n (-1+ m)))))


(define (- n m)
  (if (zero? m) n
      (-1+ (- n (-1+ m)))))


(define (* n m)
  (if (zero? m) 0
      (+ n (* n (-1+ m)))))


(define (/ n m)
  (if (< n m) 0
      (1+ (/ (- n m) m))))


(define (** n m)
  (if (zero? m) 1
      (* (** n (-1+ m)))))


(define (> n m)
  (cond ((zero? n) #f)
        ((zero? m) #t)
        (else (> (-1+ n) (-1+ m)))))


(define (< n m)
  (cond ((zero? m) #f)
        ((zero? n) #t)
        (else (< (-1+ n) (-1+ m)))))


(define (= n m)
  (not (or (> n m)
           (< n m))))


(define (addtup tup)
  (if (null? tup) 0
      (+ (car tup)
         (addtup (cdr tup)))))


(define (tup+ tup1 tup2)
  (cond ((null? tup1) tup2)
        ((null? tup2) tup1)
        (else (cons (+ (car tup1) (car tup2))
                    (tup+ (cdr tup1) (cdr tup2))))))


(define (length lat)
  (if (null? lat) 0
      (1+ (length (cdr lat)))))


(define (pick n lat)
  (if (= 1 (car lat)) (car lat)
      (pick (-1+ n) (cdr lat))))


(define (rempick n lat)
  (if (= 1 (car lat)) (cdr lat)
      (rempick (-1+ n) (cdr lat))))


(define (no-nums lat)
  (cond ((null? lat) '())
        ((number? (car lat)
                  (no-nums (cdr lat))))
        (else (cons (car lat)
                    (no-nums (cdr lat))))))


(define (all-nums lat)
  (cond ((null? lat) '())
        ((number? (car lat)
                  (cons (car lat)
                        (all-nums (cdr lat)))))
        (else (all-nums (cdr lat)))))


(define (occur a lat)
  (if (null? lat) 0
      (if (eq? a (car lat))
          (1+ (occur a (cdr lat)))
          (occur a (cdr lat)))))
