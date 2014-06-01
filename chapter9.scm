(define (atom? x)
  (not (or (null? x) (pair? x))))

(define (a-pair? x)
  (cond ((atom? x) #f)
        ((null? x) #f)
        ((null? (cdr x)) #f)
        ((null? (cddr x)) #t)
        (else #f)))

(define first car)

(define second cadr)

(define (build x y)
  (cons x (cons y '())))

(define (revpair p)
  (build (second p) (first p)))




(define (looking a lat)
  (define (keep-looking a sorn lat)
    (if (number? sorn)
        (keep-looking a (pick sorn lat) lat)
        (eq? sorn a)))

  (and (not (null? lat))
       (keep-looking a (pick 0 lat) lat)))

(looking 'caviar '(6 2 4 caviar 5 7 3))
;; Value: #t

(looking 'caviar '(6 2 grits caviar 5 7 3))
;; Value: #f




(define (shift p)
  (build (first (first p))
         (build (second (first p))
                (second p))))

(shift '((a b) c))
;; Value: (a (b c))

(shift '((a b) (c d)))
;; Value: (a (b (c d)))




(define (align pora)
  (cond
   ((atom? pora) pora)
   ((a-pair? (first pora))
    (align (shift pora)))
   (else (build (first pora)
                (align (second pora))))))




(define (length* pora)
  (if (atom? pora)
      1
      (+ (length* (first pora))
         (length* (second pora)))))

(define (weight* pora)
  (if (atom? pora)
      1
      (+ (* 2 (weight* (first pora)))
         (weight* (second pora)))))

(weight* '((a b) c))
;; Value: 7

(weight* '(a (b c)))
;; Value: 5




(define (shuffle pora)
  (cond ((atom? pora) pora)
        ((a-pair? (first pora))
         (shuffle (revpair pora)))
        (else (build (first pora)
                     (shuffle (second pora))))))




(define (C n)
  (cond ((= n 1) 1)
        ((even? n) (C (/ n 2)))
        (else (C (+ 1 (* 3 n))))))

(define (A n m)
  (cond ((zero? n) (+ 1 m))
        ((zero? m) (A (- n 1) 1))
        (else (A (- n 1)
                 (A n (- m 1))))))




(define (will-stop? f))

(define (eternity x) (eternity x))

(define (last-try x)
  (and (will-stop? last-try)
       (eternity x)))

(will-stop? length)
;; Value: #t

(will-stop? eternity)
;; Value: #f

(will-stop? last-try)
;; will-stop? can not be defined!
length0 ->
(lambda (l)
  (if (null? l)
      0
      (1+ (eternity (cdr l)))))

((lambda (length)
   (lambda (l)
     (if (null? l)
         0
         (1+ (length (cdr l))))))
 eternity)

((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (if (null? l)
         0
         (1+ (length (cdr l)))))))


((lambda (mk-length)
   (mk-length mk-length))
 (lambda (length)
   (lambda (l)
     (if (null? l)
         0
         (1+ (length (cdr l)))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (if (null? l)
         0
         (1+ (mk-length (cdr l)))))))

length1 ->
(lambda (l)
  (if (null? l)
      0
      (1+ ((lambda (l)
             (if (null? l)
                 0
                 (1+ (eternity (cdr l)))))
           (cdr l)))))

((lambda (f)
   (lambda (l)
     (if (null? l)
         0
         (1+ (f (cdr l))))))
 ((lambda (g)
    (lambda (l)
      (if (null? l)
          0
          (1+ (g (cdr l))))))
  eternity))

((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (if (null? l)
         0
         (1+ (length (cdr l)))))))


((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (if (null? l)
         0
         (1+ ((mk-length eternity)
              (cdr l)))))))

length2 ->
(lambda (l)
  (if (null? l)
      0
      (1+ ((lambda (l)
             (if (null? l)
                 0
                 (1+ ((lambda (l)
                        (if (null? l)
                            0
                            (1+ (eternity (cdr l)))))
                      (cdr l)))))
           (cdr l)))))

((lambda (length)
   (lambda (l)
     (if (null? l)
         0
         (1+ (length (cdr l))))))
 ((lambda (length)
    (lambda (l)
      (if (null? l)
          0
          (1+ (length (cdr l))))))
  ((lambda (length)
     (lambda (l)
       (if (null? l)
           0
           (1+ (length (cdr l))))))
   eternity)))

((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (if (null? l)
         0
         (1+ (length (cdr l)))))))

length ->
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (if (null? l)
         0
         (1+ ((mk-length mk-length)
              (cdr l)))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (if (null? l)
            0
            (1+ (length (cdr l))))))
    (mk-length mk-length))))

((lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (if (null? l)
            0
            (1+ (length (cdr l))))))
    (mk-length mk-length)))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (if (null? l)
            0
            (1+ (length (cdr l))))))
    (mk-length mk-length))))

((lambda (length)
   (lambda (l)
     (if (null? l)
         0
         (1+ (length (cdr l))))))
 ((lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (if (null? l)
             0
             (1+ (length (cdr l))))))
     (mk-length mk-length)))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (if (null? l)
             0
             (1+ (length (cdr l))))))
     (mk-length mk-length)))))

((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (if (null? l)
         0
         (1+ (length (cdr l)))))))

(lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
           ((mk-length mk-length) x))))))

(define Y
  (lambda (g)
    ((lambda (f) (f f))
     (lambda (f)
       (g (lambda (x) ((f f) x)))))))

(Y Y)                                   ; ?
