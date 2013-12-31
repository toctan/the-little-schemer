(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (atom? x)
  (not (list? x)))
