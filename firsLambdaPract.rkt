(define (accumulate op nv a b term next)
  (if (< b a)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (< b a)
      nv
      (accumulate op (op nv (term a)) (next a) b term next)))

(define (factorial n)
  (accumulate * 1 1 n (lambda (x) x) (lambda (x) (+ 1 x))))

(define (myExpt x n)
  (accumulate * 1 1 n (lambda (a) x) (lambda (x) (+ 1 x))))

(define (myExp x n)
  (accumulate + 0. 0 n
              (lambda (a) (/ (myExpt x a) (factorial a)))
              (lambda (x) (+ 1 x))))

(define (exists? pr? a b)
  (accumulate (lambda (a b) (or a b)) #f a b pr? (lambda (x) (+ 1 x))))

(define (forAll? pr? a b)
  (not (exists? (lambda (x)(if (pr? x) #f #t)) a b)))

(define (f x)(lambda () x))

(define (id x)(x))

(define (square x)(* x x))

(define (fast-expt x n)
(define (fast-expt-h x n1 result)
  (cond ((= n1 1) (if (even? n) (* result result) (* result x)))
        ((even? n1) (if (= result 1)
                       (fast-expt-h x (quotient n1 2) x)
                                    (fast-expt-h x (quotient n1 2) (square result))))
        (else (fast-expt-h x (- n1 1) (* result x)))))
  (fast-expt-h x n 1))




                              