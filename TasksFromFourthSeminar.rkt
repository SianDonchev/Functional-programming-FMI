(define (accumulate op nv a b term next)
  (if (> a b) nv
   (op (term a)(accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (next x)(+ 1 x))

(define (count p? a b)
  (accumulate + 0 a b (lambda(x)(if (p? x) 1 0)) next))

(define (palindrome x)
  (define (reverse-digits x_h rev)
    (if (= (remainder x_h 10) x_h)
        (+ (* rev 10) x_h)
        (reverse-digits (quotient x_h 10) (+ (* rev 10) (remainder x_h 10)))))
  (= x (reverse-digits x 0)))

(define (count-palindromes a b)
  (accumulate + 0 a b (lambda (x) (if (palindrome x) 1 0) ) next))

(define (1+ x)(+ 1 x))

(define (double func) (lambda (x)(func (func x))))

(define (repeated func n)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x)(if (= n 1) (func x) (func ((repeated func (- n 1)) x))))))

(define (compose f g)(lambda (x) (f (g x))))

(define (sq x)(* x x))

(define (exists? predicate a b)
  (accumulate (lambda (x y) (or x y)) #f a b predicate next))

(define (for-all? predicate a b)
  (not (exists? (lambda (x) (if (predicate x) #f #t)) a b)))