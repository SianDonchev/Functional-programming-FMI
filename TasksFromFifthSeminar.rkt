(define (mYlength l)
  (+ 1 (length (cdr l))))
  
(define (sum l)
  (if (null? l)
      0
  (+ (car l) (sum (cdr l)))))

(define (myMember l x)
 (cond ((null? l) #f)
       ((equal? (car l) x) #t)
       (else (myMember (cdr l) x))))

(define (last l)
  (if (null? (cdr l)) (car l)
      (last (cdr l))))

(define (nth l n)
  (if (= n 0) (car l)
      (nth (cdr l) (- n 1))))

(define (scale l x)
  (myMap l (lambda (y) (* y x))))

(define (myMap l f)
  (if (null? l)
      '()
      (cons (f (car l)) (myMap (cdr l) f))))

(define (myReverse l)
(define (reverse-h l res)
  (if (null? l) res
      (reverse-h (cdr l) (cons (car l) res))))
  (reverse-h l '()))

(define (add-last l x)
  (if (null? l) '(x)
      (myReverse (cons x (myReverse  l)))))