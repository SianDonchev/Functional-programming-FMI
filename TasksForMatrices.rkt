;task 1
(define (countElements l)
  (if (null? l) 0
      (+ 1 (countElements (cdr l))))) 

(define (dimensions m)
  (if (null? m) (cons 0 0)
      (cons (countElements m) (countElements (car m)))))

;task 2
(define (snoc l x)
  (append l (list x)))

(define (myReverse l)
  (define (myReverse-h l res)
  (if (null? l) res
      (myReverse-h (cdr l) (cons (car l) res))))
  (myReverse-h l '()))

(define (reverse-columns m)
 (if (null? m) m
     (cons (myReverse (car m)) (reverse-columns (cdr m)))))

;task 3
(define (nth-element l n)
  (cond ((or (> n (length l))) '())
        ((= n 1) (car l))
        ((null? l) l)
        (else (nth-element (cdr l) (- n 1)))))

(define (nth-column m n)
  (if (null? m) m
      (cons (nth-element (car m) n) (nth-column (cdr m) n))))



;task 4
(define (main-diagonal m)
  (define (main-diagonal-h m index)
    (if (null? m) m
        (cons (nth-element (car m) index)
              (main-diagonal-h (cdr m) (+ index 1)))))
  (main-diagonal-h m 1))


;task 5
(define (transpose m)
  (define (transpose-h m index)
    (if (null? m) m
        (if (> index (length m)) '()
        (cons (nth-column m index)
              (transpose-h m (+ index 1))))))
  (transpose-h m 1))


;task 6
(define (for-all-columns? m p)
  (define (helper m p index)
  (cond ((null? m) t)
        ((not (p (nth-column m index)) #f))
        ((> index (length m)) #t)
        (else (helper m p (+ index 1)))))
  (helper m p 1))

























      
      