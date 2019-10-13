  (define (fact x)(if (= x 1) 1 (* (fact (- x 1)) x)))

  (define (even x) (= (remainder x 2) 0))

  (define (square x) (* x x))

 (define (fast-expt x n) (cond ((= n 1) x)
                           ((even n) (square (fast-expt x (/ n 2))))
                               (else (* (fast-expt x (- n 1)) x))))

  (define (sum-Interval start end)
    (define (sumInt start end sum)
      (if (= start end)
            (+ sum end)
            (sumInt (+ start 1) end (+ sum start))) )
  (sumInt start end 0))

   (define (count-digits n)
   (define (count-digits-for num dig)
     (if (= (remainder num 10) num)
         (+ dig 1)
         (count-digits-for (quotient num 10) (+ dig 1))))
   (count-digits-for n 0))

  (define (prime n)
  (define (prime-for n div) 
   (cond    ((or (= n 2) (= n 3)) #t)
            ((= n 1) #f)
            ((= div (/ n 2)) #t)
             (else (if (= (remainder n div) 0) #f (prime-for n (+ div 1))) )))
    (prime-for n 2))

             
(define (sum-digits n sum)
     (if (= (remainder n 10) n) 
            (+ sum n)  
            (sum-digits (quotient n 10) (+ sum (remainder n 10) ))))


(define (sumOfDigits n)
(define (sum-digits n sum)
     (if (= (remainder n 10) n) 
            (+ sum n)  
            (sum-digits (quotient n 10) (+ sum (remainder n 10) ))))
  (sum-digits n 0 ))

(define (reverseDigits n)
(define (reverse-digits num revNum)
 (define currentDigit (remainder num 10))
 (if (= currentDigit num)
           (+ (* revNum 10) currentDigit)
           (reverse-digits (quotient num 10) (+ (* revNum 10) currentDigit))))
    (reverse-digits n 0))