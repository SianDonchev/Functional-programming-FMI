(define (atom? x) (and (not (list? x)) (not (pair? x))))

(define (map-deep f l)
  (cond ((null? l) l)
        ((atom? (car l)) (cons (f (car l))
                               (map-deep f (cdr l))))
        (else (cons (map-deep f (car l))
                    (map-deep f (cdr l)))))) 

(define (zip a b res)
  (if (or (null? a) (null? b)) res
      (zip (cdr a) (cdr b) (if (null? res)
                               (list (car a) (car b))
                               (list res (list (car a) (car b)))))))


(define (removeAll l x)
  (if(null? l) l
  (if(= x (car l)) (removeAll (cdr l) x)
     (cons (car l)(removeAll (cdr l) x)))))
  

(define (remove-duplicates l)
  (if(null? l) l
     (cons (car l) (remove-duplicates (removeAll (cdr l) (car l))))))


(define (chunk l n)
  (define (chunk-h  l1 first second n1)
    (cond ((null? l1) (list  first second))
          ((= n1 0)
           (chunk-h l1
                    (if (null? first) second (list first second)) '() n))
          (else
           (chunk-h (cdr l1) first (cons (car l1) second) (- n1 1)))))
  (chunk-h l '() '() n)) 
    








