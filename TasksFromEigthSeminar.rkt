;task2

(define (makeList-h fromW n)
  (if (= n 0) '()
      (cons fromW (makeList-h fromW (- n 1)))))

(define (makeList pairList)
  (if (null? pairList) '()
      (makeList-h (car pairList) (cdr pairList))))

(define (run-length-decode l)
  (if (null? l) l
      (append (makeList (car l)) (run-length-decode (cdr l)))))

;task3

(define (countElement l x)
  (if (null? l) 0
      (if (= (car l) x)
          (+ 1 (countElement(cdr l) x))
          (countElement (cdr l) x))))

(define (snoc l x)
      (append l (list x)))

(define (removeAll l x)
(define (removeAll-h l res x)
  (if (null? l) res
      (if (= (car l) x) (removeAll-h (cdr l) res x)
          (removeAll-h (cdr l) (snoc res (car l)) x))))
(removeAll-h l '() x))

(define (histogram l)
  (if (null? l) l
      (cons (cons (car l) (countElement l (car l)))
            (histogram (removeAll l (car l))))))


;task 4
(define (isAlreadyIn l x)
  (if (null? l) #f
      (if (= (caar l) x) #t
      (isAlreadyIn (cdr l) x))))

(define (addResult l x)
  (if (null? l) (list (list x))
      (if (isAlreadyIn l x)
          l
          (snoc l (list x)))))

(define (makeListFromResults l f)
(define (makeListFromResults-h l f res)
  (if (null? l) res
      (makeListFromResults-h (cdr l) f (addResult res (f (car l))))))
(makeListFromResults-h l f '()))

  ;(map (lambda (x) (addResult res (f x))) l)))

(define (addLists l toAdd)
 (if (null? toAdd) l
  (addLists (cons l (car toAdd)) (cdr toAdd))))

(define (addTof l x fx)
(define (addTof-h l1 res)
  (if (null? l1) res
      (if (not (equal? fx (car (car l1))))
     (addTof-h (cdr l1) (snoc res (car l1)))
     (addTof-h (cdr l1) (snoc res (snoc (car l1) x))))))      
(addTof-h l '()))

(define (group-by f l)
  (define newList (makeListFromResults l f))
  (define (group-by-h f l1 res)
    (if (null? l1) res
      (group-by-h f (cdr l1) (addTof res (car l1) (f (car l1))))))
  (group-by-h f l newList))

      
  ;    (map (lambda (x) (addTof newList (f x))) l)))


























































