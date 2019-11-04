;task 1
(define (make-tree root left right)
  (cons root (list left right)))

(define (root-tree tree)
  (if (null? tree) 'empty
  (car tree)))

(define (left-tree tree)
  (if (null? tree) tree
      (cadr tree)))

(define (right-tree tree)
  (if (null? tree) tree
      (caddr tree)))

(define (empty-tree? tree)
  (null? tree))

(define (leaf-tree? tree)
  (if (null? tree) #f
  (and (null? (left-tree tree)) (null? (right-tree tree)))))

(define (tree? tree)
  (cond ((null? tree) #t)
        (else (and (<= (length (cdr tree)) 2)
                   (tree? (left-tree tree))
                   (tree? (right-tree tree))))))
      
;task 2
(define (in-order tree)
  (cond ((null? tree) tree)
        ((leaf-tree? tree) (list (car tree)))
        (else (append (in-order (left-tree tree))
                    (list (root-tree tree))(in-order (right-tree tree))))))
      

(define (pre-order tree)
  (cond ((null? tree) tree)
        ((leaf-tree? tree) (list (car tree)))
        (else (append (list (root-tree tree))
                      (pre-order (left-tree tree))(pre-order (right-tree tree))))))


(define (post-order tree)
  (cond ((null? tree) tree)
        ((leaf-tree? tree) (list (car tree)))
        (else (append (post-order (left-tree tree))
                      (post-order (right-tree tree))(list (root-tree tree))))))



;task 3
(define (level n tree)
  (cond ((null? tree) tree)
        ((= n 0) (list (root-tree tree)))
        (else (append (level (- n 1)(left-tree tree))
                      (level (- n 1) (right-tree tree))))))
      

;task 4
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((leaf-tree? tree) 1)
        (else (+ (count-leaves (left-tree tree)) (count-leaves (right-tree tree))))))
      

;task 5
(define (map-tree f tree)
  (if (null? tree) tree
      (cons (f (root-tree tree))(cons (map-tree f (left-tree tree))
                          (list (map-tree f (right-tree tree)))))))


;task 6
(define (height tree)
  (cond ((null? tree) 0)
        ((leaf-tree? tree) 1)
        (else (max (+ 1 (height (left-tree tree))) (+ 1 (height (right-tree tree)))))))


;task 7
(define (sum-tree tree)
  (cond ((null? tree) 0)
        ((leaf-tree? tree) (root-tree tree))
        (else (+ (car tree) (sum-tree (left-tree tree)) (sum-tree (right-tree tree))))))
      

;task 8
(define (max-tree tree)
  (cond ((null? tree) 0)
        ((leaf-tree? tree) (root-tree tree))
        (else (max (root-tree tree)
                   (max-tree (left-tree tree))(max-tree (right-tree tree))))))


;task 9
(define (invert tree)
  (cond ((null? tree) tree)
        ((leaf-tree? tree) tree)
        (else (make-tree (root-tree tree)
                         (invert (right-tree tree))
                         (invert (left-tree tree))))))
      

;task 10
(define (binary-heap? tree)
  (cond ((null? tree) #t)
        ((leaf-tree? tree) #t)
        (else (and (if (not (equal? '() (left-tree tree)))
                       (>= (root-tree tree)
                           (root-tree (left-tree tree)))
                       #t)
                   (if (not (equal? '() (right-tree tree)))
                       (>= (root-tree tree)
                           (root-tree (right-tree tree)))
                       #t)
                   (binary-heap? (left-tree tree))
                   (binary-heap? (right-tree tree))))))

;task 11
(define (differenceByOne a b)
  (or (= (+ a 1) b) (= (+ b 1) a) (= a b)))

(define (balanced? tree)
  (cond ((null? tree) #t)
        ((leaf-tree? tree) #t)
        (else (and (or (differenceByOne (height (left-tree tree))
                                        (height (right-tree tree))))
                   (balanced? (left-tree tree))
                   (balanced? (right-tree tree))))))


                              
;task 12
(define (binary-search-tree? tree)
  (cond ((null? tree) #t)
        ((leaf-tree? tree) #t)
        (else (and (> (root-tree tree) (root-tree (left-tree tree)))
                   (<= (root-tree tree) (root-tree (right-tree tree)))
                   (binary-search-tree? (left-tree tree))
                   (binary-search-tree? (right-tree tree))))))


;task 13
(define (binary-search-tree-insert tree v)
  (cond ((null? tree) (list v '() '()))
        ((< v (root-tree tree)) (make-tree (root-tree tree)
                                           (binary-search-tree-insert (left-tree tree) v)
                                           (right-tree tree)))
        ((>= v (root-tree tree)) (make-tree (root-tree tree)
                                            (left-tree tree)
                                            (binary-search-tree-insert (right-tree tree) v)))))
        
;task 14
(define (addElementsToTree tree l)
  (if (null? l) tree
      (addElementsToTree (binary-search-tree-insert tree (car l)) (cdr l))))

(define (tree-sort l)
  (define (tree-sort-h l bst)
    (in-order (addElementsToTree bst l)))
  (tree-sort-h l '()))
    
    
;task 15
(define (nth-element l n)
  (cond ((or (> n (length l))) '())
        ((= n 1) (car l))
        ((null? l) l)
        (else (nth-element (cdr l) (- n 1)))))

(define (to-balanced-tree l)
  (define (to-balanced-tree-h l from to bst)
    (cond ((= from to) (binary-search-tree-insert bst
                                                (nth-element l (quotient (+ from to) 2))))
          ((< from to)
    (make-tree (nth-element l (quotient (+ from to) 2))
               (to-balanced-tree-h l from (- (quotient (+ from to) 2) 1) bst)
               (to-balanced-tree-h l (+ (quotient (+ from to) 2) 1) to bst)))
          (else bst)))
  (to-balanced-tree-h l 1 (length l) '()))
    






      

               