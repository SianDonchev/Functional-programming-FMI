#lang racket

(define (removeXFromList x l comparePredicate)
  (cond ((null? l) '())
        ((comparePredicate (car l) x) (removeXFromList x (cdr l) comparePredicate))
        (else (cons (car l) (removeXFromList x (cdr l) comparePredicate)))))


(define (timesInList x l comparePredicate)
  (cond ((null? l) 0)
        ((comparePredicate (car l) x) (+ 1 (timesInList x (cdr l) comparePredicate)))
        (else (timesInList x (cdr l) comparePredicate))))

(define (histogram word comparePredicate)
  (if (null? word) '()
      (let ((firstLetter (car word)))
      (cons (cons firstLetter (timesInList firstLetter word comparePredicate))
            (histogram (removeXFromList firstLetter word comparePredicate)
                       comparePredicate)))))


(define (equalForTrees t1 t2 comparePredicate)
  (cond ((and (null? t1) (not (null? t2))) #f)
        ((and (null? t1) (null? t2)) #t)
        ((and (not (null? t1)) (null? t2)) #f)
        ((and (leafTree? t1) (not (leafTree? t2))) #f)
        ((and (not (leafTree? t1)) (leafTree? t2)) #f)
        ((and (leafTree? t1) (leafTree? t2)) (and (comparePredicate (car (root-tree t1))
                                                                    (car (root-tree t2)))
                                                  (= (cdr (root-tree t1))
                                                     (cdr (root-tree t2)))))
        (else (and (equal? (root-tree t1) (root-tree t2))
                   (equalForTrees (left-tree t1) (left-tree t2) comparePredicate)
                   (equalForTrees (right-tree t1) (right-tree t2) comparePredicate)))))

                                                  
(define (makeTree root left right)
  (list root left right))

(define root-tree car)

(define left-tree cadr)

(define right-tree caddr)

(define (leafTree? tree)
  (if (null? tree) #f
      (and (null? (left-tree tree)) (null? (right-tree tree)))))

(define (makeLeafTree root)
  (list root '() '()))


(define (startingTrees histogramList)
  (map makeLeafTree histogramList))


(define (findTreeWithSmallestRoot trees)
  (define (helper trees smallest)
  (cond ((null? trees) smallest)
        ((< (cdr (root-tree (car trees))) (cdr (root-tree smallest)))
         (helper (cdr trees) (car trees)))
        (else (helper (cdr trees) smallest))))
  (if (null? trees) '()
      (helper trees (car trees))))


(define (findTheTwoSmallestTrees trees comparePredicate)
  (if (< (length trees) 2) trees
      (let* ((smallestTree (findTreeWithSmallestRoot trees))
            (secondSmallest
              (findTreeWithSmallestRoot (removeXFromList smallestTree trees comparePredicate))))
      (list smallestTree secondSmallest)))) 
  

(define (removeTheTwoSmallestTrees smallest trees comparePredicate)
  (define first (car smallest))
  (define second (cadr smallest))
  (filter (lambda (x) (and (not (equalForTrees first x comparePredicate)) (not (equalForTrees second x comparePredicate)))) trees))


(define (makeTreeFromSmallestTrees twoSmallest)
  (define first (car twoSmallest))
  (define second (cadr twoSmallest))
  (makeTree (cons '/ (+ (cdr (root-tree first)) (cdr (root-tree second))))
                  first second))


(define (makeTheHuffmanTree word comparePredicate)
  (define trees (histogram word comparePredicate))
  (define forest (startingTrees trees))
  (define (helper forest)
    (cond ((null? forest) forest)
          ((< (length forest) 2) (car forest))
          (else (let ((twoSmallest (findTheTwoSmallestTrees forest comparePredicate)))
                  (helper (cons (makeTreeFromSmallestTrees twoSmallest)
                                (removeTheTwoSmallestTrees twoSmallest forest comparePredicate)))))))
  (helper forest))

(define (snoc x l)
  (append l (list x)))

(define (findTheBinaryCode elem tree comparePredicate)
  (define (helper elem tree code)
    (cond ((null? tree) #f)
          ((equalForTrees tree elem comparePredicate) code)
          (else (or (helper elem (left-tree tree) (snoc 0 code))
                    (helper elem (right-tree tree) (snoc 1 code))))))
  (helper elem tree '()))


(define (listOfLeafs hist)
  (startingTrees hist))

(define (listOfBinaryCodes leafs huffmanTree comparePredicate)
  (map (lambda (x) (cons x (list (findTheBinaryCode x huffmanTree comparePredicate)))) leafs))


(define (findCodeForX x codes comparePredicate)
  (cond ((null? codes) "")
        ((comparePredicate (caaaar codes) x) (cdar codes))
        (else (findCodeForX x (cdr codes) comparePredicate))))

(define (encoding word comparePredicate)
  (define huffmanTree (makeTheHuffmanTree word comparePredicate))
  (define leafs (listOfLeafs (histogram word comparePredicate)))
  (define codes (map (lambda (x) (cons (car x) (listToString (cadr x))))
                     (listOfBinaryCodes leafs huffmanTree comparePredicate)))
  (define encodedWord (apply string-append
                             (map (lambda (x)(findCodeForX x codes comparePredicate)) word)))
 (cons huffmanTree encodedWord))


(define (listToString l)
  (if (null? l) ""
      (string-append (number->string (car l))
                     (listToString (cdr l)))))

(define (stringToList s)
  (map (lambda (x) (- (char->integer x) 48))
       (string->list s)))


(define (generateElementNtimes element n)
  (if (= n 0) '()
      (cons element (generateElementNtimes element (- n 1)))))

(define (decode huffmanTree code)
  (define listCode (stringToList code))
  (define (helper subTree code word)
    (cond ((leafTree? subTree) (helper huffmanTree code
                     (snoc (car (root-tree subTree)) word)))
          ((null? code) word)
          ((= (car code) 0) (helper (left-tree subTree) (cdr code) word))
          (else (helper (right-tree subTree) (cdr code) word))))
  (if (and (leafTree? huffmanTree) (null? listCode))
      (generateElementNtimes (car (root-tree huffmanTree)) (cdr (root-tree huffmanTree))) 
  (helper huffmanTree listCode '())))


(define (readWithPort p)
  (let ((x (read p)))
    (if (eof-object? x)
        (begin (close-input-port p) '())
        (cons x (readWithPort p)))))
        

(define (openFileAndRead fileName)
  (let ((p (open-input-file fileName)))
    (readWithPort p)))


(define (writeWithPort p information)
  (if (not (null? information))
      (begin (write (car information) p)
             (display " " p)
             (writeWithPort p (cdr information)))
      (close-output-port p)))


(define (openFileAndWrite fileName information)
  (let ((p (open-output-file fileName)))
    (writeWithPort p information)))

(define (takePredicate l)
  (cond ((null? l) '())
        ((null? (cdr l)) (eval (car l)))
        (else (takePredicate (cdr l)))))


(define (takeWord l)
  (define (helper l res)
  (cond ((null? l) '())
        ((null? (cdr l)) '())
        ((null? (cddr l)) (snoc (car l) res))
        (else (helper (cdr l) (snoc (car l) res)))))
  (helper l '()))

(define (encodeWithFile fileName)
  (define inputFromFile (openFileAndRead fileName))
  (define comparePredicate  (takePredicate inputFromFile))
  (define word (takeWord inputFromFile))
  (encoding word comparePredicate))
    

(define (encodeWithReadingAndWritingToFile readFileName writeFileName)
  (define information (encodeWithFile readFileName))
  (openFileAndWrite writeFileName (cons (car information) (list (cdr information)))))

(define (decodeWithReadingAndWritingToFile readFileName writeFileName)
  (define encodedData (openFileAndRead readFileName))
  (define huffmanTree (car encodedData))
  (define binaryCode (cadr encodedData))
  (openFileAndWrite writeFileName (decode huffmanTree binaryCode)))


;---EXAMPLES---
(define example '(a b r a c a d a b r a))
(define te (encoding example equal?))
(define decTe (decode (car te) (cdr te)))

(define secondExample (list 1 2 3 (list 1 2 3) 'abracadabra '(1 2 3) "string" "abracadabra"))
(define het (encoding secondExample equal?))
(define decHet (decode (car het) (cdr het)))

(define empty '())
(define encEmpt (encoding empty =))
(define decEmpt (decode (car encEmpt) (cdr encEmpt)))


;---BONUS---
;(encodeWithReadingAndWritingToFile "test.txt" "result.txt")
;(decodeWithReadingAndWritingToFile "result.txt" "originalData.txt")
