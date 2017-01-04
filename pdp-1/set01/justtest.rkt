;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname justtest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

(define-struct reorder (days quantity))
; a Reorder is a (make-reorder NonNegInteger NonNegInteger)
; Interpretation:
;  --days: the time needed for the delivery of the books(one unit as a day)
;  --quantity: the number of the books reordered
; Template:
; (define (reorder-fn r)
; 	(...
; 		(reorder-days r)
; 		(reorder-quantity r)))
;examples: see below
(define reorder1 (make-reorder 15 15))
(define reorder2 (make-reorder 10 25))

; a ReorderStatus is one of:
;  --Boolean
;  --(make-reorder days quantity)
; Interpretation:
;  --iff there is no reorder the ReorderStatus is false
;  --iff there is a reorder, the ReorderStatus is a Reorder
; EXAMPLES: see below
; Template:
; (define (ros-fn ... ros)
; 	(if (reorder-present? ros) 
; 		(reorder-fn ros)
; 		...))

; reorder-present? : ReorderStatus -> Boolean
; GIVEN: a ReorderStatus
; RETURNS: #t iff the given ReorderStatus shows a pending re-order or false.
; EXAMPLES: see tests below
; STRATEGY: functions composition
(define (reorder-present? r)
  (not (false? r)))

; make-empty-reorder : Any -> ReorderStatus
; GIVEN: Any, ignores any inputs
; RETURNS: a ReorderStatus showing no pending re-order. 
; EXAMPLES: see tests below
; STRATEGY: functions composition
(define (make-empty-reorder r)
  false)

;examples:
(define emptyReorder (make-empty-reorder 1))
(define rs1 reorder1)
(define rs2 emptyReorder)

;; A MaybeInteger is one of:
;; -- Integer
;; -- false
; Template: 
; (define (MaybeInteger-fn mi)
; 	(cond 
; 		[(number? mi) ...]
; 		[else ...]))
; examples:
; (define mi_false false)
; (define mi2_number 10)

;; A MaybeBook is one of:
;; -- Book
;; -- false
; Template: 
; (define (MaybeBook-fn mb)
; 	(cond 
; 		[(book? mb) ...]
; 		[else ...]))
; examples:
; (define mb_false fasle)
; (define mb_book book3 (make-book 24351387 "Three good books" "Joe" "MIT Press" 2419 2000 20 emptyReorder 10))

(define-struct line-item (isbn quantity))
; a Line-Item is a (make-line-item NonNegInteger NonNegInteger)
; Interpretation:
;  --isbn: the isbn of the book
;  --quantity: the quantity
; Template:
; (define (line-item-fn lt)
; 	(...
; 		(line-item-isbn lt)
; 		(line-item-quantity lt)))
; EXAMPLE for tests:
(define lt0 (make-line-item 61387 2))
(define lt1 (make-line-item 45861387 2))
(define lt1-y2 (make-line-item 45861387 2))
(define lt1-n (make-line-item 45861387 18))
(define lt2 (make-line-item 23841387 10))
(define lt2-y2 (make-line-item 23841387 2))
(define lt3 (make-line-item 24351387 30))

; an Order is a List of Line-Item, it's one of
;  --empty
;  --(cons Line-Item ListOfLine-Item)
;  Template:
;  (define (order-fn olst)
;  	(cond
;  		[(empty? olst) ...]
;  		[else (... (first olst)
; 					(order-fn (rest olst)))]))
; Examples:
(define order0 (list lt0))
(define order1 (list lt1))
(define order1-n (list lt1-n))
(define order2 (list lt1 lt2))
(define order2-y2 (list lt1-y2 lt2-y2))
(define order3 (list lt1 lt2 lt3))
(define-struct book (isbn title author publisher unit_price unit_cost stock_number reorder_status unit_volume))
; a Book is a (make-book Integer String String String NonNegInteger NonNegInteger NonNegInteger ReorderStatus NonNegInteger)
; Interpretation:
; --isbn, an NonNegInteger (the "international standard book number"). This serves as a unique identifier for this book. 
;  	There is an official data definition for ISBN's, but we will simply model them as integers.
;  --title, a string, the book's title
;  --author, a string, the book's author
;  --publisher, a string, the book's publisher
;  --unit price: a non-negative integer, the price at which we will sell the book (in USD*100, ie $14.99 is represented as 1499).
;  --unit cost: a number, the cost of the book to the bookstore in USD*100, also a non-negative integer
;  --stock_number: number of copies on hand, also a NonNegInteger
;  --ReorderStatus. a ReorderStatus, contains the infomation about the book's reorder
;  --unit_volume: the volume taken up by one unit of this item, in cubic feet, also NonNegInteger
; Template:
; (define (book-fn b)
; 	(... 
; 		(book-isbn b)
; 		(book-title b)
; 		(book-author b)
; 		(book-publisher b)
; 		(book-unit_price b)
; 		(book-unit_cost b)
; 		(book-stock_number b)
; 		(book-reorder_status b)
; 		(book-unit_volume b)))
; Examples for testing:

(define book0 (make-book 61387 "A good book" "Joe" "NU Press" 1409 1000 12 reorder1 10))
(define book1 (make-book 45861387 "A good book" "Joe" "NU Press" 1409 1000 12 reorder1 10))
(define book2 (make-book 23841387 "Two good books" "Joe" "NU Press" 1419 900 2 reorder2 10))
(define book3 (make-book 24351387 "Three good books" "Joe" "MIT Press" 2419 2000 20 emptyReorder 10))

(define ivt1 (list (make-book 61387 "A good book" "Joe" "NU Press" 1409 1000 12 reorder1 10)
                   (make-book 45861387 "A good book" "Joe" "NU Press" 1409 1000 12 reorder1 10)
                   (make-book 23841387 "Two good books" "Joe" "NU Press" 1419 900 2 reorder2 10)
                   (make-book 24351387 "Three good books" "Joe" "MIT Press" 2419 2000 20 emptyReorder 10)))

(begin-for-test (check-equal? (first ivt1) (make-book 61387 "A good book" "Joe" "NU Press" 1409 1000 12 reorder1 10))
                (check-equal? (cons 	
                               (make-book (book-isbn (first ivt1))
                                          (book-title (first ivt1))
                                          (book-author (first ivt1))
                                          (book-publisher (first ivt1))
                                          (book-unit_price (first ivt1))
                                          (book-unit_cost (first ivt1))
                                          (book-stock_number (first ivt1))
                                          (book-reorder_status (first ivt1))
                                          (book-unit_volume (first ivt1)))
                               book1 book2 book3 empty)
                              ivt1))