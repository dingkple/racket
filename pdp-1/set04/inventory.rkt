;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname inventory) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require rackunit)
(require "extras.rkt")

(provide
 inventory-potential-profit
 inventory-total-volume
 price-for-line-item
 fillable-now?
 days-til-fillable
 price-for-order
 inventory-after-order
 increase-prices
 make-book
 reorder-present?
 make-empty-reorder
 make-reorder
 make-line-item
 inventory-after-deliveries)

; a NonNegInteger: an Integer which is 0 or greater than 0

(define-struct reorder (days quantity))
; a Reorder is a (make-reorder NonNegInteger NonNegInteger)
; Interpretation:
;  --days: the time needed for the delivery of the books(one unit as a day)
;  --quantity: the number of the books reordered
; Template:
; reorder-fn: Reorder -> ??
; (define (reorder-fn r)
; 	(...
; 		(reorder-days r)
; 		(reorder-quantity r)))
;examples: see below

; a ReorderStatus is one of:
;  --Boolean
;  --(make-reorder days quantity)
; Interpretation:
;  --iff there is no pending reorder, the ReorderStatus is a false
;  --iff there is a pending reorder, the ReorderStatus is a Reorder
; EXAMPLES: see below
; Template:
; ros-fn -> ReorderStatus -> ??
; (define (ros-fn ros)
;   (cond
;     [(Boolean? ros) ...]
;     [else ...]))

; reorder-present? : ReorderStatus -> Boolean
; GIVEN: a ReorderStatus
; RETURNS: #t iff the given ReorderStatus shows a pending re-order or false.
; EXAMPLES: see tests below
; STRATEGY: functions composition
(define (reorder-present? r)
  (not (false? r)))

(begin-for-test
  (check-equal? (reorder-present? (make-reorder 15 15))
                true
                "ReorderStatus not false should return #t"))

; make-empty-reorder : Any -> ReorderStatus
; GIVEN: Any, ignores any inputs
; RETURNS: a ReorderStatus showing no pending re-order. 
; EXAMPLES: see tests below
; STRATEGY: functions composition
(define (make-empty-reorder r)
  false)

;examples for tests:

 ; A MaybeInteger is one of:
; -- Integer
; -- false
; Template: 
; (define (MaybeInteger-fn mi)
; 	(cond 
; 		[(number? mi) ...]
; 		[else ...]))
; examples:
; (define mi_false false)
; (define mi2_number 10)

; A MaybeBook is one of:
; -- Book
; -- False(Boolean)
; Template: 
; (define (MaybeBook-fn mb)
; 	(cond 
; 		[(book? mb) ...]
; 		[else ...]))
; examples:
; (define mb_false fasle)
; (define mb_book book3 
;   (make-book 
;     24351387 
;     "Three good books" 
;     "Joe" 
;     "MIT Press" 
;     2419 
;     200
;     20 
;     emptyReorder
;     10))

(define-struct line-item (isbn quantity))
; a LineItem is a (make-line-item NonNegInteger NonNegInteger)
; Interpretation:
;  --isbn: the isbn of the book
;  --quantity: the quantity of the book
; Template:
; (define (line-item-fn lt)
; 	(...
; 		(line-item-isbn lt)
; 		(line-item-quantity lt)))
; EXAMPLE for tests:
(define lt-book0 (make-line-item 61387 2))
(define lt-book1-q2 (make-line-item 45861387 2))
(define lt-book1-q18 (make-line-item 45861387 18))
(define lt-book2-q10 (make-line-item 23841387 10))
(define lt-book2-q2 (make-line-item 23841387 2))
(define lt-book3 (make-line-item 24351387 30))

; an Order is a List of LineItem, it's one of
;  --empty
;  --(cons LineItem ListOfLineItem)
;  Template:
;  (define (order-fn olst)
;  	(cond
;  		[(empty? olst) ...]
;  		[else (... (line-item-fn (first olst))
;        				 (order-fn (rest olst)))]))
; Examples:
(define order0 (list lt-book0))
(define order1 (list lt-book1-q2))
(define order1-n (list lt-book1-q18))
(define order2 (list lt-book1-q2 lt-book2-q10))
(define order2-y2 (list lt-book1-q2 lt-book2-q2))
(define order3 (list lt-book1-q2 lt-book2-q10 lt-book3))


(define-struct book 
  (isbn 
   title 
   author 
   publisher 
   unit_price 
   unit_cost 
   stock_number 
   reorder_status 
   unit_volume))
; a Book is a (make-book NonNegInteger 
;                        String 
;                        String 
;                        String 
;                        NonNegInteger 
;                        NonNegInteger 
;                        NonNegInteger 
;                        ReorderStatus 
;                        NonNegReal)
; Interpretation:
;  --isbn, an NonNegInteger (the "international standard book number"). 
;    This serves as a unique identifier for this book. 
;  	 There is an official data definition for ISBN's, but we will simply 
;    model them as integers.
;  --title, a string, the book's title
;  --author, a string, the book's author
;  --publisher, a string, the book's publisher
;  --unit_price: a NonNegInteger, the price at which we will sell the book 
;    (in USD*100, ie $14.99 is represented as 1499).
;  --unit_cost: a NonNegInteger, the cost of the book to the bookstore in 
;    USD*100, also a non-negative integer
;  --stock_number: NonNegInteger of copies on hand, also a NonNegInteger
;  --ReorderStatus: a ReorderStatus, contains the infomation about the book's 
;    reorder
;  --unit_volume: a real the volume taken up by one unit of this item, in 
;    cubic feet, also NonNegInteger
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

(define reorder1 (make-reorder 15 15))
(define reorder1-after-1-day (make-reorder 14 15))
(define reorder2 (make-reorder 10 25))
(define reorder2-after-1-day (make-reorder 9 25))
(define reorder3 (make-reorder 1 15))
(define reorder3-after-1-day false)

(define emptyReorder (make-empty-reorder 1))
(define rs1 reorder1)
(define rs2 emptyReorder)

(define book0 
  (make-book 
   61387 
   "A good book" 
   "Joe" 
   "NU Press" 
   1409 
   1000 
   12 
   reorder1 
   10))
(define book1 
  (make-book 
   45861387 
   "A good book" 
   "Joe" 
   "NU Press" 
   1409 
   1000 
   12 
   reorder1 
   10))
(define book2 
  (make-book 
   23841387 
   "Two good books" 
   "Joe" 
   "NU Press" 
   1419 
   900 
   2 
   reorder2 
   10))
(define book3 
  (make-book 
   24351387 
   "Three good books" 
   "Joe" 
   "MIT Press" 
   2419 
   2000 
   20 
   emptyReorder 
   10))
(define book4 
  (make-book 
   23861387 
   "Four good books" 
   "key" 
   "BU Press" 
   1419 
   900 
   2 
   reorder3 
   10))

(define book0-after-1-day 
  (make-book 
   61387 
   "A good book" 
   "Joe" 
   "NU Press" 
   1409 
   1000 
   12 
   reorder1-after-1-day 
   10))
(define book1-after-1-day 
  (make-book 
   45861387 
   "A good book" 
   "Joe" 
   "NU Press" 
   1409 
   1000 
   12 
   reorder1-after-1-day 
   10))
(define book2-after-1-day 
  (make-book 
   23841387 
   "Two good books" 
   "Joe" 
   "NU Press" 
   1419 
   900 
   2 
   reorder2-after-1-day 
   10))
(define book3-after-1-day 
  (make-book 
   24351387 
   "Three good books" 
   "Joe" 
   "MIT Press" 
   2419 
   2000 
   20 
   emptyReorder 
   10))
(define book4-after-1-day 
  (make-book 
   23861387 
   "Four good books"
   "key" 
   "BU Press" 
   1419 
   900 
   17 
   emptyReorder 
   10))

(define book1-sold-3 (make-book 
                      45861387 
                      "A good book" 
                      "Joe" 
                      "NU Press" 
                      1409 
                      1000 
                      9 
                      reorder1 
                      10))

(define book1-filled-order 
  (make-book 45861387 
             
             "A good book" 
             "Joe" 
             "NU Press" 
             1409 
             1000 
             10 
             reorder1 
             10))
(define book2-filled-order 
  (make-book 23841387 
             "Two good books" 
             "Joe" 
             "NU Press" 
             1419 
             900 
             0 
             reorder2 
             10))
(define book3-filled-order 
  (make-book 24351387 
             "Three good books" 
             "Joe" 
             "MIT Press" 
             2419 
             2000 
             20 
             emptyReorder 
             10))

(define book1-increased 
  (make-book 
   45861387 
   "A good book" 
   "Joe" 
   "NU Press" 
   (round (* 1409 1.1)) 
   1000 
   12 
   reorder1 
   10))
(define book2-increased 
  (make-book 
   23841387 
   "Two good books" 
   "Joe" 
   "NU Press" 
   (round (* 1419 1.1)) 
   900 
   2 
   reorder2 
   10))
(define book3-increased 
  (make-book 
   24351387 
   "Three good books" 
   "Joe" 
   "MIT Press" 
   (round (* 1.1 2419)) 
   2000 
   20 
   emptyReorder 
   10))

; an Inventory is a ListOf<Book>(LOB), which is one of
;  --empty
;  --(cons Book ListOfBooks)
; WHERE: Every Book in the list has a different isbn
; Template:
; LOB-fn: LOB -> ??
; (define (LOB-fn lst)
;   (cond
;     [(empty? lst) ...]
;     [else (... (book-fn (first lst))
;                (LOB-fn (rest lst)))]))

; examples for tests:
(define inventory1 (list book1))
(define inventory2 (list book1 book2))
(define inventory2-filled-order (list book1-filled-order 
                                      book2-filled-order))
(define inventory3 (list book1 book2 book3))
(define inventory-increased-NUPRESS (list book1-increased 
                                          book2-increased book3))

; check-book-isbn: Book NonNegInteger -> Boolean
; GIVEN: a Book and a book's isbn
; RETURNS: #t iff the given book has the given isbn
; examples: see tests below
; STRATEGY: structural decomposition on Book
(define (check-book-isbn b isbn) 
  (= isbn (book-isbn b)))

; inventory-potential-profit : Inventory ->  NonNegInteger
; GIVEN: an inventory
; RETURNS: the total profit, in USD*100, for all the items 
; 	in stock (i.e., how muchthe bookstore would profit if it 
;   sold every book in the inventory)
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (inventory-potential-profit invty)
  (foldr
   ; Book NonNegInteger -> NonNegInteger
   ; GIVEN: a Book and current total price of the processed books
   ; RETURNS: the the sum of the profit of given book and the 
   ;  calculated books
   (lambda (book sum)
     (+ (* (- (book-unit_price book) 
              (book-unit_cost book)) 
           (book-stock_number book)) 
        sum))
   0
   invty))

(begin-for-test
  (check-equal? (inventory-potential-profit inventory2)
                (+ (* (- (book-unit_price book1) (book-unit_cost book1))
                      (book-stock_number book1))
                   (* (- (book-unit_price book2) (book-unit_cost book2))
                      (book-stock_number book2)))
                "wrong profit"))

; inventory-total-volume : Inventory -> NonNegReal
; GIVEN: a Inventory
; RETURNS: the total volume needed to store all the books in stock.
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (inventory-total-volume invty)
  (foldr
   ; Book NonNegReal -> NonNegReal
   ; GIVEN: a Book and the current total volume of processed books
   ; RETURNS: the total-volume for one book in inventory(all the books 
   ;  having the same isbn 
   ;  with the given book)
   (lambda (book total-volume)
     (+ (* (book-stock_number book)
           (book-unit_volume book))
        total-volume))
   0
   invty))

(begin-for-test
  (check-equal? (inventory-total-volume inventory2)
                (+ (* (book-unit_volume book1)
                      (book-stock_number book1))
                   (* (book-unit_volume book2)
                      (book-stock_number book2)))
                "wrong volume"))

; check-line-itme-and-book-isbn: Book LineItem -> Boolean
; GIVEN: a Book and a LineItem
; RETURNS: #t if the GIVEN Book has same isbn with the GIVEN LineItem
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on line-item
(define (check-line-itme-and-book-isbn b lt)
  (check-book-isbn b (line-item-isbn lt)))

; price-for-line-item-helper : Inventory LineItem -> NonNegInteger
; GIVEN: an inventory and a line item
; RETURNS: the price for that line-item (the quantity times the unit
;   price for that item).  Returns 0 if that isbn does not exist in
;   the inventory.
; STRATEGY: structural decomposition on Book
(define (price-for-line-item-helper quantity aBook)
  (* (book-unit_price aBook) quantity))

; calculate-price-for-line-item: Inventory LineItem -> NonNegInteger
; GIVEN: a Inventory and a LineItem
; RETURNS: the price for the line-item(the quantity times the unit
;   price of that book)
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on LineItem
(define (calculate-price-for-line-item invty aLiItm)
  (price-for-line-item-helper 
   (line-item-quantity aLiItm)
   (book-get-from-isbn (line-item-isbn aLiItm) invty)))

; book-in-inventory: NonNegInteger Inventory -> Boolean
; GIVEN: an isbn and a Inventory
; RETURNS: #t if the given inventory has the book having GIVEN isbn
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (book-in-inventory? isbn invty)
  (ormap
   ; Book -> Boolean
   ; GIVEN: a book
   ; RETURNS: #t if the GIVEN book has the GIVEN isbn 
   (lambda (book) 
     (= isbn (book-isbn book)))
   invty))

; price-for-line-item : Inventory LineItem -> MaybeInteger
; GIVEN: an inventory and a line item
; RETURNS: the price for that line-item (the quantity times the unit
;   price for that item).  Returns false if that isbn does not exist in
;   the inventory.
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on LineItem
(define (price-for-line-item invty aLiItm)
  (if (book-in-inventory? (line-item-isbn aLiItm) invty)  
      (calculate-price-for-line-item invty aLiItm)
      false))

(begin-for-test
  (check-equal? (price-for-line-item inventory2 lt-book1-q2)
                (* (book-unit_price book1)
                   (line-item-quantity lt-book1-q2))
                "wrong price")
  (check-equal? (price-for-line-item inventory2 lt-book0)
                false
                "wrong price"))

; book-line-item-on-hand-fillable?: NonNegInteger LineItem -> Boolean
; GIVEN: the stock number of the book and a LineItem
; RETURNS: #t iff the sotck number is greater than the quantity needed 
;   by GIVEN LineItem
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on LineItem
(define (book-line-item-on-hand-fillable?-helper stock aLiItm)
  (> stock (line-item-quantity aLiItm)))

; book-line-item-on-hand-fillable?: Book LineItem -> Boolean
; GIVEN: a Book and a LineItem
; RETURNS: #t iff the given book's sotck number is greater than 
;   the quantity needed by GIVEN LineItem
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (book-line-item-on-hand-fillable? aLiItm invty)
  (ormap
   ; Book -> Boolean
   ; GIVEN: a Book
   ; RETURNS: return #t if the GIVEN book is in the inventory and it's 
   ;  stock-number can fill the line-item
   (lambda (b)  
     (and (check-line-itme-and-book-isbn b aLiItm)
          (book-line-item-on-hand-fillable?-helper 
            (book-stock_number b) 
            aLiItm)))
   invty))

; fillable-now? : Order Inventory -> Boolean.
; GIVEN: an Order and an Inventory
; RETURNS: true iff there are enough copies of each book on hand to fill
; 	the order.  If the order contains a book that is not in the inventory,
; 	then the order is not fillable.
; EXAMPLES: see tests below
; STRATEGY: HOFC
(define (fillable-now? order invty)
  (andmap
   ; LineItem -> Boolean
   ; GIVEN: a LineItem
   ; RETURNS: #t if the line-item is fillable by the books on hand 
   ;    in inventory
   (lambda (aLiItm) 
     (book-line-item-on-hand-fillable? aLiItm invty))
   order))

(begin-for-test
  (check-equal? (fillable-now? order1 inventory2)
                #t
                "should be fillable")
  (check-equal? (fillable-now? order1-n inventory2)
                #f
                "should not be fillable")
  (check-equal? (fillable-now? order0 inventory2)
                #f
                "should not be fillable"))

; book-get-from-isbn: NonNegInteger Inventory -> MaybeBook
; GIVEN: a isbn and a Inventory
; RETURNS: false if the book not in the inventory or the Book 
;   ßhaving given isbn
; EXAMPLES: see tests below
; STRATEGY: HOFC
(define (book-get-from-isbn isbn invty)
  (foldr
   ; Book -> MaybeBook
   ; GIVEN: a Book
   ; RETURNS: the GIVEN book if having the GIVEN isbn else false
   (lambda (book b)
     (if (check-book-isbn book isbn)
         book
         b))
   false
   invty))

; reorder-status-for-quantity-fillable?: NonNegInteger ReorderStatus 
;   NonNegInteger -> Boolean
; GIVEN: the book's stock-number, ReorderStatus and the line-item to fill
; RETURNS: #t iff there's a pending reorder and the reorder-quantity plus the 
;   stock-number of the given book is greater than the given line-item's 
;   quantity
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on ReorderStatus
(define (reorder-status-for-quantity-fillable? stock ros quantity)
  (cond 
    [(boolean? ros) false]
    [else (< quantity (+ stock (reorder-quantity ros)))]))

; book-line-item-available?-helper: 
;   NonNegInteger ReorderStatus LineItem -> Boolean
; GIVEN: the book's stock-number ReorderStatus and a LineItem
; RETURNS: #t iff there's a pending reorder and the reorder-quantity plus 
;   the stock-number of the given book is greater than the given 
;   line-item's quantity or the current stock-number itself can fill the 
;   line-item
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on LineItem
(define (book-line-item-available?-helper stock ros lt)
  (if (< stock (line-item-quantity lt))
      (reorder-status-for-quantity-fillable? stock ros (line-item-quantity lt))
      true))

; book-line-item-available? Book LineItem -> Boolean
; GIVEN: a Book and the a LineItem
; RETURNS: #t iff the given book in the inventory can fill the line-item
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (book-line-item-available? b lt)
  (book-line-item-available?-helper 
   (book-stock_number b) 
   (book-reorder_status b) lt))

; order-all-available?-helper: LineItem Inventory -> Boolean
; GIVEN: a LineItem and a Inventory
; RETURNS: #t iff the inventory has the quantity of book needed by the GIVEN 
;   LineItem
; EXAMPLES: see tests below
; STRATEGY: HOFC
(define (order-all-available?-helper aLiItm invty)
  (ormap
   ; Book -> Boolean
   ; GIVEN: a LineItem and a Inventory
   ; RETURNS: #t if the inventory can fill the given line-item
   (lambda  (book) 
     (and (check-line-itme-and-book-isbn book aLiItm)
          (book-line-item-available? book aLiItm)))
   invty))

; order-all-available?: Order Inventory -> Boolean
; GIVEN: an Order and an Inventory
; RETURNS: #t iff the given inventory have enough books that given Order 
;   requires
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Order
(define (order-all-available? order invty)
  (andmap
   ; LineItem -> Boolean
   ; GIVEN: a LineItem
   ; RETURNS: #t iff the inventory can fill the given line-item
   (lambda  (aLiItm) 
     (order-all-available?-helper aLiItm invty))
   order))

; get-book-reorder-day: Reorder -> NonNegInteger
; GIVEN: a Reorder
; RETURNS: the days needed by the reorder
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Reorder
(define (get-book-reorder-day r)
  (reorder-days r))

; get-reorder-days-line-itme-inventory-helper: Book -> NonNegInteger
; GIVEN: a Book
; WHERE: the book's ReorderStatus is a Reorder(having a pending order)
; RETURNS: the days needed for the reorder's delivery
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (get-reorder-days-line-itme-inventory-helper b)
  (get-book-reorder-day (book-reorder_status b)))

; days-til-fillable-helper: Order Inventory -> MaybeInteger
; GIVEN: an Order and a Inventory
; RETURNS: return the max number of the days the reorders need
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Order
(define (days-til-fillable-helper order invty)
  (foldr
   ; LineItem NonNegInteger -> NonNegInteger
   ; GIVEN: a LineItem and max days need now
   ; WHERE: the order is fillable by the book on hand or by pending
   ;    reorder
   ; RETURNS: the current max days need to get the reorder
   ; STRATEGY: structural decomposition on LineItem
   (lambda (aLiItm days)
     (if (book-line-item-on-hand-fillable? aLiItm invty)
         days
         (max days (get-reorder-days-line-itme-inventory-helper 
                    (book-get-from-isbn (line-item-isbn aLiItm)
                                        invty)))))
   0
   order))

; days-til-fillable : Order Inventory -> MaybeInteger
; GIVEN: an order and an inventory
; RETURNS: the number of days until the order is fillable, assuming all
; the shipments come in on time.  Returns false if there won't be enough
; copies of some book, even after the next shipment of that book comes
; in.
; EXAMPLES: if the order contains one book that is out of stock, with a
; 	reorder status showing 2 days until delivery, then the order is
; 	fillable in 2 days.  If the order is for 10 copies of the book, and
; 	the next order consists of only 5 books, then the function should 
;   return false.
; STRATEGY: function composition
(define (days-til-fillable order invty)
  (if (order-all-available? order invty)
      (days-til-fillable-helper order invty)
      false))

(begin-for-test
  (check-equal? (days-til-fillable order1 inventory2)
                0
                "should be fillable")
  (check-equal? (days-til-fillable order1-n inventory2)
                15
                "should be not fillable")
  (check-equal? (days-til-fillable order3 inventory3)
                false
                "should be not fillable"))

; price-for-order : Inventory Order -> NonNegInteger
; GIVEN: a inventory and a Order
; RETURNS: the total price for the given order, in USD*100.  
;   The price does not depend on whether any particular line item 
;   is in stock.  Line items for an ISBN that is not in the inventory 
;   count as 0.
; STRATEGY: structural decomposition on LineItem
(define (price-for-order invty order)
  (foldr
   ; LineItem NonNegInteger -> NonNegInteger
   ; GIVEN: a LineItem and the current sum of price
   ; RETURNS: the sum plus the price of the line-item
   ; STRATEGY: Function Composition
   (lambda (aLiItm sum) 
     (if (book-in-inventory? (line-item-isbn aLiItm) invty)
         (+ sum (calculate-price-for-line-item invty aLiItm))
         sum))
   0
   order))


(begin-for-test
  (check-equal? 
   (price-for-order inventory2 order2)
   (+ (* (book-unit_price book1) (line-item-quantity lt-book1-q2))
      (* (book-unit_price book2) (line-item-quantity lt-book2-q10)))
   "should be fillable")
  (check-equal? 
   (price-for-order inventory2 order3)
   (+ (* (book-unit_price book1) (line-item-quantity lt-book1-q2))
      (* (book-unit_price book2) (line-item-quantity lt-book2-q10)))
   "should be fillable"))

; sell-book-number: Book NonNegInteger -> Book
; GIVEN: a Book and the number to sell
; where: the number is less than the book's sotck number
; RETURNS: a Book with its stock_number updated
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (sell-book-number b quantity)
  (make-book (book-isbn b)
             (book-title b)
             (book-author b)
             (book-publisher b)
             (book-unit_price b)
             (book-unit_cost b)
             (- (book-stock_number b) quantity)
             (book-reorder_status b)
             (book-unit_volume b)))

(begin-for-test
  (check-equal? (sell-book-number book1 3)
                book1-sold-3
                "selling number is wrong"))

; sell-book-in-line-item: Inventory LineItem -> Book
; GIVEN: an Inventory and a LineItem
; RETURNS: a Book with it's stock-number adjusted
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on LineItem
(define (sell-book-in-line-item invty aLiItm)
  (map
   ; Book -> Book
   ; GIVEN: a Book
   ; RETURNS: the book after filling the line-item
   (lambda (book) 
     (if (check-line-itme-and-book-isbn book aLiItm) 
         (sell-book-number book (line-item-quantity aLiItm))
         book))
   invty))

; inventory-after-order : Inventory Order -> Inventory.
; GIVEN: an order
; WHERE: the order is fillable now
; RETURNS: the inventory after the order has been filled.
; EXAMPLES: see tests below
; STRATEGY: HOFC
(define (inventory-after-order invty order)
  (foldr
   ; LineItem Inventory -> Inventory
   ; GIVEN: a LineItem and a Inventory
   ; RETURNS: the Inventory after the given line-item is filled
   (lambda (aLiItm invty) 
     (sell-book-in-line-item invty aLiItm))
   invty
   order))

(begin-for-test
  (check-equal? (inventory-after-order inventory2 order2-y2)
                inventory2-filled-order
                "filled order"))

; increase-book-price: Book Real -> Book
; GIVEN: a Book and a percentage 
; RETURNS: a Book with its price updated by the percentage, round to 
;   integer since the price 
;   is already in USD*100
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (increase-book-price b increase)
  (make-book 	(book-isbn b)
                   (book-title b)
                   (book-author b)
                   (book-publisher b)
                   (round (* (+ 1 (/ increase 100)) (book-unit_price b)))
                   (book-unit_cost b)
                   (book-stock_number b)
                   (book-reorder_status b)
                   (book-unit_volume b)))

; check-book-publisher: String Book -> Boolean
; GIVEN: the publisher's name and a Book
; RETURNS: #t iff the book's publisher is the GIVEN publisher
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (check-book-publisher publisher b)
  (string=? publisher (book-publisher b)))

; increase-prices : Inventory String Real -> Inventory
; GIVEN: an inventory, a publisher, and a percentage,
; RETURNS: an inventory like the original, except that all items by that
; publisher have their unit prices increased by the specified
; percentage.
; EXAMPLE: (increase-prices inventory1 "MIT Press" 10)
; STRATEGY: HOFC
(define (increase-prices invty publisher increase)
  (map
   ; Book -> Book
   ; GIVEN: a Book
   ; RETURNS: a Book with its price updated if its publisher is the 
   ;    GIVEN publisher
   (lambda (b) 
     (if (check-book-publisher publisher b)
         (increase-book-price b increase)
         b))
   invty))

(begin-for-test
  (check-equal? (increase-prices inventory3 "NU Press" 10)
                inventory-increased-NUPRESS
                "filled order"))


; add-reorder-to-inventory-helper: NonNegInteger Reorder -> NonNegInteger
; GIVEN: a Reorder
; RETURNS: the number of that kind of books currently in inventory
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Reorder
(define (add-reorder-to-inventory-helper stock ros)
  (+ stock (reorder-quantity ros)))


; add-reorder-to-inventory: Book -> Book
; GIVEN: a Book
; RETURNS: the given book with its stock-number and ReorderStatus updated
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (add-reorder-to-inventory b)
  (make-book  (book-isbn b)
              (book-title b)
              (book-author b)
              (book-publisher b)
              (book-unit_price b)
              (book-unit_cost b)
              (add-reorder-to-inventory-helper 
               (book-stock_number b) 
               (book-reorder_status b))
              (make-empty-reorder 1)
              (book-unit_volume b)))

; update-reorder-days-helper: ReorderStatus -> ReorderStatus
; GIVEN: a ReorderStatus
; RETURNS: the updated ReorderStatus
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Reorder
(define (update-reorder-days-helper ro)
  (make-reorder (- (reorder-days ro) 1) (reorder-quantity ro)))

; update-reorder-days: Book -> Book
; GIVEN: a Book
; RETURNS: the GIVEN book with its ReorderStatus updated
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (update-reorder-days b)
  (make-book  (book-isbn b)
              (book-title b)
              (book-author b)
              (book-publisher b)
              (book-unit_price b)
              (book-unit_cost b)
              (book-stock_number b)
              (update-reorder-days-helper (book-reorder_status b))
              (book-unit_volume b)))

; update-reorder: Book Reorder -> Book
; GIVEN: a Book and a Reorder
; RETURNS: the GIVEN book with its Reorder updated
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Reorder
(define (update-reorder b ro)
  (if (> (reorder-days ro) 1)
      (update-reorder-days b)
      (add-reorder-to-inventory b)))

; update-reorder-status-helper: Book ReorderStatus -> ReorderStatus
; GIVEN: a book and its ReorderStatus
; RETURNS: the given book with its ReorderStatus updated
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on ReorderStatus
(define (update-reorder-status-helper b ros)
  (cond
    [(boolean? ros) b]
    [else (update-reorder b ros)]))

; inventory-after-deliveries : Inventory -> Inventory
; GIVEN: today's inventory
; RETURNS: an Inventory representing tomorrow's inventory, in which all
;   reorders that were due in 1 day are now available, and all other
;   reorders have their expected times decreased by 1. 
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (inventory-after-deliveries invty)
  (map
   ; Book -> Book
   ; GIVEN: a Book
   ; RETURNS: the given book with it's ReorderStatus updated
   (lambda (b) 
     (update-reorder-status-helper b (book-reorder_status b)))
   invty))

(begin-for-test 
  (check-equal? (inventory-after-deliveries (list book0 
                                                  book1 
                                                  book2 
                                                  book3 
                                                  book4))
                (list book0-after-1-day 
                      book1-after-1-day 
                      book2-after-1-day 
                      book3-after-1-day 
                      book4-after-1-day)
                "inventory should be updated"))
