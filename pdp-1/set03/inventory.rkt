;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname inventory) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

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
 make-line-item)

; a NonNegInteger: an Integer which is 0 or greater than 0

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
;  --iff there is a pending reorder, the ReorderStatus is a Reorder
; EXAMPLES: see below
; Template:
; (define (ros-fn ros)
; 	(if (reorder-present? ros) 
; 		  (reorder-fn ros)
; 		  ...))

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

;examples for tests:
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
;  --quantity: the quantity of the book
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
;  		[else (... (line-item-fn (first olst))
;        				 (order-fn (rest olst)))]))
; Examples:
(define order0 (list lt0))
(define order1 (list lt1))
(define order1-n (list lt1-n))
(define order2 (list lt1 lt2))
(define order2-y2 (list lt1-y2 lt2-y2))
(define order3 (list lt1 lt2 lt3))


(define-struct book (isbn title author publisher unit_price unit_cost stock_number reorder_status unit_volume))
; a Book is a (make-book NonNegInteger String String String NonNegInteger NonNegInteger NonNegInteger ReorderStatus NonNegReal)
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

(define book1-sold-3 (make-book 45861387 "A good book" "Joe" "NU Press" 1409 1000 9 reorder1 10))

(define book1-filled-order (make-book 45861387 "A good book" "Joe" "NU Press" 1409 1000 10 reorder1 10))
(define book2-filled-order (make-book 23841387 "Two good books" "Joe" "NU Press" 1419 900 0 reorder2 10))
(define book3-filled-order (make-book 24351387 "Three good books" "Joe" "MIT Press" 2419 2000 20 emptyReorder 10))

(define book1-increased (make-book 45861387 "A good book" "Joe" "NU Press" (round (* 1409 1.1)) 1000 12 reorder1 10))
(define book2-increased (make-book 23841387 "Two good books" "Joe" "NU Press" (round (* 1419 1.1)) 900 2 reorder2 10))
(define book3-increased (make-book 24351387 "Three good books" "Joe" "MIT Press" (round (* 1.1 2419)) 2000 20 emptyReorder 10))

; an Inventory is a List Of Books (LOB) is one of
;  --empty
;  --(cons Book ListOfBooks)
; Every Book in the list has different isbn
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
(define inventory2-filled-order (list book1-filled-order book2-filled-order))
(define inventory3 (list book1 book2 book3))
(define inventory-increased-NUPRESS (list book1-increased book2-increased book3))


; check-book-isbn: Book NonNegInteger -> Boolean
; GIVEN: a Book and a book's isbn
; RETURNS: #t iff the given book has the given isbn
; examples: see tests below
; STRATEGY: structural decomposition on Book
(define (check-book-isbn b isbn) 
  (= isbn (book-isbn b)))

; stock_number-in-inventory: NonNegInteger Inventory -> NonNegInteger
; GIVEN: a NonNegInteger and a Inventory
; RETURNS: the stock number of the book in the given inventory, return 0 iff there's no book has a 
; 	given isbn in the inventory 
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Inventory
(define (stock_number-in-inventory isbn invty)
  (cond
    [(empty? invty) 0]
    [else (if (check-book-isbn (first invty) isbn)
              (book-stock_number (first invty))
              (stock_number-in-inventory isbn (rest invty)))]))

; profit-for-one-book: Book -> NonNegInteger
; GIVEN: a Book
; RETURNS: the profit that can get from selling all the given book in inventory
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (profit-for-one-book b)
  (* (- (book-unit_price b) 
        (book-unit_cost b)) 
     (book-stock_number b)))

; inventory-potential-profit : Inventory ->  NonNegInteger
; GIVEN: an inventory
; RETURNS: the total profit, in USD*100, for all the items in stock (i.e., how much
; 	the bookstore would profit if it sold every book in the inventory)
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Inventory
(define (inventory-potential-profit invty)
  (cond 
    [(empty? invty) 0]
    [else (+ (profit-for-one-book (first invty))
             (inventory-potential-profit (rest invty)))]))

(begin-for-test
  (check-equal? (inventory-potential-profit inventory2)
                (+ (* (- (book-unit_price book1) (book-unit_cost book1))
                      (book-stock_number book1))
                   (* (- (book-unit_price book2) (book-unit_cost book2))
                      (book-stock_number book2)))
                "wrong profit"))

; total-volume-for-one-book: Book -> NonNegReal
; GIVEN: a Book
; RETURNS: the total volume for the given Book(means all the books having the given book's isbn)
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (total-volume-for-one-book b)
  (* (book-stock_number b) (book-unit_volume b)))

; inventory-total-volume : Inventory -> NonNegReal
; GIVEN: a Inventory
; RETURNS: the total volume needed to store all the books in stock.
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Inventory
(define (inventory-total-volume invty)
  (cond
    [(empty? invty) 0]
    [else (+ (total-volume-for-one-book (first invty))
             (inventory-total-volume (rest invty)))]))

(begin-for-test
  (check-equal? (inventory-total-volume inventory2)
                (+ (* (book-unit_volume book1)
                      (book-stock_number book1))
                   (* (book-unit_volume book2)
                      (book-stock_number book2)))
                "wrong volume"))

; check-line-itme-and-book-isbn: Book Line-Item -> Boolean
; GIVEN: a Book and a Line-Item
; RETURNS: #t if the GIVEN Book has same isbn with the GIVEN Line-Item
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on line-item
(define (check-line-itme-and-book-isbn b lt)
  (check-book-isbn b (line-item-isbn lt)))

; calculate-price-for-line-item-helpler: NonNegInteger Line-Item -> NonNegInteger
; GIVEN: the book's price and a Line-Item
; RETURNS: the price for all that line-item(the quantity times the unit
;   price for that item)
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Line-Item
(define (calculate-price-for-line-item-helpler price aLiItm)
  (* price (line-item-quantity aLiItm)))

; calculate-price-for-line-item: Book Line-Item -> NonNegInteger
; GIVEN: a Book and a Line-Item
; RETURNS: the price for all that line-item(the quantity times the unit
;   price for that item)
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (calculate-price-for-line-item b aLiItm)
  (calculate-price-for-line-item-helpler (book-unit_price b) aLiItm))

; price-for-line-item-helper : Inventory Line-Item -> NonNegInteger
; GIVEN: an inventory and a line item
; RETURNS: the price for that line-item (the quantity times the unit
;   price for that item).  Returns 0 if that isbn does not exist in
;   the inventory.
; STRATEGY: structural decomposition on Inventory
(define (price-for-line-item-helper invty aLiItm)
  (cond
    [(empty? invty) 0]
    [else (if (check-line-itme-and-book-isbn (first invty) 
                                             aLiItm)
              (calculate-price-for-line-item (first invty) aLiItm)
              (price-for-line-item-helper (rest invty) aLiItm))]))

; price-for-line-item : Inventory Line-Item -> MaybeInteger
; GIVEN: an inventory and a line item
; RETURNS: the price for that line-item (the quantity times the unit
; price for that item).  Returns false if that isbn does not exist in
; the inventory.
; STRATEGY: structural decomposition on Book and line-item
(define (price-for-line-item invty aLiItm)
  (if (= 0 (price-for-line-item-helper invty aLiItm))
      false
      (price-for-line-item-helper invty aLiItm)))

(begin-for-test
  (check-equal? (price-for-line-item inventory2 lt1)
                (* (book-unit_price book1)
                   (line-item-quantity lt1))
                "wrong price")
  (check-equal? (price-for-line-item inventory2 lt0)
                false
                "wrong price"))

; book-line-item-fillable?: NonNegInteger Line-Item -> Boolean
; GIVEN: the stock number of the book and a Line-Item
; RETURNS: #t iff the sotck number is greater than the quantity needed by GIVEN Line-Item
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Line-Item
(define (book-line-item-fillable-helper stock lt)
  (> stock (line-item-quantity lt)))

; book-line-item-fillable?: Book Line-Item -> Boolean
; GIVEN: a Book and a Line-Item
; RETURNS: #t iff the given book's sotck number is less than the quantity needed by GIVEN Line-Item
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Line-Item
(define (line-itme-fillable-now-helper lt invty)
  (book-line-item-fillable-helper (stock_number-in-inventory (line-item-isbn lt) invty) 
                                  lt))

; fillable-now? : Order Inventory -> Boolean.
; GIVEN: an Order and an Inventory
; RETURNS: true iff there are enough copies of each book on hand to fill
; 	the order.  If the order contains a book that is not in the inventory,
; 	then the order is not fillable.
; STRATEGY: structural decomposition on Order
(define (fillable-now? order invty)
  (cond
    [(empty? order) true]
    [else (if (line-itme-fillable-now-helper (first order) invty)
              (fillable-now? (rest order) invty)
              false)]))

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
; RETURNS: false if the book not in the inventory or the Book having given isbn
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Inventory
(define (book-get-from-isbn isbn invty)
  (cond
    [(empty? invty) false]
    [else (if (check-book-isbn (first invty) isbn)
              (first invty)
              (book-get-from-isbn isbn (rest invty)))]))

; reorder-status-for-quantity-fillable?: NonNegInteger ReorderStatus NonNegInteger -> Boolean
; GIVEN: the book's stock-number, ReorderStatus and the line-item to fill
; RETURNS: #t if there's a pending reorder and the reorder-quantity plus the stock-number of the given book
;   is greater than the given line-item's quantity
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on ReorderStatus
(define (reorder-status-for-quantity-fillable? stock ros quantity)
  (if (reorder-present? ros)
      (< quantity (+ stock (reorder-quantity ros)))
      false))

; book-line-item-available?-helper: NonNegInteger ReorderStatus Line-Item -> Boolean
; GIVEN: the book's stock-number ReorderStatus and a Line-Item
; RETURNS: #t if there's a pending reorder and the reorder-quantity plus the stock-number of the given book
;  is greater than the given line-item's quantity or the current stock-number itself can fill the line-item
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on line-item
(define (book-line-item-available?-helper stock ros lt)
  (if (< stock (line-item-quantity lt))
      (reorder-status-for-quantity-fillable? stock ros (line-item-quantity lt))
      true))

; book-line-item-available? Book Line-Item -> Boolean
; GIVEN: a Book and the a Line-Item
; RETURNS: #t if the given book in the inventory can fill the line-item
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Book
(define (book-line-item-available? b lt)
  (book-line-item-available?-helper (book-stock_number b) (book-reorder_status b) lt))

; order-all-available?-helper: Line-Item Inventory -> Boolean
; GIVEN: a Line-Item and a Inventory
; RETURNS: #t if the inventory has the quantity of book of the GIVEN Line-Item
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Line-Item
(define (order-all-available?-helper lt invty)
  (book-line-item-available? (book-get-from-isbn (line-item-isbn lt) 
                                                 invty) 
                             lt))

; order-all-available?: Order Inventory -> Boolean
; GIVEN: an Order and an Inventory
; RETURNS: #t iff the given inventory have enough books that given Order requires
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Order
(define (order-all-available? order invty)
  (cond
    [(empty? order) true]
    [else (if (order-all-available?-helper (first order) invty)
              (order-all-available? (rest order) invty)
              false)]))


; get-book-reorder-day: ReorderStatus -> NonNegInteger
; GIVEN: a Reorder
; RETURNS: a NonNegInteger
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on ReorderStatus
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

; get-reorder-days-line-itme-inventory: Line-Item Inventory -> NonNegInteger
; GIVEN: a Line-Item and a Inventory
; WHERE: line-item needs more books than the current sotck number and the book's Reorderstatus is a Reorder
; RETURNS: the days needed for the reorder books
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Line-Item
(define (get-reorder-days-line-itme-inventory lt invty)
  (get-reorder-days-line-itme-inventory-helper (book-get-from-isbn (line-item-isbn lt) invty)))

; days-til-fillable-helper: Order Inventory -> MaybeInteger
; GIVEN: an Order and a Inventory
; RETURNS: return the max number of the days the reorders need
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Order
(define (days-til-fillable-helper order invty)
  (cond
    [(empty? order) 0]
    [else (if (not (line-itme-fillable-now-helper (first order) invty))
              (max (get-reorder-days-line-itme-inventory (first order) invty)
                   (days-til-fillable (rest order) invty))
              (days-til-fillable (rest order) invty))]))

; days-til-fillable : Order Inventory -> MaybeInteger
; GIVEN: an order and an inventory
; RETURNS: the number of days until the order is fillable, assuming all
; the shipments come in on time.  Returns false if there won't be enough
; copies of some book, even after the next shipment of that book comes
; in.
; EXAMPLES: if the order contains one book that is out of stock, with a
; 	reorder status showing 2 days until delivery, then the order is
; 	fillable in 2 days.  If the order is for 10 copies of the book, and
; 	the next order consists of only 5 books, then the function should return false.
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
; RETURNS: the total price for the given order, in USD*100.  The price does not
; 	depend on whether any particular line item is in stock.  Line items
; 	for an ISBN that is not in the inventory count as 0.
; STRATEGY: structural decomposition on Order
(define (price-for-order invty order)
  (cond
    [(empty? order) 0]
    [else (+ (price-for-line-item-helper invty (first order))
             (price-for-order invty (rest order)))]))

(begin-for-test
  (check-equal? (price-for-order inventory2 order2)
                (+ (* (book-unit_price book1) (line-item-quantity lt1))
                   (* (book-unit_price book2) (line-item-quantity lt2)))
                "should be fillable")
  (check-equal? (price-for-order inventory2 order3)
                (+ (* (book-unit_price book1) (line-item-quantity lt1))
                   (* (book-unit_price book2) (line-item-quantity lt2)))
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

; sell-book-in-line-item: Inventory Line-Item -> Book
; GIVEN: an Inventory and a Line-Item
; RETURNS: a Book with it's stock-number adjusted
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Line-Item
(define (sell-book-in-line-item b lt)
  (sell-book-number b
                    (line-item-quantity lt)))

; inventory-after-order : Inventory Order -> Inventory.
; GIVEN: an order and Inventory
; WHERE: the order is fillable now
; RETURNS: the inventory after the order has been filled.
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Order
(define (inventory-after-order invty order)
  (cond
    [(empty? order) empty]
    [else (cons (sell-book-in-line-item invty (first order)) 
                (inventory-after-order invty (rest order)))]))

(define (inventory-after-order-helper invty aLiItm)
  (cond
    [(empty? invty) empty]
    [else (if (check-line-itme-and-book-isbn (first invty) aLiItm)
              (cons (sell-book-in-line-item (first invty) aLiItm)
                    (inventory-after-order-helper (rest invty) aLiItm))
              (cons (first invty) (inventory-after-order-helper (rest helper) aLiItm)))]))

(define (inventory-after-order invty order)
  (cond
    [(empty? order) invty]
    [else (inventory-after-order-helper invty (rest order))]))

(begin-for-test
  (check-equal? (inventory-after-order inventory2 order2-y2)
                inventory2-filled-order
                "filled order"))

; increase-book-price: Book Real -> Book
; GIVEN: a Book and a percentage 
; RETURNS: a Book with its price updated by the percentage, round to integer since the price 
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
; RETURNS: #t if the book's publisher is the GIVEN publisher
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
;	returns an inventory like the original, except that all MIT Press
; 	books in the inventory have had their prices increased by 10%.
; 	see tests below
; STRATEGY: structural decomposition on Inventory
(define (increase-prices invty publisher increase)
  (cond
    [(empty? invty) empty]
    [else (if (check-book-publisher publisher (first invty))
              (cons (increase-book-price (first invty) increase)
                    (increase-prices (rest invty) publisher increase))
              (cons (first invty) (increase-prices (rest invty) publisher increase)))]))


(begin-for-test
  (check-equal? (increase-prices inventory3 "NU Press" 10)
                inventory-increased-NUPRESS
                "filled order"))





