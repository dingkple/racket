;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;;FILE NAME: outlines.rkt

(require "extras.rkt")
(require rackunit)

(provide
 nested-rep?
 nested-to-flat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITION

;; An Sexp is one of
;; -- String        Interp: a String represents a string
;; -- NonNegInt     Interp: a NonNegInt represents a non negative integer
;; -- ListOfSexp    Interp: a ListOfSexp represents a list of sexps

;; TEMPLATE
;; sexp-fn : Sexp -> ??
;; (define (sexp-fn s)
;;   (cond
;;    [(string? s) ...] 
;;    [(integer? s) ...]
;;    [else (...
;;           (los-fn s))]))


;; A ListOfSexp (LOS) is one of
;; -- empty
;; -- (cons Sexp ListOfSexp)

;; TEMPLATE
;; los-fn : LOS -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los) ...]
;;     [else (... (sexp-fn (first los))
;;                (los-fn (rest los)))]))


;; A ListOfNestedList is one of
;; -- empty
;; -- (cons NestedList ListOfNestedList)
;; WHERE: ListOfNestedList is a subset of Sexp

;; TEMPLATE
;; lonl-fn : ListOfNestedList -> ??
;; (define (lonl-fn lonl)
;;   (cond
;;     [(empty? lonl) ...]
;;     [else (...
;;             (nl-fn (first lonl))
;;             (lonl-fn (rest lonl)))]))

;; NestedList = (cons String ListOfNestedList)
;; Interp:
;; the first element of NestedList represents the section title (String)
;; the rest of NestedList is a list of nested-lists

;; TEMPLATE
;; nl-fn : NestedList -> ??
;; (define (nl-fn nl)
;;   (... (first nl)
;;        (lonl-fn (rest nl))))

;; A NestedRep is a ListOfNestedList


;; A ListOfFlatList is one of
;; -- empty
;; -- (cons FlatList ListOfFlatList)
;; WHERE: ListOfFlatList is a subset of Sexp

;; TEMPLATE
;; lofl-fn : ListOfFlatList -> ??
;; (define (lofl-fn lofl)
;;   (cond
;;     [(empty? lofl) ...]
;;     [else (...
;;             (fl-fn (first lofl))
;;             (lofl-fn (rest lofl)))]))

;; FlatList = (list NEListOfPosInt String)
;; Interp: 
;; the first element of the FlatList is the section index
;; the second element of the FlatList is the section title

;; TEMPLATE
;; fl-fn : FlatList -> ??
;; (define (fl-fn fl)
;;   (... (nelopi-fn (first fl))
;;        (second fl)))

;; A FlatRep is a ListOfFlatList


;; NEListOfPosInt = (cons PosInt ListOfPosInt)
;; Interp:
;; the first element of the NEListOfNonNegInt is a postive integer
;; the second element of the NEListOfNonNegInt is a list of positive integers

;; TEMPLATE
;; nelopi-fn : NEListOfPosInt -> ??
;; (define (nelopi-fn nelopi)
;;   (... (first nelopi)
;;        (lopi-fn (rest nelopi))))


;; A ListOfPosInt is one of
;; -- empty
;; -- (cons PosInt ListOfPosInt)

;; TEMPLATE
;; lopi-fn : ListOfPosInt -> ??
;; (define (lopi-fn lopi)
;;   (cond
;;     [(empty? lopi) ...]
;;     [else (...
;;             (first lopi)
;;             (lopi-fn (rest lopi)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; Numbers
(define ZERO 0)
(define ONE 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; examples for testing

(define test-list
  '(("The first section"
     ("A subsection with no subsections")
     ("Another subsection"
      ("This is a subsection of 1.2")
      ("This is another subsection of 1.2"))
     ("The last subsection of 1"))
    ("Another section"
     ("More stuff")
     ("Still more stuff"))
    ("Third section"
     ("More stuff"))))

(define result-list
  '(((1) "The first section")
    ((1 1) "A subsection with no subsections")
    ((1 2) "Another subsection")
    ((1 2 1) "This is a subsection of 1.2")
    ((1 2 2) "This is another subsection of 1.2")
    ((1 3) "The last subsection of 1")
    ((2) "Another section")
    ((2 1) "More stuff")
    ((2 2) "Still more stuff")
    ((3) "Third section")
    ((3 1) "More stuff")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nested-rep? : Sexp -> Boolean
;; GIVEN: a sexp
;; RETURNS: true iff it is a nested representation of some outline
;; EXAMPLES: (nested-rep? test-list) => true
;; STRATEGY: structural decomposition on s : Sexp
(define (nested-rep? s)
  (cond
    [(string? s) false]
    [(integer? s) false]
    [else (list-of-nested-list? s)]))

;; list-of-nested-list? : ListOfSexp -> Boolean
;; GIVEN: a list of sexps
;; RETURNS: true iff every element in the list of sexps is the nested-list
;;          representation
;; EXAMPLES: (list-of-nested-list? test-list) => true
;; STRATEGY: structural decomposition on s : Sexp
(define (list-of-nested-list? los)
  (andmap
   ;; Sexp -> Boolean
   ;; GIVEN: a sexp
   ;; WHERE: the given sexp is also a ListOfSexp
   ;; RETURNS: true iff the given sexp can be represented as a nested-list 
   ;; representation
   (lambda (s)
     (if (and (list? s)
              (not (empty? s))
              (string? (first s)))
         (list-of-nested-list? (rest s))
         false))
   los))

;; nested-to-flat : NestedRep -> FlatRep
;; GIVEN: the representation of an outline as a nested list
;; RETURNS: the flat representation of the outline
;; EXAMPLES: (nested-to-flat test-list) => result-list
;; STRATEGY: function composition
(define (nested-to-flat nr)
  (nested-to-flat-inner nr empty ONE))

;; nested-to-flat-inner : NestedRep FlatRep PosInt -> FlatRep
;; GIVEN: a sub nested-rep nr of the original one, a flat-rep fr-til-now
;; and a positive integer level 
;; WHERE: fr-til-now is the the result of flat rep already converted
;;        from nested rep
;;        level represents the nesting level of the given restedrep
;; RETURNS: a flat rep of the outline
;; EXAMPLES: (nested-to-flat-inner test-list empty 1) => result-list
;; STRATEGY: structural decomposition on nl : NestedList
(define (nested-to-flat-inner nr fr-til-now level)
  (foldr
   ;; NestedList FlatRep -> FlatRep
   ;; GIVEN: a nested-list nl and a flat rep ans-til-now
   ;; WHERE: ans-til-now is the current result of converting nestedrep to
   ;;        flatrep
   ;; RETURNS: a flat rep of the outline
   (lambda (nl ans-til-now)
     (cons (list (index-list level) (first nl))
           (nested-to-flat-inner
            (rest nl)
            (fr-til-now-updated ans-til-now level)  
            (+ ONE level))))
   fr-til-now
   nr))

;; index-list: PosInt -> NEListOfPosInt
;; GIVEN: a positive integer
;; RETURNS: a non empty list which contains the given number of 1
;; EXAMPLES: (index-list 2) => (list 1 1)
;; STRATEGY: function composition
(define (index-list level)
  (make-list level ONE))

;; fr-til-now-updated : FlatRep PosInt -> FlatRep
;; GIVEN: a flat rep and a positive integer
;; WHERE: level represents the level of the section
;; RETURNS: a flatrep of which the index part in every flat list has been
;; updated
;; EXAMPLES: (fr-til-now-updated (list (list (list 1 1) "abc")) 2) =>
;; (list (list (list 1 2) "abc"))
;; STRATEGY: structural decomposition on fl : FlatList
(define (fr-til-now-updated fr level)
  (map
   ;; FlatList -> FlatList
   ;; GIVEN: a flatlist
   ;; RETURNS: a flatlist whose index part(first member) is updated
   (lambda (fl)
     (if (nelopi-should-update? (first fl) level)
         (cons (nelopi-updated (first fl) level) (rest fl))
         fl))
   fr))

;; nelopi-should-update? : NEListOfPosInt PosInt -> Boolean
;; GIVEN: a non empty list of positive integers and a positive integer
;; WHERE: the positive integer represents the nesting level of the section
;; RETURNS: true iff the level of the section equals to 1 or the first element
;; in the given list equals to 1
;; EXAMPLES: (nelopi-should-update? (list 1 1 1) 1) => true
;; STRATEGY: function composition
(define (nelopi-should-update? nelopi level)
  (or (= level ONE)
      (= (list-ref nelopi ZERO) ONE)))


;; nelopi-updated : NEListOfPosInt PosInt -> NEListOfPosInt
;; GIVEN: a non empty list of positive integers and a non negative integer
;; representing some index in the given list
;; WHERE: NEListOfPosInt is a subset of ListOfPosInt
;; RETURNS: a non empty list of positive integers like the original one, 
;; except that the positive integer at the index of the given list
;; should be plused one
;; EXAMPLES: (nelopi-updated (list 1 1) 1) => (list 2 1)
;; STRATEGY: HOFC
(define (nelopi-updated nelopi level)
  (foldr
   ;; PosInt NEListOfPosInt -> NEListOfPosInt
   ;; GIVEN: a positive integer and a non empty list of positive integers
   ;; RETURNS: a list of non empty positive integers except that the positive
   ;; integer in the list at the given index should be updated
   (lambda (n ans-til-now)
     (if (pos-in-index-at-level? nelopi ans-til-now level)
         (cons (+ ONE n) ans-til-now)
         (cons n ans-til-now)))
   empty
   nelopi))

;; pos-in-index-at-level? : NEListOfPosInt NEListOfPosInt PosInt -> Boolean
;; GIVEN: a non empty list of positive integers, another non empty list of 
;; positive integers and a positive integer
;; WHERE: the first non empty list of positive integers representing the index 
;;        part of a flat-list
;;        the second non empty list of positive integers representing the 
;;        result list after lopi-updated function to handle rest of the given 
;;        list recursively
;;        the positive integer represents the position needed updating
;;        in the list (nesting level)
;; RETURNS: true iff found the index in the whole list
;; EXAMPLES: (pos-in-index-at-level? (list 1 1) (list 1) 1) => true
;; STRATEGY: function composition 
(define (pos-in-index-at-level? nelopi rest-nelopi level)
  (= (- (length nelopi) (length rest-nelopi)) 
     level))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS

(begin-for-test
  (check-equal? (nested-rep? empty) true
                "if the input sexp is empty, the result should be true, but it 
                returns false")
  (check-equal? (nested-rep? (list "a")) false
                "if the input sexp is (list 'a'), the result should be false, 
                but it returns true")
  (check-equal? (nested-rep? (list "a" "b")) false
                "if the input sexp is (list 'a' 'b'), the result should be 
                false, but it returns true")
  (check-equal? (nested-rep? (list empty)) false
                "if the input sexp is (list empty), the result should be 
                false, but it returns true")
  (check-equal? (nested-rep? (list (list (list "a")))) false
                "if the input sexp is (list (list (list 'a')))), the result 
                should be false, but it returns true")
  (check-equal? (nested-rep? 123) false
                "a non negative integer is not a nested expression, but 
                it does")
  (check-equal? (nested-rep? "abc") false
                "a string is not a nested expression, but it does")
  (check-equal? (nested-rep? test-list) true
                "the expression should be nested expression, but it does not")
  (check-equal? (nested-rep? result-list) false
                "the expression should not be nested expression, but it does")
  (check-equal? (nested-to-flat test-list) result-list
                "the nested expression should convert to flat expression, but 
                it does not"))



