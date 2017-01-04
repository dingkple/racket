;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; sets
(require rackunit)

(require "extras.rkt")

(provide my-member? subset? set-equal?)
(provide set-diff)
(provide set-cons set-union)

;; A SetOf<X> is a ListOf<X> with no duplicates

;; note: empty is a SetOf<X>

;; my-member? : X SetOf<X> -> Boolean
;; GIVEN: an X and a set of X's
;; RETURNS: true iff the X is an element of the set
;; STRATEGY: HO Function Combination
#;(define (my-member? x set1)
  (local
    ((define (test elt) (equal? x elt)))
    (ormap test set1)))

(define (my-member? x set1)
  (ormap
   (lambda (elt) (equal? x elt))
   set1))
  
  
(begin-for-test
  (check-true (my-member? 3 (list 1 3 5)))
  (check-false (my-member? 4 (list 1 3 5)))
)

;; subset? : SetOf<X> SetOf<X> -> Boolean
;; STRATEGY: HO Function Combination
#;(define (subset? set1 set2)
  (local
    ((define (test elt) (my-member? elt set2)))
    (andmap test set1)))

(define (subset? set1 set2)
  (andmap
   (lambda (elt) (my-member? elt set2))
   set1))

(begin-for-test
  (check-true (subset? (list 1 3 5) (list 1 3 2 4 5 8)))
  (check-false (subset? (list 1 3 5) (list 1 3 8))))

;; set-equal? : SetOf<X> SetOf<X> -> Boolean
;; STRATEGY: function composition
(define (set-equal? set1 set2)
  (and
   (subset? set1 set2)
   (subset? set2 set1)))

(begin-for-test
  (check-true (set-equal? (list 1 3 5) (list 3 5 1)))
  (check-false (set-equal? (list 1 3 5) (list 1 3 4 5)))
  (check-false (set-equal? (list 1 3 5) (list 1 3 5 7)))
  (check-false (set-equal? (list 1 3 5 7) (list 1 3 5))))

(define (set-diff set1 set2)
  (filter
   (lambda (x) (not (my-member? x set2)))
   set1))

(begin-for-test
  (check set-equal?
         (set-diff (list 1 2 3 4 5)
                   (list 2 4))
         (list 1 3 5)))

;; set-cons : X SetOf<X> -> SetOf<X>
;; STRATEGY: function composition
(define (set-cons x set1)
  (if (my-member? x set1)
      set1
      (cons x set1)))

(begin-for-test
  (check set-equal? (set-cons 1 (list 3 5 7)) (list 3 1 5 7))
  (check set-equal? (set-cons 3 (list 3 5 7)) (list 5 3 7))
)

;; set-union : SetOf<X> SetOf<X> -> SetOf<X>
;; STRATEGY: struct decomp on set1
#;(define (set-union set1 set2)
  (cond
    [(empty? set1) set2]
    [else (set-cons
           (first set1) 
           (set-union (rest set1) set2))]))

(define (set-union set1 set2)
  (foldr
   set-cons
   set2
   set1))

(begin-for-test
  (check set-equal?
    (set-union (list 1 3 5) (list 2 4 3 5 8))
    (list 1 2 3 4 5 8))
)


    

