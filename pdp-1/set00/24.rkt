;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |24|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))


;; product : List -> Number
;; Returns the product of the numbers in the given list
;; Examples: 
;; (product empty) = 0
;; (product (list 1)) = 1
;; (product (list 1 2 3 4) = 24
(define (product lst)
  (cond
    [(empty? lst) 0]
    [else (* (first lst) (product_helper (rest lst)))]))



;; product : List -> Number
;; Returns the product of the numbers in the given list
;; Examples: 
;; (product empty) = 0
;; (product (list 1)) = 1
;; (product (list 1 2 3 4) = 24
(define (product_helper lst)
  (cond
    [(empty? lst) 1]
    [else (* (first lst) (product_helper (rest lst)))]))

(check-expect (product (list 1 2 3 4)) 24)
(check-expect (product (list 1)) 1)
(check-expect (product (list)) 0)