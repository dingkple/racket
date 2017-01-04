;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |9|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;9.
;even: Number -> Boolean
;GIVEN: the integer to be check if it's even
;RETURN: if the given integer is even, return true, else return false
;EXAMPLES:
;(myEven 2) -> true
;(myEven 3) -> false

(define (myEven n)
 (= (remainder n 2) 0))

(check-expect (myEven 2) true)
(check-expect (myEven 3) false)