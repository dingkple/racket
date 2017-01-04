;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;10.
;sum_of_larger: Number Number Number -> Number
;GIVEN: three number as a, b, c
;RETURN: sum of the larger two number in a,b,c.
;EXAMPLES:
;(sum_of_larger 1 2 4) -> 6
;(sum_of_larger 12 5 8) -> 20

(define (sum_of_larger a b c)
  (if (> (if (> a b) 
             b 
             a) 
         c) 
      (+ a b) 
      (+ (if (< a b) b a) c))) 

(check-expect (sum_of_larger 1 2 4) 6)
(check-expect (sum_of_larger 12 5 8) 20)
(check-expect (sum_of_larger 6 7 1) 13)
