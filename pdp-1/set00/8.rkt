;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;8.
;circle-area: Number -> Number
;GIVEN: the radius of the circle ~ r
;RETURNS: the area of the circle, using the formula area = pi * r * r
;EXAMPLES:
;(circle-area 1) =>  3.141592653589793
;(circle-area 5) =>  78.53981633974483
;(circle-area 7) =>  153.93804002589985

(define (circle-area r)
 (* pi r r)
 )

(check-within (circle-area 1) 3.141592653589793 0.00001)
(check-within (circle-area 5) 78.53981633974483 0.00001)
(check-within (circle-area 7) 153.93804002589985 0.00001)