;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;7. 
; circumference : Number -> Number
; GIVEN: the radius of a circle ~ r
; RETURNS: its circumference, using the formula 2 * pi * r.
; Examples:
; (circumference 1)  =>  6.283185307179586 
; (circumference 5)  =>  0

(define (circumference r)
 (* 2 r pi)
 )
(check-within (circumference 1) 6.283185307179586 0.00001)
(check-within (circumference 5) 31.41592653589793 0.00001)