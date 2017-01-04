;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;5. 
;sq: Number -> Number
;GIVEN: the number to be compute it's square
;RETURN: the square of the given number
;EXAMPLE:
;(sq 2) => 4
;(sq -4) => 16

(define (sq number)
  (* number number)
 )

(check-expect (sq 2) 4)
(check-expect (sq -4) 16)