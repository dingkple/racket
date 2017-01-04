;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |19|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require 2htdp/image)

; rel-rec-sequence: Number Number -> Rectangle
; Takes two numbers and returns a solid blue rectangle, where the first number is
; the width of the rectangle, and the second number is the proportion of width
; and height of the rectangle to be produced (i.e. height = width * proportion).

(define (rel-rec-sequence width prop)
  (rectangle width 
             (* width prop)
             "solid"
             "blue"
             )
  )

(check-expect (rel-rec-sequence 100 2) (rectangle 100 200 "solid" "blue"))
(check-expect (rel-rec-sequence 100 0.5) (rectangle 100 50 "solid" "blue"))