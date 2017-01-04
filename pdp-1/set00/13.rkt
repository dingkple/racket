;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |13|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(define-struct point (x y)) 

; I think this will generate a point, whose x is true, y is false, 
; since in the definination it does not have a rule about the type
; of the arguement, so its value is a point (make-point true false)
(make-point true false)


; It's value is true
(point-x (make-point true false))