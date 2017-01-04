;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |18|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; the dimensions of the next 2 rectangles: 
; 32*64
; 64*128

; rec-sequence : Number -> String
; GIVEN: a number n, which tell the function to return the nth element of the sequence
; RETURN: a string, in pattern of "a*b", in which a is the width of the rectangle and 
;         and b is the length of the rectangle
; EXAMPLES: 
;   (rec-sequence 3) -> "8*16"
;   (rec-sequence 5) -> "32*64"

(define (rec-sequence n)
  (string-append (number->string (expt 2 n)) "*" (number->string (expt 2 (+ n 1)))))

(check-expect (rec-sequence 3) "8*16")
(check-expect (rec-sequence 5) "32*64")