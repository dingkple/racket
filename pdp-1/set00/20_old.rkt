;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |20|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require 2htdp/image)

(define all_s (empty-scene 400 600))
(define head (circle 25 "outline" "red"))
(define body (rectangle 60 80 "outline" "red"))
(define left_arm (rectangle 50 10 "outline" "red"))
(define right_arm (rectangle 50 10 "outline" "red"))
(define legs (rectangle 15 80 "outline" "red"))


(place-images 
 (list head
       left_arm
       body
       right_arm
       legs
       legs
       )
 (list (make-posn 190 110)
       (make-posn 120 170)
       (make-posn 190 190)
       (make-posn 260 170)
       (make-posn 176 280)
       (make-posn 206 280)
       )
 all_s)