;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 21_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

(define all_s (empty-scene 400 600))
(define head (circle 25 "outline" "red"))
(define body (rectangle 60 80 "outline" "red"))
(define left_arm (rectangle 10 70 "outline" "red"))
(define right_arm (rectangle 10 50 "outline" "red"))
(define legs (rectangle 15 80 "outline" "red"))
(define foot (rectangle 20 10 "outline" "red"))



(above head (beside/align "top" left_arm body left_arm) (beside legs legs) (beside foot foot))