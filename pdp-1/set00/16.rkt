;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |16|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require 2htdp/image)

(define husky00 (bitmap "pictures/husky00.jpg"))
(define husky01 (bitmap "pictures/husky01.jpg"))

(above (beside husky00 husky00) husky01)