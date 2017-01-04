;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |31|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

;; list-length : List -> Number
;; Returns the length of the given list
;; Examples: 
;; (list-length empty) = 0
;; (list-length (list 1)) = 1
;; (list-length (list 1 2 3) = 3
(define (list-length lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (list-length (rest lst)))]))

(define (circle_list lst)
  (cond
    [(= (list-length lst) 1) (cons (circle (first lst) "solid" "blue") empty)]
    [else (cons (circle (first lst) "solid" "blue") (circle_list (rest lst)))]
    ))


(circle_list (list 1 3 5 7 9))