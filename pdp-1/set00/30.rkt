;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |30|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

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


(define (non_list blst)
  (cond
    [(= (list-length blst) 1) (if (first blst) (cons false empty) (cons true empty))]
    [else (if (first blst) (cons false (non_list (rest blst))) (cons true (non_list (rest blst))))]
    ))


(non_list (list true true true false false))