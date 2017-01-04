;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |26|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(define all_s (empty-scene 300 300))
(define (draw_circle_at_points lst)
  (place-images
   (tr lst)
   lst
   all_s))


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

(define (tr lst)
  (cond
    [(= (list-length lst) 1) (cons (circle 10 "solid" "blue") empty)]
    [else (cons (circle 10 "solid" "blue") (tr (rest lst)))]
    ))

(tr (list 1 2 4 5 6))


(draw_circle_at_points (list (make-posn 100 100)
                             (make-posn 100 150)
                             (make-posn 200 200)))