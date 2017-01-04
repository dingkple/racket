;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |29|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "/Documents/git/pdp-zhikai/set01/22_2.rkt")
(require 2htdp/image)

(define (draw_people plst)
  (cond
    [(empty? plst) empty-image]
    [else (beside/align "bottom" empty-image (person-image (first plst)) (draw_people (rest plst)))]
    ))

(draw_people (list (make-person "zhikai" "Ding" 24 180 80)
                   (make-person "zhikai" "Ding" 24 120 20)
                   (make-person "zhikai" "Ding" 24 280 80)
                   (make-person "zhikai" "Ding" 24 280 180)))