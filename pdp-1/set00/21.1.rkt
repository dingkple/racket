;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |21.1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require "extras.rkt")


; a person is a (make-person String String PosNumber PosRealNumber PosRealNumber)
; Interpretation:
;   first-name is the person's first name, usually it's a String just contains
;     a-z or A-Z
;   last-name is the person's last name, usually it's a String just contains
;     a-z or A-Z
;   age is the person's age, it's a PosNumber
;   height is the person's height in centimeters, which is a PosRealNumber
;   weight is the person's weight in kilograms, which is a PosRealNumber
(provide person-image)
(provide make-person)
(define-struct person (first-name last-name age height weight))

(define foot (rectangle 20 10 "outline" "red"))

; person-image : person->image
; GIVEN: a person (make-person first-name last-name age height weight)
; RETURN: a image roughly describe the person's image

(define all_s (empty-scene 400 600))

(define (get_weight_prop height weight)
  (/ weight (- height 105)))

(define (get_height_prop height)
  (* (/ height 200) 0.7)
  )

(define (get_head_image p)
  (circle (* 25 (get_height_prop (person-height p))) "outline" "red")
  )

(define (get_arm_image p)
  (rectangle (* 10 (get_weight_prop (person-height p) (person-weight p))) 
                  (* 70 (get_height_prop (person-height p))) 
                  "outline" 
                  "red")
  )

(define (get_body_image p)
  (rectangle (* 60 (get_weight_prop (person-height p) (person-weight p))) 
             (* 80 (get_height_prop (person-height p))) 
             "outline" 
             "red"))

(define (get_leg_image p) 
  (rectangle (* 15 (get_weight_prop (person-height p) (person-weight p))) 
             (* 80 (get_height_prop (person-height p))) 
             "outline" 
             "red"))

(define (get_text_image p)
  (text (string-append (person-first-name p) " " (person-last-name p))
        (+ (string-length (person-first-name p)) 1 (string-length (person-last-name p)))
        "blue"))

(define (person-image p)
  (above (get_head_image p) 
         (beside/align "top" (get_arm_image p) (get_body_image p) (get_arm_image p))
         (beside (get_leg_image p) (get_leg_image p))
         (beside foot foot)
         (get_text_image p)))


(person-image (make-person "zhikai" "Ding" 24 180 50))