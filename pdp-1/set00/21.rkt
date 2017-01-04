;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |21|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require "extras.rkt")

; Data-defination: 

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

; get_weight_prop: PosRealNumber PosRealNumber -> Number
; GIVEN: the height and weight of the person
; RETURN: the coefficient that should scale relating to the person's BMI, which uses the following
;         fomula to calculate: BMI = (height - 105)
(define (get_weight_prop height weight)
  (/ weight (- height 105)))

; get_height_prop: PosRealNumber PosRealNumber -> Number
; GIVEN: the height of the person
; RETURN: the coefficient of that should scale relating to the height of the people
(define (get_height_prop height)
  (/ height 180)
  )

; get_height_prop: PosRealNumber PosRealNumber -> Number
; GIVEN: the height of the person
; RETURN: the coefficient of that should scale relating to the height of the people
(define (get_head_prop height)
  (/ height 180)
  )

;get_head_image: person->image
; GIVEN: a person (make-person first-name last-name age height weight)
; RETURN: a image roughly describe the person's head
(define (get_head_image p)
  (circle (* 25 
             (get_height_prop (person-height p)) 
             (get_weight_prop (person-height p) (person-weight p))) "outline" "red")
  )

;get_head_image: person->image
; GIVEN: a person (make-person first-name last-name age height weight)
; RETURN: a image roughly describe the person's arm
; exmpale: see tests
(define (get_arm_image p)
  (rectangle (* 10 (get_weight_prop (person-height p) (person-weight p))) 
                  (* 70 (get_height_prop (person-height p))) 
                  "outline" 
                  "red")
  )

;get_head_image: person->image
; GIVEN: a person (make-person first-name last-name age height weight)
; RETURN: a image roughly describe the person's body
(define (get_body_image p)
  (rectangle (* 60 (get_weight_prop (person-height p) (person-weight p))) 
             (* 80 (get_height_prop (person-height p))) 
             "outline" 
             "red"))

;get_head_image: person->image
; GIVEN: a person (make-person first-name last-name age height weight)
; RETURN: a image roughly describe the person's one leg, right or left
(define (get_leg_image p) 
  (rectangle (* 15 (get_weight_prop (person-height p) (person-weight p))) 
             (* 80 (get_height_prop (person-height p))) 
             "outline" 
             "red"))

;get_foot_image: person->image
; GIVEN: a person (make-person first-name last-name age height weight)
; RETURN: a image roughly describe the person's one foot, right or left
(define (get_foot_image p) 
  (rectangle (* 20 (get_weight_prop (person-height p) (person-weight p))) 
             (* 10 (get_height_prop (person-height p))) 
             "outline" 
             "red"))


; person-image : person->image
; GIVEN: a person (make-person first-name last-name age height weight)
; RETURN: a image roughly describe the person's image, including the head, arm body two legs
;         feet
; EXAMPLES:
;     (person-image (make-person "zhikai" "Ding" 24 180 50))
(define (person-image p)
  (above (get_head_image p) 
         (beside/align "top" (get_arm_image p) (get_body_image p) (get_arm_image p))
         (beside (get_leg_image p) (get_leg_image p))
         (beside (get_foot_image p) (get_foot_image p))))
(person-image (make-person "zhikai" "Ding" 24 180 80))