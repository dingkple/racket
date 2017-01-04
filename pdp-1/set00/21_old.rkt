;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |21|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")

(require 2htdp/image)
; a person is a (make-person String String PosNumber PosRealNumber PosRealNumber)
; Interpretation:
;   first-name is the person's first name, usually it's a String just contains
;     a-z or A-Z
;   last-name is the person's last name, usually it's a String just contains
;     a-z or A-Z
;   age is the person's age, it's a PosNumber
;   height is the person's height in centimeters, which is a PosRealNumber
;   weight is the person's weight in kilograms, which is a PosRealNumber

(define-struct person (first-name last-name age height weight))

(provide person-image)

; person-image : person->image
; GIVEN: a person (make-person first-name last-name age height weight)
; RETURN: a image roughly describe the person's image

(define all_s (empty-scene 400 600))

(define (get_weight_prop height weight)
  (/ weight (- height 105)))

(define (get_height_prop height)
  (/ height 170)
  )

(define (get_head_image p)
  (circle (* 25 (get_height_prop (person-height p))) "outline" "red")
  )

(define (get_arm_image p)
  (rectangle (* 50 (get_height_prop (person-height p))) 
                  (* 10 (get_weight_prop (person-height p) (person-weight p))) 
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

(define (get_head_posn p)
  (make-posn 190 110)
  )

(define (get_body_posn p)
  (make-posn 190 
             (+ 110 
                (/ (image-height (get_head_image p)) 2) 
                (* (/ (image-height (get_body_image p)) 2) 1.3)
                ;(* 15 (get_height_prop (person-height p))))
             )
  ))
  

(define (get_left_arm_posn p)
  (make-posn (- 190
                (/ (image-width (get_body_image p)) 2)
                (* 15 (get_height_prop (person-height p)))
                (/ (image-width (get_arm_image p)) 2)
                )
             
             (+ 110 
                (/ (image-height (get_head_image p)) 2) 
                (/ (image-height (get_body_image p)) 4)
                (* 15 (get_height_prop (person-height p))))
             )
  )


(define (get_right_arm_posn p)
  (make-posn (+ 190
                (/ (image-width (get_body_image p)) 2)
                (* 15 (get_height_prop (person-height p)))
                (/ (image-width (get_arm_image p)) 2)
                )
             
             (+ 110 
                (/ (image-height (get_head_image p)) 2) 
                (/ (image-height (get_body_image p)) 4)
                (* 15 (get_height_prop (person-height p))))
             )
  )


(define (get_left_leg_posn p)
  (make-posn (- 190
                (/ (image-width (get_body_image p)) 4)
                )
             (+ 110 
                (/ (image-height (get_head_image p)) 2) 
                (* (image-height (get_body_image p)) 1.3)
                ;(* 15 (get_height_prop (person-height p)) 2)
                (/ (image-height (get_leg_image p)) 2)
                )))

(define (get_right_leg_posn p)
  (make-posn (+ 190
                (/ (image-width (get_body_image p)) 4)
                )
             (+ 110 
                (/ (image-height (get_head_image p)) 2)
                (* (image-height (get_body_image p)) 1.3)
                ;(* 15 (get_height_prop (person-height p)) 2)
                (/ (image-height (get_leg_image p)) 2)
                )))
           
              
;(circle (* 25 (get_height_prop (person-height p))) "outline" "red")
(define (person-image p)
  (place-images 
   (list (get_head_image p)
         (get_arm_image p)
         (get_body_image p)
         (get_arm_image p)
         (get_leg_image p) 
         (get_leg_image p) 
         )
   (list 
    (get_head_posn p)
    (get_left_arm_posn p)
    (get_body_posn p)
    (get_right_arm_posn p)
    (get_left_leg_posn p)
    (get_right_leg_posn p)
    )
   all_s)
  )

(person-image (make-person "zhikai" "Ding" 24 180 300))