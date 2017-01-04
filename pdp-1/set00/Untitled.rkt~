;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))


; a person is a (make-person String String PosNumber PosRealNumber PosRealNumber)
; Interpretation:
;   first-name is the person's first name, usually it's a String just contains
;     a-z or A-Z
;   last-name is the person's last name, usually it's a String just contains
;     a-z or A-Z
;   age is the person's age, it's a PosNumber
;   height is the person's height, which is a PosRealNumber
;   weight is the person's weight, which is a PosRealNumber

(define-struct person (first-name last-name age height weight))

; person-image : person->image
; GIVEN: a person (make-person first-name last-name age height weight)
; RETURN: a image roughly describe the person's image

(define person-image p)