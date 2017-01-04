;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |15|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(define-struct student (id name major))
; A studet is a (make-student Number String String).
; It represents a student
; Interpretaion:
;   id = the id of the student, usually it's a sequence of number.
;   name = student's name, String contains just(a-z or A-Z).
;   major = student's major, normal string.