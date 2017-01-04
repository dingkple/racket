;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)


(define CANVAS-WIDTH 450)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define RADIUS 20)
(define OUTLINE-BALL (circle RADIUS "outline" "red"))
(define SOLID-BALL (circle RADIUS "solid" "red"))
(define CENTER-WIDTH (/ CANVAS-WIDTH 2))
(define CENTER-HEIGHT (/ CANVAS-HEIGHT 2))

(define (get-string i)
  (int->string i))

(place-image (text (number->string 15)  34 "red")
             24
             24
             EMPTY-CANVAS)