;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ps05-qualification) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

(require "trees.rkt")

(check-location "05" "trees.rkt")


;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(define world-1 (initial-world 0))
(define node-1 (first (world-to-roots (world-after-key-event world-1 "t"))))

(begin-for-test
  ;; this only tests to see if required functions were provided.  Does
  ;; not completely test correctness. 
  (check-provided (world-after-mouse-event world-1 100 100 "button-down"))
  (check-provided (world-to-roots world-1))
  (check-provided (node-to-center node-1))
  (check-provided (node-to-sons node-1))
  (check-provided (node-to-selected? node-1))
  (check-provided run))
