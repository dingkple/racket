;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ps03-balls-in-box-qualification) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)

(require "balls-in-box.rkt")

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(define world-1 (initial-world 0))
(define world-2 (world-after-key-event world-1 "n"))

(define-test-suite balls-in-box-tests
  ;; this only tests to see if required functions were provided.  Does
  ;; not test correctness AT ALL 
  (check-provided (world-after-mouse-event world-1 100 100 "button-down"))
  (check-provided (world-balls world-1))
  (check-provided (ball-x-pos (first (world-balls world-2))))
  (check-provided (ball-y-pos (first (world-balls world-2))))
  (check-provided (ball-selected? (first (world-balls world-2)))))  

(run-tests balls-in-box-tests)