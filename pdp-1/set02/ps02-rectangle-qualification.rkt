;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ps02-rectangle-qualification) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)

(require "rectangle.rkt")  

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(define world1 (initial-world 1))

(define-test-suite rectangle-tests
  ;; this only tests to see if required functions were provided.  Does not test correctness AT ALL
  (check-provided (world-x world1))
  (check-provided (world-y world1))
  (check-provided (world-selected? world1))
  (check-provided (world-after-mouse-event world1 200 150 "button-down"))
  (check-equal? (world-selected? world1) false))

"tests to see if functions are provided:"
(run-tests rectangle-tests)
