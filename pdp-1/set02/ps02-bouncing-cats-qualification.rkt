;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ps02-bouncing-cats-qualification) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)

(require "two-bouncing-cats.rkt")  

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(define world1 (initial-world 100))
(define cat1 (world-cat1 world1))
(define cat2 (world-cat2 world1))

(define-test-suite bouncing-cats-tests
  ;; this only tests to see if required functions were provided.  Does not test correctness AT ALL
  (check-provided (world-paused? world1))
  (check-provided (cat-x-pos cat1))
  (check-provided (cat-y-pos cat1))
  (check-provided (cat-selected? cat2))
  (check-provided (cat-north? cat1))
  (check-provided (cat-south? cat2))
  (check-provided (cat-east? cat1))
  (check-provided (cat-west? cat2))
  (check-provided (world-after-mouse-event world1 100 200 "button-down"))
  (check-provided (world-after-key-event world1 "x")))

"tests to see if functions are provided:"
(run-tests bouncing-cats-tests) 