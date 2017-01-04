#lang racket

(require rackunit)
(require "extras.rkt")
(require 2htdp/image)

(require "toys.rkt")

(check-location "10" "toys.rkt")

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))


(begin-for-test
  ;; this only tests to see if required functions were provided. This
  ;; does not completely test correctness. 
  (check-provided World%)
  (check-provided SquareToy%)
  (check-provided CircleToy%)
  (check-provided make-world)
  (check-provided run)
  (check-provided make-square-toy)
  (check-provided make-circle-toy)
  (check-provided StatefulWorld<%>)
  (check-provided StatefulToy<%>)
  )

(define w (make-world 5))
(define c (make-circle-toy 50 50))
(define s (make-square-toy 50 50 8))

(begin-for-test
  (check-provided (make-world 5))
  (check-provided (make-circle-toy 50 50))
  (check-provided (make-square-toy 50 50 8))
  
  (check-provided (send w on-tick))
  (check-provided (send w on-mouse 100 100 "button-down"))
  (check-provided (send w on-key "c"))
  (check-provided (send w on-draw))
  (check-provided (send w target-x))
  (check-provided (send w target-y))
  (check-provided (send w target-selected?))
  (check-provided (send w get-toys))
  
  (check-provided (send c on-tick))
  (check-provided (send c add-to-scene (empty-scene 100 100)))
  (check-provided (send c toy-x))
  (check-provided (send c toy-y))
  (check-provided (send c toy-color))
  
  (check-provided (send s on-tick))
  (check-provided (send s add-to-scene (empty-scene 100 100)))
  (check-provided (send s toy-x))
  (check-provided (send s toy-y))
  (check-provided (send s toy-color)))




