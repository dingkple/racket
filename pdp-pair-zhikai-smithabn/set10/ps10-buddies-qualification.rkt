#lang racket

(require rackunit)
(require "extras.rkt")
(require 2htdp/image)

(require "buddies.rkt")

(check-location "10" "buddies.rkt")

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))


(begin-for-test
  ;; this only tests to see if required functions were provided. This
  ;; does not completely test correctness. 
  (check-provided World%)
  (check-provided SquareToy%)
  (check-provided make-world)
  (check-provided run)
  (check-provided make-square-toy)
  (check-provided StatefulWorld<%>)
  (check-provided StatefulToy<%>)
  )

(define w (make-world))
(define s (make-square-toy 50 50))

(begin-for-test
  (check-provided (make-world))
  (check-provided (make-square-toy 50 50))
  
  (check-provided (send w on-tick))
  (check-provided (send w on-mouse 100 100 "button-down"))
  (check-provided (send w on-key "s"))
  (check-provided (send w on-draw))
  (check-provided (send w target-x))
  (check-provided (send w target-y))
  (check-provided (send w target-selected?))
  (check-provided (send w target-color))
  (check-provided (send w get-toys))
  
  (check-provided (send s add-to-scene (empty-scene 100 100)))
  (check-provided (send s toy-x))
  (check-provided (send s toy-y))
  (check-provided (send s toy-color))
  (check-provided (send s toy-selected?))
  (check-provided (send s on-mouse 100 100 "button-down"))
  )




