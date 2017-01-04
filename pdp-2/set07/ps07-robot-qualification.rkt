#lang racket

(require rackunit)
(require "extras.rkt")

(require "robot.rkt")

(check-location "07" "robot.rkt")

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(begin-for-test
  ;; this only tests to see if required functions were provided. This
  ;; does not completely test correctness. 
  (check-provided path))

(define board1 (list (list 4 1) (list 4 2) (list 4 3) (list 4 4)
                    (list 1 4) (list 2 4) (list 3 4)))

(define board2 (list 
                (list 2 2) (list 3 2) (list 4 2) (list 2 3)
                (list 4 3) (list 2 4) (list 3 4) (list 4 4)))

(begin-for-test
  (printf "Beginning stress test...~n")
  (path (list 1 1) (list 11 11) empty)
  (path (list 2 2) (list 6 6) board1)
  (path (list 1 1) (list 3 3) board2)
  (printf "Stress test completed~n"))


