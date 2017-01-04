#lang racket


(require rackunit)
(require "extras.rkt")

(require "obstacles.rkt")

(check-location "07" "obstacles.rkt")

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(define blob1 '((1 1) (1 2)))
(define board1 '((1 1) (1 3) (2 3)))

(begin-for-test
  ;; this only tests to see if required functions were provided. This
  ;; does not completely test correctness. 
  (check-provided position-set-equal?)
  (check-provided obstacle?)
  (check-provided blocks-to-obstacles)
  )

;; Helper functions
;; cross : (Listof X) (Listof Y) -> (Listof (List X Y))
;; RETURNS: the cartesian product of lists l1 and l2
(define (cross l1 l2)
  (foldr
   append
   empty
   (map
    (lambda (e1)
      (map
       (lambda (e2) (list e1 e2))
       l2))
      l1)))

(define list-10 (build-list 10 add1))
(define big-list (cross list-10 list-10))


(begin-for-test
  (printf "Beginning stress test...~n")
  ;; This could have bad behavior in a naive implementation
  (blocks-to-obstacles big-list)
  
  ;; This could also behave badly
  (obstacle? big-list)
  (printf "Stress test completed~n"))


