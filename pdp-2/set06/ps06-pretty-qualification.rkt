;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ps06-outlines-qualification) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

(require "pretty.rkt")

(check-location "06" "pretty.rkt")

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(begin-for-test
  ;; this only tests to see if required functions were provided. 
  ;; This does not test correctness at all.
  (check-provided expr-to-strings)
  (check-provided make-sum-exp)
  (check-provided make-mult-exp)
  (check-provided sum-exp-exprs)
  (check-provided mult-exp-exprs)
  )


