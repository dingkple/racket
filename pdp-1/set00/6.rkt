;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;6. 
;quadratic-root: Number Number Number -> Number
;GIVEN: the argument a, b and c of the quadratic equation
;RETURN: one of the root of the corresponding quadratic equation
;EXAMPLES:
;(quadratic-root 1 2 1) => -1
;(quadratic-root 1 -3 2) => 2
;(quadratic-root 1 0 1) => 0+1i


(define (quadratic-root a b c)
  (/ (+ (- 0 b) 
        (sqrt (- (* b b) 
                (* 4 
                   (* a c))))) 
     (* 2 a))
  )

(check-expect (quadratic-root 1 2 1)  -1)
(check-expect (quadratic-root 1 -3 2)  2)
(check-expect (quadratic-root 1 0 1) 0+1i)
