;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname regexp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)

(provide 
 initial-state
 next-state
 accepting-state?
 error-state?
 )


; DATA DEFINITION:
;  a state is one of
;    --ERR
;    --START
;    --P1
;    --P2
;    --END
(define ERR -1)
(define START 0)
(define P1 1)
(define P2 2)
(define END 3)


; initial-state : STATE -> STATE
; GIVEN: a number
; RETURN: the initial state of state
; STRATEGY: Function Composition
(define (initial-state n)
  START
  )

; input_1 : STATE -> STATE
; GIVEN:  a STATE
; RETURN: a STATE
; STRATEGY: Function Composition
(define (input_1 state)
  (if (or (= state START) (= state P1))
      P1
      ERR
      ))

; input_1 : STATE -> STATE
; GIVEN:  a STATE
; RETURN: a STATE
; STRATEGY: Function Composition
(define (input_2 state)
  (if (or (= state P1) (= state P2) (= state START))
      P2
      ERR))

; input_1 : STATE -> STATE
; GIVEN:  a STATE
; RETURN: a STATE
; STRATEGY: Function Composition
(define (input_3 state)
  (if (or (= state P1) (= state P2) (= state START))
      END
      ERR
      ))

; accepting-state?: STATE->Boolean
; GIVEN: a STATE
; RETURN: a Boolean, #t iff the state is a accepting-state
; STRATEGY: Function Composition
(define (accepting-state? state)
  (= state END))

; accepting-state?: STATE->Boolean
; GIVEN: a STATE
; RETURN: a Boolean, #t iff the state is a error-state
; STRATEGY: Function Composition
(define (error-state? state)
  (= state ERR))

; next-state: STATE input -> STATE
; GIVEN: a STATE and input(which is from {"a", "b", "c", "e"})
; RETURN: a STATE
; EXAMPLE:
; (next-state START "a") -> P1
; (next-state START "b") -> P1
; (next-state START "c") -> P2
; (next-state START "d") -> P2
; (next-state START "e") -> END
; (next-state P1 "a") -> P1
; (next-state P1 "b") -> P1
; (next-state P1 "c") -> P2
; (next-state P1 "d") -> P2
; (next-state P1 "e") -> END
; (next-state P2 "a") -> ERR
; (next-state P2 "b") -> ERR
; (next-state P2 "c") -> P2
; (next-state P2 "d") -> P2
; (next-state P2 "e") -> END
; (next-state END "a") -> END
; (next-state END "b") -> END
; (next-state END "c") -> END
; (next-state END "d") -> END
; (next-state END "e") -> END
; STRATEGY: Function Composition
(define (next-state state input)
  (if (key-event? input)
    (cond
      [(= state ERR) ERR]
      [(= state END) ERR]
      [(> (string-length input) 1) state]
      [(or (key=? input "a") (key=? "b" input)) (input_1 state)]
      [(or (key=? input "c") (key=? "d" input)) (input_2 state)]
      [(key=? input "e") (input_3 state)]
      [else ERR]
      )
    state))

(begin-for-test
  (check-equal? (next-state START "a") P1)
  (check-equal? (next-state START "b") P1)
  (check-equal? (next-state START "c") P2)
  (check-equal? (next-state START "d") P2)
  (check-equal? (next-state START "e") END)
  (check-equal? (next-state START "w") ERR)
  (check-equal? (next-state START "ewww") START)
  (check-equal? (next-state P1 "a") P1)
  (check-equal? (next-state P1 "b") P1)
  (check-equal? (next-state P1 "c") P2)
  (check-equal? (next-state P1 "d") P2)
  (check-equal? (next-state P1 "e") END)
  (check-equal? (next-state P1 "w") ERR)
  (check-equal? (next-state P1 "ewww") P1)
  (check-equal? (next-state P2 "a") ERR)
  (check-equal? (next-state P2 "b") ERR)
  (check-equal? (next-state P2 "c") P2)
  (check-equal? (next-state P2 "d") P2)
  (check-equal? (next-state P2 "e") END)
  (check-equal? (next-state P2 "w") ERR)
  (check-equal? (next-state P2 "ewww") P2)
  (check-equal? (next-state END "a") ERR)
  (check-equal? (next-state END "b") ERR)
  (check-equal? (next-state END "c") ERR)
  (check-equal? (next-state END "d") ERR)
  (check-equal? (next-state END "ewww") END)
  (check-equal? (next-state END "w") ERR)
)
