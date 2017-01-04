;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snacks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)

(provide
 initial-machine
 machine-next-state
 machine-chocolates
 machine-carrots
 machine-bank
 )



; DATA DEFINITION:
; a machine is a (make-machine NonNegInt NonNegInt NonNegInt NonNegInt)
; Interp:
;  --chocolate is the number of current chocolates in machine
;  --carrot is the number of current carrots in machine
;  --bank is the current money in cents in machine from selling things
;  --cus_money is the money customer put into the machine in cents
; TEMPLATE:
; (define (machine-fn machine)
;   (...
;     (machine-chocolates machine)
;     (machine-carrots machine)
;     (machine-bank machine)
;     (machine-cus_money machine)
;   ))
(define-struct machine (chocolates carrots bank cus_money))

; the constants of chocolate's price and carrot's price
(define CHOCOLATE_PRICE 175)
(define CARROT_PRICE 70)



; initial-machine : NonNegInt NonNegInt -> machine
; GIVEN: the number of chocolate and the number of carrot
; RETURN: a machine with GIVEN numbers of chocolate and carrot
; EXAMPLE:
;  (initial-machine 0 0)
;  (initial-machine 10 5)
; STRATEGY: Function Composition
(define (initial-machine chocolate carrot)
  (make-machine chocolate carrot 0 0)
  )

(begin-for-test
  (check-equal? (initial-machine 0 0) (make-machine 0 0 0 0))
  (check-equal? (initial-machine 10 5) (make-machine 10 5 0 0))
  )


; buy_one_chocolate: machine
; GIVEN: a machine
; RETURN: a machine after responsing to the buying request
; EXAMPLE:
;  (buy_one_chocolate (make-machine 10 9 175 176)) -> (make-machine 9 9 350 1)
;  (buy_one_chocolate (make-machine 10 9 175 150)) -> (make-machine 10 9 175 150)
;  (buy_one_chocolate (make-machine 0 9 350 200)) -> (make-machine 0 9 350 200)
; STRATEGY: Structural Decomposition [machine]
(define (buy_one_chocolate m)
  (if (and (> (machine-cus_money m) CHOCOLATE_PRICE)
           (> (machine-chocolates m) 0))
      (make-machine (- (machine-chocolates m) 1)
                    (machine-carrots m)
                    (+ (machine-bank m) CHOCOLATE_PRICE)
                    0
                    )
      m))

; buy_one_carrot: machine
; GIVEN: a machine
; RETURN: a machine after responsing to the buying request
; EXAMPLE:
;  (buy_one_carrot (make-machine 10 9 175 176)) -> (make-machine 10 8 245 106)
;  (buy_one_carrot (make-machine 10 9 175 50)) -> (make-machine 10 9 175 50)
;  (buy_one_carrot (make-machine 10 0 350 200)) -> (make-machine 10 9 350 200)
; STRATEGY: Structural Decomposition [machine]
(define (buy_one_carrot m)
  (if (and (> (machine-cus_money m) CARROT_PRICE)
           (> (machine-carrots m) 0))
      (make-machine (machine-chocolates m)
                    (- (machine-carrots m) 1)
                    (+ (machine-bank m) CARROT_PRICE)
                    0
                    )
      m))

; release : machine -> machine
; GIVEN: a machine with customer's money returned
; RETURN: a machine which return the change iff any to the customer
; EXAMPLE:
; (release (make-machine 10 8 1400 40)) -> (make-machine 10 8 1400 0)
; (release (make-machine 0 0 1750 0)) -> (make-machine 10 8 1400 0)
; STRATEGY: Structural Decomposition [machine]
(define (release m)
  (make-machine (machine-chocolates m)
                (machine-carrots m)
                (machine-bank m)
                0)
  )
(begin-for-test 
  (check-equal? (release (make-machine 10 8 1400 40)) (make-machine 10 8 1400 0))
  (check-equal? (release (make-machine 10 8 1400 0)) (make-machine 10 8 1400 0))
  )


; add_money : machine PosInt -> machine
; GIVEN: a machine and the money to insert in
; RETURN: a machine with money inserted in
; STRATEGY: Structural Decomposition [machine]
(define (add_money m money)
  (make-machine (machine-chocolates m)
                (machine-carrots m)
                (machine-bank m)
                (+ money (machine-cus_money m))))

; machine-next-state : machine CMD
; GIVEN: a machine and a CMD(it's either a PosInt or a String from {"chocolate" "carrot" "release"})
; RETURN: a machine
; EXAMPLE:
;   (machine-next-state (make-machine 10 9 100 50) 50) -> (make-machine 10 9 100 100) 
;   (machine-next-state (make-machine 10 9 175 176) "chocolate") -> (make-machine 9 9 350 1) 
;   (machine-next-state (make-machine 10 9 175 150) "chocolate") -> (make-machine 10 9 175 150) 
;   (machine-next-state (make-machine 0 9 350 200) "chocolate") -> (make-machine 0 9 350 200) 
;   (machine-next-state (make-machine 10 9 175 176) "carrot") -> (make-machine 10 8 245 106)
;   (machine-next-state (make-machine 10 9 175 50) "carrot") -> (make-machine 10 9 175 50)
;   (machine-next-state (make-machine 10 0 350 20) "carrot") -> (make-machine 10 0 350 200)
;   (machine-next-state (make-machine 10 10 1000 100) "release") -> (make-machine 10 10 1000 0)
; STRATEGY: Function Compositon

(define (machine-next-state machine cmd)
  (cond
    [(number? cmd) (add_money machine cmd)]
    [(string? cmd) (cond
                     [(string=? "chocolate" cmd) (buy_one_chocolate machine)]
                     [(string=? "carrots" cmd) (buy_one_carrot machine)]
                     [(string=? "release" cmd) (release machine)]
                     )]
    [else machine]
    ))


(begin-for-test 
  (check-equal? (machine-next-state (make-machine 10 9 100 50) 50) (make-machine 10 9 100 100))
  (check-equal? (machine-next-state (make-machine 10 9 175 176) "chocolate") (make-machine 9 9 350 0))
  (check-equal? (machine-next-state (make-machine 10 9 175 150) "chocolate") (make-machine 10 9 175 150))
  (check-equal? (machine-next-state (make-machine 0 9 350 200) "chocolate") (make-machine 0 9 350 200)) 
  (check-equal? (machine-next-state (make-machine 10 9 175 176) "carrots") (make-machine 10 8 245 0))
  (check-equal? (machine-next-state (make-machine 10 9 175 50) "carrots") (make-machine 10 9 175 50))
  (check-equal? (machine-next-state (make-machine 10 0 350 200) "carrots") (make-machine 10 0 350 200))
  (check-equal? (machine-next-state (make-machine 10 10 1000 100) "release") (make-machine 10 10 1000 0))
  )














