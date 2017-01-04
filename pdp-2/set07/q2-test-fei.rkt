;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2-test-fei) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;; Set07-Question 2-Test
;; Pengfei Tan

(require rackunit)
(require "extras.rkt")
(require "robot.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                               CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; directions:
(define NORTH "north")
(define EAST "east")
(define SOUTH "south")
(define WEST "west")

;; numbers:
(define ZERO 0)
(define ONE 1)
(define TWO 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            DATA DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; direction:
;; A Direction is one of
;; -- "north"    Interpretation: robot goes north
;; -- "east"     Interpretation: robot goes east
;; -- "south"    Interpretation: robot goes south
;; -- "west"     Interpretation: robot goes west
;; Template:
;; direc-fn : Direction -> ??
#;(define (direc-fn d)
    (cond
      [(string=? d NORTH) ...]
      [(string=? d EAST) ...]
      [(string=? d SOUTH) ...]
      [(string=? d WEST) ...]))
;; Examples:
(define d1 NORTH)

;; position:
;; A Position is a (list PosInt PosInt)
;; (x y) represents the position x, y.
;; Note: this is not to be confused with the built-in data type Posn.
;; Template:
;; pos-fn : Position -> ??
#;(define (pos-fn p)
    (...
     (first p)
     (second p)))
;; Examples:
(define pos-1-1 '(1 1))
(define s-5-5 '(1 1))
(define t-5-5 '(4 2))
(define s-5-5-2 '(3 1))
(define t-5-5-2 '(1 5))
(define s-10-10 '(1 1))
(define t-10-10 '(10 10))
(define s-circle '(1 1))
(define t-circle '(6 4))

;; A ListOf<Position> is one of
;; -- empty
;; -- (cons Position ListOf<Position>)
;; Template:
;; lop-fn : ListOf<Position> -> ??
#;(define (lop-fn lop)
    (cond
      [(empty? lop) ...]
      [else (...
             (pos-fn (first lop))
             (lop-fn (rest lop)))]))
;; Examples:
(define block-empty empty)
(define block-5-5 '((1 3) (1 4) (2 1) (2 5) (3 2) (3 3) (3 5) (4 5)
                          (5 1) (5 2) (5 3) (5 4) (5 5)))
(define block-10-10 '((9 7) (8 8) (8 9) (8 10) (10 9) (11 10) (9 11) (10 11)))
(define block-circle '((5 3) (6 3) (7 3) (7 4) (7 5) (6 5) (5 5) (5 4)))

;; move:
;; A Move is a (list Direction PosInt)
;; Interp: a move of the specified number of steps in the indicated direction.
;; Template:
;; move-fn : Move -> ??
#;(define (move-fn m)
    (...
     (direc-fn (first m))
     (second m)))
;; Examples:
(define move1 '("east" 5))

;; A ListOf<Move> is one of
;; -- empty
;; -- (cons Move ListOf<Move>)
;; Template:
;; lom-fn : ListOf<Move> -> ??
#;(define (lom-fn lom)
    (cond
      [(empty? lom) ...]
      [else (...
             (move-fn (first lom))
             (lom-fn (rest lom)))]))
;; Examples:
(define lom1 (list move1))

;; A Plan is a ListOf<Move>
;; WHERE: the list does not contain two consecutive moves in the same direction.
;; Examples:
(define plan1 lom1)
(define plan-5-5 '(("south" 1)
                   ("east" 1)
                   ("south" 2)
                   ("east" 2)
                   ("north" 2)))
(define plan-false false)
(define plan-10-10 '(("east" 9) ("south" 7) ("west" 1) ("south" 2) ("east" 1)))

;; A Maybe<Plan> is one of
;; -- Plan     Interpretation: a list of move that record the path 
;;             from start to target
;; -- false    Interpretation: there's no path from start to target
;; Template:
;; maybep-fn : Maybe<Plan> -> ??
#;(define (maybep-fn mp)
    (cond
      [(false? mp)...]
      [else (...
             (lom-fn mp))]))
;; Examples:
(define maybeplan-false false)
(define maybeplan1 plan1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              TEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS:
(begin-for-test
  (check-equal? (path s-5-5 t-5-5 block-5-5) plan-5-5
                "Find a path from s-5-5 to t-5-5 is plan 5-5.")
  (check-equal? (path s-5-5-2 t-5-5-2 block-5-5) plan-false
                "There's no path from s-5-5-2 to t-5-5-2.")
  (check-equal? (path s-10-10 t-10-10 block-10-10) plan-10-10
                "Find a path from s-10-10 to t-10-10 is plan-10-10.")
  (check-equal? (path s-circle t-circle block-circle) plan-false
                "There's no path from (1 1) to (6 4)."))
