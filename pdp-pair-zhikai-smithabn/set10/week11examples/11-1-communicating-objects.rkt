#lang racket

(require rackunit)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Balls Communicating with an external agent
;;; this is considered poor OO design
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Ball0<%>
  (interface ()
    ;; -> Integer
    ;; return x, y coords of center and radius, all in pixels
    get-x
    get-y
    get-r))

(define Ball0%
  (class* object% (Ball0<%>)
    (init-field x y r)  ; interpretation omitted...
    (super-new)
    (define/public (get-x) x)
    (define/public (get-y) y)
    (define/public (get-r) r)))

;; Ball0<%> Ball0<%> -> Boolean
;; Do these two balls intersect?
(define (intersects? b1 b2)
  (intersect-helper
    (send b1 get-x) (send b1 get-y) (send b1 get-r)
    (send b2 get-x) (send b2 get-y) (send b2 get-r)))

;; Would balls with these coordinates intersect?
(define (intersect-helper x1 y1 r1 x2 y2 r2)
  (<=
    (+ (sqr (- x1 x2)) (sqr (- y1 y2)))
    (sqr (+ r1 r2))))

(begin-for-test

  (check-true
    (let ((ball1 (new Ball0% [x 0][y 0][r 10]))
          (ball2 (new Ball0% [x 0][y 10][r 10])))
      (and
        (intersects? ball1 ball2)
        (intersects? ball2 ball1))))

  (check-false
    (let ((ball1 (new Ball0% [x 0][y 0][r 10]))
          (ball2 (new Ball0% [x 20][y 10][r 10])))
      (or
        (intersects? ball1 ball2)
        (intersects? ball2 ball1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BALLS COMMUNICATING BY PULL
;; computation gets done in this ball
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Ball1<%>
  (interface ()
    ;; -> Integer
    ;; return x, y coords of center and radius, all in pixels
    get-x
    get-y
    get-r

    ;; Ball1<%> -> Boolean
    ;; Does this ball intersect the other one?
    intersects?
    ))

;; A Ball1 is a (new Ball1% [x Integer][y Integer][r Integer])
(define Ball1%
  (class* object% (Ball1<%>)
    (init-field x y r)  ; interpretation omitted...
    (super-new)

    ;; Does the other ball intersect this one?
    (define/public (intersects? other-b)
      (coordinates-intersect?
        (send other-b get-x)
        (send other-b get-y)
        (send other-b get-r)))

    ;; Integer^3 -> Boolean
    ;; GIVEN: the coordinates of some ball
    ;; RETURNS: would that ball intersect this one?
    (define (coordinates-intersect? other-x other-y other-r)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
        (sqr (+ r other-r))))

    (define/public (get-x) x)
    (define/public (get-y) y)
    (define/public (get-r) r)

))

(begin-for-test

  (check-true
    (let ((ball1 (new Ball1% [x 0][y 0][r 10]))
          (ball2 (new Ball1% [x 0][y 10][r 10])))
      (and
        (send ball1 intersects? ball2)
        (send ball2 intersects? ball1))))

  (check-false
    (let ((ball1 (new Ball1% [x 0][y 0][r 10]))
          (ball2 (new Ball1% [x 20][y 10][r 10])))
      (or
        (send ball1 intersects? ball2)
        (send ball2 intersects? ball1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BALLS COMMUNICATING BY PUSH
;;; computation gets done in other-ball
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Ball-Better<%>
  (interface ()

    ;; not needed for this example!
    ;; ;; -> Integer
    ;; ;; return x, y coords of center and radius, all in pixels
    ;; get-x
    ;; get-y
    ;; get-r

    ;; Ball-Better<%> -> Boolean
    ;; does this ball intersect the other one?
    intersects?

    ;; Integer^3 -> Boolean
    ;; Would a ball with the given x,y,r intersect this one?
    coordinates-intersect?
    ))

;; A Ball2 is a (new Ball2% [x Integer][y Integer][r Integer])
(define Ball2% 
  (class* object% (Ball-Better<%>)
    (init-field x y r) ; interpretation omitted...
    (super-new)

    (define/public (intersects? other-b)
      (send other-b coordinates-intersect? x y r))

    ;; Integer^3 -> Boolean

    (define/public (coordinates-intersect? other-x other-y other-r)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
        (sqr (+ r other-r))))
    ))


(begin-for-test

  (check-true
    (let ((ball1 (new Ball2% [x 0][y 0][r 10]))
          (ball2 (new Ball2% [x 0][y 10][r 10])))
      (and
        (send ball1 intersects? ball2)
        (send ball2 intersects? ball1))))

  (check-false
    (let ((ball1 (new Ball2% [x 0][y 0][r 10]))
          (ball2 (new Ball2% [x 20][y 10][r 10])))
      (or
        (send ball1 intersects? ball2)
        (send ball2 intersects? ball1)))))

;; Puzzle: There is a lot of repeated code in these tests.  Can you
;; write a function that generalizes them?

