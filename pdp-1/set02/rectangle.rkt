;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rectangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")

(provide
  run
  initial-world
  world-x
  world-y
  world-selected?
  world-after-mouse-event)

; RECT_WIDTH means the rectangle's length
; RECT_HIGHT means the rectangle's width
(define RECT_WIDTH 100)
(define RECT_HIGHT 60)

; CIRCLE_RADIUS means the circle's radius
(define CIRCLE_RADIUS 5)

(define CENTER_WIDTH 200)
(define CENTER_HEIGHT 150)

(define SCENE_WIDTH 400)
(define SCENE_HIGHT 300)

; SRect, ORect, Scene represent the solid rectangle, ouline rectangle and the canvas respectively
(define SRect (rectangle RECT_WIDTH RECT_HIGHT "solid" "green"))
(define ORect (rectangle RECT_WIDTH RECT_HIGHT "outline" "green"))
(define Scene (rectangle SCENE_WIDTH SCENE_HIGHT "solid" "white"))

; a World is a (make-world Int Int NonNegInt NonNegInt Boolean)
; --x_vector is the x-value of rectangle postion vector(from the current mouse's position to the orignial rectangle's)
; --y_vector is the y-value of rectangle postion vector(from the current mouse's position to the orignial rectangle's)
; --x is the current rect's x-coordinate
; --y is the current rect's y-coordinate
; --selected? indicates whether the rectangle is selected
; Template:
; (define (world-fn ...)
; 	(...
; 		(world-x_vector w)
; 		(world-y_vector w)
; 		(world-x w)
; 		(world-y w)
; 		(world-selected? w)))
(define-struct world (x_vector y_vector x y selected?))


(define world-rect-at-center (make-world 0 0 CENTER_WIDTH CENTER_HEIGHT false))
(define world-rect-at-center_selected (make-world 0 0 CENTER_WIDTH CENTER_HEIGHT true))


; display-SRect: NonNegInt NonNegInt -> Image
; GIVEN: the rectangle's x-coordinate and y-coordinate
; RETURN: the solid rectangle drew on the scene at the given point
; Examples: see tests below
; STRATEGY: Function Composition
(define (display-SRect x y)
  (place-image SRect x y Scene))


; display-ORect: Int Int NonNegInt NonNegInt -> Image
; GIVEN: the rectangle's x_vector y_vector x-coordinate and y-coordinate
; RETURN: the outlined rectangle drew on the scene at the given point
; Examples: see tests below
; STRATEGY: Function Composition
(define (display-ORect x_vector y_vector x y)
  (place-image  (circle CIRCLE_RADIUS "solid" "red") 
                (- x x_vector) 
                (- y y_vector) 
                (place-image ORect x y Scene)))

; world-to-scene: World -> Image
; GIVEN: a World
; RETURN: an image that portrays the given World
; Examples: see tests below
; STRATEGY: Structural Decomposition [World]
(define (world-to-scene w)
  (if
    (world-selected? w) 
    (display-ORect  (world-x_vector w) 
                    (world-y_vector w) 
                    (world-x w) 
                    (world-y w))
    (display-SRect  (world-x w) 
                    (world-y w))))

(begin-for-test
  (check-equal?
    (world-to-scene world-rect-at-center)
    (display-SRect CENTER_WIDTH CENTER_HEIGHT))
  (check-equal?
    (world-to-scene world-rect-at-center_selected)
    (display-ORect 0 0 CENTER_WIDTH CENTER_HEIGHT)))

; inrect: NonNegInt NonNegInt World -> Boolean
; GIVEN: the current mouse's x-coordinate and y-coordinate and a World
; RETURN: #t iff the GIVEN point is in the rectangle
; Examples: see tests below
; STRATEGY: Structural Decomposition [World]
(define (inrect x y w)
  (and (<= (abs (- x (world-x w))) (/ RECT_WIDTH 2))
       (<= (abs (- y (world-y w))) (/ RECT_HIGHT 2))))

(begin-for-test
  (check-equal? (inrect CENTER_WIDTH 150 world-rect-at-center)
                true)
  (check-equal? (inrect 200 190 world-rect-at-center)
                false))

; mouse-event-button-down: World NonNegInt NonNegInt -> World:
; GIVEN: a world, current x-coordinate and y-coordinate
; RETURN: a world after the mouse-event-button-down
; Examples: see tests below
; STRATEGY: Structural Decomposition [World]
(define (mouse-event-button-down w mx my)
  (if (inrect mx my w)
      (make-world (- (world-x w) mx) 
                  (- (world-y w) my) 
                  (world-x w) 
                  (world-y w) 
                  true)
      w))

; mouse-event-button-up: World NonNegInt NonNegInt -> World:
; GIVEN: a World, current x-coordinate and y-coordinate
; RETURN: a World after the mouse-event-button-up
; Examples: see tests below
; STRATEGY: Structural Decomposition [world]
(define (mouse-event-button-up w mx my)
  (if (inrect mx my w)
      (make-world 0 
                  0 
                  (+ mx (world-x_vector w)) 
                  (+ my (world-y_vector w)) 
                  false)
      w))


; mouse-event-drag: world NonNegInt NonNegInt -> World:
; GIVEN: a world, current x-coordinate and y-coordinate
; RETURN: a world after the mouse-event-drag
; Examples: see tests below
; STRATEGY: Structural Decomposition [World]
(define (mouse-event-drag w mx my)
  (if (inrect mx my w) 
      (make-world (world-x_vector w) 
                  (world-y_vector w) 
                  (+ mx (world-x_vector w)) 
                  (+ my (world-y_vector w)) 
                  true)
      w))


; unselect-world: World -> World
; GIVEN: a World
; RETURNS: a World that is not selected
; Examples: see tests below
; STRATEGY: Structural Decomposition [world]
(define (unselect-world w)
  (make-world (world-x_vector w)
              (world-y_vector w)
              (world-x w)
              (world-y w)
              false))

; world-after-mouse-event: world NonNegInt NonNegInt mouseEvent -> World:
; GIVEN: a World, current x-coordinate and y-coordinate, and a mouse event
; RETURN: a World after the mouse event
; Examples: see tests below
; STRATEGY: cases [MouseEvent]
(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev "drag") (mouse-event-drag w mx my)]
    [(mouse=? mev "button-down") (mouse-event-button-down w mx my)]
    [(mouse=? mev "button-up") (mouse-event-button-up w mx my)]
    [else (unselect-world w)]))

(begin-for-test
  (check-equal? 
    (world-after-mouse-event world-rect-at-center (+ 20 CENTER_WIDTH) (+ 10 CENTER_HEIGHT) "drag")
    (make-world 0 0 (+ 20 CENTER_WIDTH) (+ 10 CENTER_HEIGHT) true))

  (check-equal?
    (world-after-mouse-event world-rect-at-center (+ 20 CENTER_WIDTH) (+ 10 CENTER_HEIGHT) "button-down")
    (make-world -20 -10 CENTER_WIDTH CENTER_HEIGHT true))

  (check-equal?
    (world-after-mouse-event world-rect-at-center (+ 20 CENTER_WIDTH) (+ 10 CENTER_HEIGHT) "button-up")
    (make-world 0 0 (+ 20 CENTER_WIDTH) (+ 10 CENTER_HEIGHT) false))

  (check-equal? 
    (world-after-mouse-event world-rect-at-center  (+ 20 CENTER_WIDTH) (+ 40 CENTER_HEIGHT) "drag")
     world-rect-at-center)

  (check-equal?
    (world-after-mouse-event world-rect-at-center (+ 20 CENTER_WIDTH) (+ 40 CENTER_HEIGHT) "button-down")
    world-rect-at-center)

  (check-equal?
    (world-after-mouse-event world-rect-at-center (+ 20 CENTER_WIDTH) (+ 40 CENTER_HEIGHT) "button-up")
     world-rect-at-center)

  (check-equal?
    (world-after-mouse-event world-rect-at-center (+ 20 CENTER_WIDTH) (+ 40 CENTER_HEIGHT) "enter")
    (make-world 0 0 CENTER_WIDTH CENTER_HEIGHT false)))



; initial-world: Anything -> World
; GIVEN: anything
; RETURN: a initial World with the rectangle at the center point
; Examples: see tests below
; STRATEGY: Function Composition
(define (initial-world a)
  (make-world 0 0 CENTER_WIDTH CENTER_HEIGHT  false))

(begin-for-test
  (check-equal? (initial-world 100)
                (make-world 0 0 CENTER_WIDTH CENTER_HEIGHT false)))

; MAIN FUNCTION.

; main : Anything -> World
; GIVEN: the initial World
; EFFECT: runs the simulation, starting with the rectangle at the center point
; RETURNS: the final state of the World
(define (run a)
  (big-bang (initial-world 0)
          (on-mouse world-after-mouse-event)
          (to-draw world-to-scene)))





