;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname balls-in-box) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
 run
 initial-world
 world-after-mouse-event
 world-after-key-event
 world-balls
 ball-x-pos
 ball-y-pos
 ball-selected?
 world-after-tick)


(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define RADIUS 20)
(define OUTLINE-BALL (circle RADIUS "outline" "red"))
(define SOLID-BALL (circle RADIUS "solid" "red"))
(define CENTER-WIDTH (/ CANVAS-WIDTH 2))
(define CENTER-HEIGHT (/ CANVAS-HEIGHT 2))
(define LEFT -1)
(define RIGHT 1)
(define BALL-TEST-SPEED 8)
(define ZERO 0)
(define TEXT-POSITION 60)
(define TEXT-FONT-SIZE 30)

; a Direction is one of
;  --RIGHT: the ball is moving towards RIGHT
;  --LEFT: the ball is moving towards LEFT
; Template:
; (define (direction-fn d)
;   (cond
;     [(= d RIGHT) ...]
;     [else ...]))
; EXAMPLES: see tests below

(define-struct ball (x-pos y-pos x_vector y_vector selected? direction))
; a Ball is a (make-ball Real Real Real Real Boolean Direction)
; Interpretation:
;  --x is the x-coordinate of the ball
;  --y is the y-coordinate of the ball
;  --x_vector is the x-value of the vetor(from the ball's center to mouse 
;   position)
;  --y_vector is the y-value of the vetor(from the ball's center to mouse 
;   position)
;  --selected? means whether the ball is selected
;  --direction: the direction of the ball moving towards
;  Template:
; (define (ball-fn b)
;  (...
;    (ball-x-pos b)
;    (ball-y-pos b)
;    (ball-x_vector b)
;    (ball-y_vector b)
;    (ball-selected? b)
;    (ball-direction b)))
;EXAMPLES for tests:
(define ball-at-center-unselected 
  (make-ball CENTER-WIDTH
             CENTER-HEIGHT 
             0 
             0 
             #f 
             RIGHT))

(define ball-at-center-selected 
  (make-ball CENTER-WIDTH
             CENTER-HEIGHT 
             0 
             0 
             #t 
             RIGHT))

(define ball-at-center-unselected-after-tick 
  (make-ball (+ BALL-TEST-SPEED CENTER-WIDTH)
             CENTER-HEIGHT 
             0 
             0 
             #f 
             RIGHT))

(define ball-at-center-selected-after-tick 
  (make-ball CENTER-WIDTH
             CENTER-HEIGHT 
             0 
             0 
             #t 
             RIGHT))

(define ball-at-50-50-unselected-right 
  (make-ball 50 50 0 0 false RIGHT))
(define ball-at-50-50-unselected-left 
  (make-ball 50 50 0 0 false LEFT))
(define ball-at-58-50-unselected 
  (make-ball 58 50 0 0 false RIGHT))
(define ball-at-42-50-unselected 
  (make-ball 42 50 0 0 false LEFT))
(define ball-at-150-150-unselected 
  (make-ball 150 150 0 0 false RIGHT))
(define ball-at-50-50-selected 
  (make-ball 50 50 0 0 #t RIGHT))
(define ball-at-150-150-selected 
  (make-ball 150 150 0 0 #t RIGHT))
(define ball-at-250-250-selected 
  (make-ball 150 150 0 0 #t RIGHT))
(define ball-at-right-wall-unselected 
  (make-ball (- CANVAS-WIDTH RADIUS) 150 0 0 false RIGHT))
(define ball-beyong-right-wall-selected 
  (make-ball CANVAS-WIDTH 150 0 0 #t RIGHT))

; (define ball-at-right-wall-selected-one 
;   (make-ball (- CANVAS-WIDTH RADIUS) 150 0 0 #f RIGHT))
; (define ball-beyong-left-wall-selected-one 
;   (make-ball 0 150 0 0 #f LEFT))

(define ball-at-right-wall-unselected-bounced 
  (make-ball (- CANVAS-WIDTH RADIUS) 150 0 0 #f LEFT))
(define ball-at-left-wall-unselected-bounced 
  (make-ball RADIUS 150 0 0 #f RIGHT))
(define ball-beyong-right-wall-unselected 
  (make-ball CANVAS-WIDTH 150 0 0 #f RIGHT))
(define ball-beyong-left-wall-unselected 
  (make-ball 0 150 0 0 #f LEFT))

(define ball-at-left-wall-selected 
  (make-ball RADIUS 150 0 0 #f LEFT))
(define ball-at-right-wall-selected 
  (make-ball (- CANVAS-WIDTH RADIUS) 150 0 0 #f RIGHT))


(define ball-at-left-wall-unselected 
  (make-ball RADIUS 150 0 0 #f LEFT))
(define ball-beyong-left-wall-selected 
  (make-ball 0 150 0 0 #t LEFT))

(define ball-at-top-wall 
  (make-ball 150 RADIUS 0 0 #f RIGHT))
(define ball-beyong-top-wall-selected 
  (make-ball 150 0 0 0 #t RIGHT))

(define ball-at-down-wall 
  (make-ball 150 (- CANVAS-HEIGHT RADIUS) 0 0 #f RIGHT))
(define ball-beyong-down-wall-selected 
  (make-ball 150 CANVAS-HEIGHT 0 0 #t RIGHT))

; unselected-world
(define scene1 
  (place-image 
   (text (number->string 2) TEXT-FONT-SIZE "blue")
   TEXT-POSITION
   TEXT-POSITION
   (place-image 
    OUTLINE-BALL
    (ball-x-pos ball-at-50-50-unselected-right)
    (ball-y-pos ball-at-50-50-unselected-right)
    (place-image OUTLINE-BALL
                 (ball-x-pos ball-at-150-150-unselected)
                 (ball-y-pos ball-at-150-150-unselected)
                 EMPTY-CANVAS))))

; world with one ball selected
(define scene2 
  (place-image 
   (text (number->string 2) TEXT-FONT-SIZE "blue")
   TEXT-POSITION
   TEXT-POSITION
   (place-image OUTLINE-BALL
                (ball-x-pos ball-at-50-50-unselected-right)
                (ball-y-pos ball-at-50-50-unselected-right)
                (place-image 
                 SOLID-BALL
                 (ball-x-pos ball-at-150-150-unselected)
                 (ball-y-pos ball-at-150-150-unselected)
                 EMPTY-CANVAS))))

; a List of Balls(LOB) is one of:
;  --empty
;  --(cons Ball ListOfBalls)
; Template:
; (define (LOB-fn lst)
;   (cond
;     [(empty? lst) ...)]
;     [else (... (first lst)
;                (LOB-fn (rest lst)))]))

(define-struct world (balls num_balls speed paused?))
; a World is a (make-world LOB NonNegInteger PosInt Boolean)
; Interpretation: 
;  --LOB is a list of Balls
;  --num_balls is the number of the balls on the scene
;  --speed is the the balls' moving speed
;  --paused?: #t iff the world is paused
; Template:
; (define (world-fn w)
;   (... 
;    (world-balls w) 
;    (world-num_balls w) 
;    (world-speed w) 
;    (world-paused? w)))
; EXAMPLES for tests:
(define world-balls-2 
  (make-world (list ball-at-50-50-unselected-right 
                    ball-at-150-150-unselected)
              2 
              BALL-TEST-SPEED
              false))

(define world-balls-1-at-center-unselected 
  (make-world (list ball-at-center-unselected) 
              1 
              BALL-TEST-SPEED 
              false))

(define world-balls-1-at-center-selected 
  (make-world (list ball-at-center-selected) 
              1 
              BALL-TEST-SPEED 
              false))

(define world-balls-1-at-center-unselected-after-tick 
  (make-world (list ball-at-center-unselected-after-tick) 
              1 
              BALL-TEST-SPEED 
              false))

(define world-balls-1-at-center-selected-after-tick 
  (make-world (list ball-at-center-selected-after-tick) 
              1 
              BALL-TEST-SPEED 
              false))

(define world-balls-1-at-center-selected-paused 
  (make-world (list ball-at-center-selected) 
              1 
              BALL-TEST-SPEED 
              true))

(define world-balls-1-at-center-unselected-paused 
  (make-world (list ball-at-center-unselected) 
              1 
              BALL-TEST-SPEED 
              true))

(define world-balls-0 
  (make-world empty 
              0 
              BALL-TEST-SPEED 
              false))


(define world-balls-2-select-50 
  (make-world (list ball-at-50-50-selected 
                    ball-at-150-150-selected) 
              2 
              BALL-TEST-SPEED 
              false))

(define world-balls-2-select-150 
  (make-world (list ball-at-50-50-unselected-right 
                    ball-at-150-150-selected) 
              2 
              BALL-TEST-SPEED 
              false))

(define world-balls-2-select-250 
  (make-world (list ball-at-50-50-unselected-right 
                    ball-at-250-250-selected) 
              2 
              BALL-TEST-SPEED 
              false))

(define world-balls-2-at-right-wall 
  (make-world (list ball-at-50-50-unselected-right
                    ball-at-right-wall-unselected) 
              2 
              BALL-TEST-SPEED 
              false))

(define world-balls-2-at-right-wall-after-tick 
  (make-world (list ball-at-58-50-unselected
                    ball-at-right-wall-unselected) 
              2 
              BALL-TEST-SPEED 
              false))

(define world-balls-2-beyond-right-wall 
  (make-world (list ball-at-50-50-unselected-right
                    ball-beyong-right-wall-selected) 
              2 
              BALL-TEST-SPEED 
              false))


(define world-balls-2-at-left-wall 
  (make-world (list ball-at-50-50-unselected-right
                    ball-at-left-wall-unselected) 
              2 
              BALL-TEST-SPEED 
              false))
(define world-balls-2-at-left-wall-after-tick 
  (make-world (list ball-at-58-50-unselected
                    ball-at-left-wall-unselected) 
              2 
              BALL-TEST-SPEED 
              false))
(define world-balls-2-beyond-left-wall 
  (make-world (list ball-at-50-50-unselected-right
                    ball-beyong-left-wall-selected) 
              2 
              BALL-TEST-SPEED 
              false))

(define world-balls-2-at-top-wall 
  (make-world (list ball-at-50-50-unselected-right
                    ball-at-top-wall) 
              2 
              BALL-TEST-SPEED 
              false))
(define world-balls-2-beyond-top-wall 
  (make-world (list ball-at-50-50-unselected-right
                    ball-beyong-top-wall-selected) 
              2 
              BALL-TEST-SPEED 
              false))

(define world-balls-2-at-down-wall 
  (make-world (list ball-at-50-50-unselected-right
                    ball-at-down-wall) 
              2 
              BALL-TEST-SPEED 
              false))
(define world-balls-2-beyond-down-wall 
  (make-world (list ball-at-50-50-unselected-right
                    ball-beyong-down-wall-selected) 
              2 
              BALL-TEST-SPEED 
              false))

(define world-balls-at-50-selected 
  (make-world (list ball-at-50-50-selected) 1 BALL-TEST-SPEED
              false))
(define world-balls-at-150-selected 
  (make-world (list 
               ball-at-150-150-selected) 1 BALL-TEST-SPEED 
                                         false))

(define world-balls-3-at-right-wall-seletcted-one 
  (make-world (list ball-at-center-selected
                    ball-at-50-50-unselected-right 
                    ball-at-right-wall-selected) 
              2 
              BALL-TEST-SPEED 
              false))
(define world-balls-3-at-right-wall-after-tick-bounced 
  (make-world (list ball-at-center-selected
                    ball-at-58-50-unselected 
                    ball-at-right-wall-unselected-bounced) 
              2 
              BALL-TEST-SPEED 
              false))
(define world-balls-3-beyond-right-wall-unselected 
  (make-world (list ball-at-center-selected
                    ball-at-50-50-unselected-right 
                    ball-beyong-right-wall-unselected) 
              2 
              BALL-TEST-SPEED 
              false))
(define world-balls-3-at-left-wall-selected-one 
  (make-world (list ball-at-center-selected
                    ball-at-50-50-unselected-right 
                    ball-at-left-wall-selected) 
              2 
              BALL-TEST-SPEED 
              false))
(define world-balls-3-at-left-wall-after-tick-bounced 
  (make-world (list ball-at-center-selected
                    ball-at-58-50-unselected 
                    ball-at-left-wall-unselected-bounced) 
              2 
              BALL-TEST-SPEED 
              false))
(define world-balls-3-beyond-left-wall-unselected 
  (make-world (list ball-at-center-selected
                    ball-at-50-50-unselected-right 
                    ball-beyong-left-wall-unselected) 
              2 
              BALL-TEST-SPEED 
              false))
(define world-balls-2-at-right-wall-seletcted-one 
  (make-world (list ball-at-50-50-unselected-right
                    ball-at-right-wall-selected) 
              2 
              BALL-TEST-SPEED 
              false))
(define world-balls-2-at-left-wall-selected-one 
  (make-world (list ball-at-50-50-unselected-right
                    ball-at-left-wall-selected) 
              2 
              BALL-TEST-SPEED 
              false))

; main : PosInt PosReal -> World
; GIVEN: the speed of the ball and the frame rate
; EFFECT: runs the simulation, starting from empty scene
; RETURNS: the final state of the world
; STRATEGY: function composition
(define (run initial-speed rate)
  (big-bang (initial-world initial-speed)
            (on-tick world-after-tick rate)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

; initial-world: PosInt -> World
; GIVEN: the speed of the balls
; RETURNS: an empty world without any balls 
; EXAMPLES: see tests below
; STRATEGY: function composition
(define (initial-world s)
  (make-world empty ZERO s false))

; place-is-solid-ball-on-image: Ball Image Boolean -> Image
; GIVEN: a Ball, a image and a boolean
; RETURNS: an image with the given ball draws on the given image, 
;   if is-solid is true, 
;  draw a solid ball, or draw an outline ball
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Ball
(define (place-is-solid-ball-on-image ball image is-solid?)
  (if is-solid?
      (place-image SOLID-BALL
                   (+ (ball-x_vector ball) (ball-x-pos ball))
                   (+ (ball-y_vector ball) (ball-y-pos ball))
                   image)
      (place-image OUTLINE-BALL
                   (+ (ball-x_vector ball) (ball-x-pos ball))
                   (+ (ball-y_vector ball) (ball-y-pos ball))
                   image)))

; draw-balls: LOB -> Image
; GIVEN: a LOB
; RETURNS: a scene(image) with all given balls on it
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Ball
(define (draw-balls blst)
  (foldr  
   ; Ball Image -> Image
   ; GIVEN: a Ball and the current scene
   ; RETURNS: the GIVEN ball drew on the GIVEN scene
   (lambda (b scene)
     (if (ball-selected? b)
         (place-is-solid-ball-on-image b scene #t)
         (place-is-solid-ball-on-image b scene #f)))
   EMPTY-CANVAS
   blst))

; world-to-scene: World -> Image
; GIVEN: a World
; RETURNS: an image describes the whole world
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on World
(define (world-to-scene w)
  (place-image (text (number->string (world-num_balls w)) 
                     TEXT-FONT-SIZE 
                     "blue")
               TEXT-POSITION
               TEXT-POSITION
               (draw-balls (world-balls w))))

(begin-for-test
  (check-equal? (world-to-scene world-balls-2)
                scene1)
  (check-equal? (world-to-scene world-balls-2-select-150)
                scene2))

; world-after-key-event: World KeyEvent -> World
; GIVEN: a World and a KeyEvent
; RETURNS: the given World after the key event
; EXAMPLES: see tests below
; STRATEGY: cases on key event
(define (world-after-key-event w kev)
  (cond 
    [(key=? kev "n") (world-after-n-key w)]
    [(key=? kev " ") (world-after-space-key w)]
    [else w]))

; world-after-n-key: World -> World
; GIVEN: a World
; RETURNS: the given World with one more ball in the world
;   (at the center of canvas towarding right)
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on World
(define (world-after-n-key w)
  (make-world (cons (initial-ball ZERO) (world-balls w))
              (+ 1 (world-num_balls w))
              (world-speed w)
              (world-paused? w)))

; world-after-space-key: World -> World
; GIVEN: a World
; RETURNS: the given world which is paused or unpaused(base on the current status 
;   of the world, eg, unpaused if now puased)
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on World
(define (world-after-space-key w)
  (make-world (world-balls w)
              (world-num_balls w)
              (world-speed w)
              (not (world-paused? w))))

; world-after-mouse-event: 
;     World NonNegInteger NonNegInteger MouseEvent -> World
; GIVEN: a World, the position of the mouse and the mouse evnet
; RETURNS: the given World after the mouse event 
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on World
(define (world-after-mouse-event w mx my mev)
  (make-world (balls-after-mouse-event (world-balls w) mx my mev) 
              (world-num_balls w) 
              (world-speed w) 
              (world-paused? w)))

; balls-after-mouse-event: 
;     LOB NonNegInteger NonNegInteger MouseEvent -> World
; GIVEN: a LOB, the position of the mouse(x-coordinate and y-coordinate) and 
;   a mouseEvent
; RETURNS: the given World after the mouse event
; EXAMPLES: see tests below
; STRATEGY: cases on mouse event
(define (balls-after-mouse-event balls mx my mev)
  (cond 
    [(mouse=? mev "button-down") (balls-after-button-down balls mx my)]
    [(mouse=? mev "button-up") (balls-after-button-up balls)]
    [(mouse=? mev "drag" ) (balls-after-drag balls mx my)]
    [(mouse=? mev "leave") (balls-after-leave balls)]
    [else balls]
    ))

; inball? Ball NonNegInteger NonNegInteger -> Boolean
; GIVEN: a Ball and a position(x-corodinate and y-coordinate)
; RETURNS: #t if the given position is in the ball(including edge)
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Ball
(define (inball? b mx my)
  (<= (+ (sqr (- mx (ball-x-pos b)))
         (sqr (- my (ball-y-pos b))))
      (sqr RADIUS)))

; select-ball: Ball NonNegInteger NonNegInteger -> Ball
; GIVEN: a Ball and a position(x-corodinate and y-coordinate)
; RETURNS: a Ball with its x_vector and y_vector, its position and its 
;   boolean flag for selected updated
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Ball
(define (select-ball b mx my)
  (make-ball mx
             my
             (- (ball-x-pos b) mx)
             (- (ball-y-pos b) my)
             true
             (ball-direction b)))

; unselect-ball: Ball -> Ball
; GIVEN: a Ball
; RETURNS: a Ball with its x_vector and y_vector, its position(x-corodinate, 
;   y-coordinate) and its boolean flag for if selected updated
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Ball
(define (unselect-ball b)
  (make-ball (+ (ball-x_vector b) (ball-x-pos b))
             (+ (ball-y_vector b) (ball-y-pos b))
             ZERO
             ZERO
             false
             (ball-direction b)))

; balls-after-button-down: LOB NonNegInteger NonNegInteger -> LOB
; GIVEN: a LOB and a mouse position(x-corodinate and y-coordinate)
; RETURNS: a LOB with the balls updated if the mouse is in any one of the balls
;     Notice: select all the balls that covers the position of the mosue
; EXAMPLES: see tests below
; STRATEGY: HOFC
(define (balls-after-button-down blst mx my)
  (map 
   ; Ball -> Ball
   ; GIVEN: a Ball
   ; RETURNS: a selected ball if position(mx, my) is within the ball's range
   ;   else return the ball
   (lambda (b)
     (if (inball? b mx my)
         (select-ball b mx my)
         b))
   blst))

; balls-after-button-up: LOB -> LOB
; GIVEN: a LOB
; RETURNS: set all selected ball in the LOB unselected and update their 
;   positions and vectors
; EXAMPLES: see tests below
; STRATEGY: HOFC
(define (balls-after-button-up blst)
  (map unselect-ball blst))

; move-selected-ball: Ball NonNegInteger NonNegInteger -> Ball
; GIVEN: a selected Ball and a mouse position(x-corodinate and y-coordinate)
; RETURNS:  the given ball moved to the given position
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Ball
(define (move-selected-ball b mx my)
  (make-ball mx
             my
             (ball-x_vector b)
             (ball-y_vector b)
             (ball-selected? b)
             (ball-direction b)))

; balls-after-drag: LOB NonNegInteger NonNegInteger -> LOB
; GIVEN: a LOB and a mouse position(x-corodinate and y-coordinate)
; RETURNS: a LOB with the balls selected move to the given position if exists
; EXAMPLES: see tests below
; STRATEGY: HOFC
(define (balls-after-drag blst mx my)
  (map 
   ; Ball -> Ball
   ; GIVEN: a Ball
   ; RETURNS: a Ball moved to the given position(mx, my) if selected else the 
   ;  given ball
   (lambda  (b)
     (if (ball-selected? b)
         (move-selected-ball b mx my)
         b))
   blst))

; balls-after-leave: LOB -> LOB
; GIVEN: a LOB
; RETURNS: a LOB with the balls selected updated if exists
; EXAMPLES: see tests below
; STRATEGY: HOFC
(define (balls-after-leave blst)
  (map 
   ; Ball -> Ball
   ; GIVEN: a Ball
   ; RETURNS: a Ball put back if it's center is out of the canvas
   (lambda  (b)
     (if (ball-selected? b)
         (bounce-ball-position-back-if-need b)
         b))
   blst))

; initial-ball: Any -> Ball
; GIVEN: ignored
; RETURNS: a unselected-ball towards right at center of the canvas
; EXAMPLES: see tests below
; STRATEGY: function compostion
(define (initial-ball ZERO)
  (make-ball CENTER-WIDTH
             CENTER-HEIGHT
             ZERO
             ZERO
             false
             RIGHT))


; bounce-ball-position-back-if-need: Ball -> Ball
; GIVEN: a Ball
; RETURNS: a Ball with its postion adjusted if its center out of canvas
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Ball
(define (bounce-ball-position-back-if-need b)
  (make-ball (bounce-x-position-if-need 
              (+ (ball-x-pos b)
                 (ball-x_vector b)))
             (bounce-y-position-if-need 
              (+ (ball-y-pos b)
                 (ball-y_vector b)))
             ZERO
             ZERO
             false
             (ball-direction b)))

; bounce-y-position-if-need: NonNegReal -> NonNegReal:
; GIVEN: a y-coordinate
; RETURNS: the adjusted y-coordinate if it is out of canvas, put it at the 
;   edge based on where it's now
; EXAMPLES: see tests below
; STRATEGY: function compostion
(define (bounce-y-position-if-need y)
  (cond
    [(< y RADIUS) RADIUS]
    [(> y (- CANVAS-HEIGHT RADIUS)) (- CANVAS-HEIGHT RADIUS)]
    [else y]))

; bounce-x-position-if-need: NonNegReal -> NonNegReal
; GIVEN: a x-position as the ball's center
; RETURNS: adjusted x-position, if the ball is beyond the left or right edge, 
;   put it at the edge of the canvas
; EXAMPLES: see tests below
; STRATEGY: function compostion
(define (bounce-x-position-if-need x)
  (cond 
    [(<= RADIUS x (- CANVAS-WIDTH RADIUS)) x]
    [(< x RADIUS) RADIUS]
    [else (- CANVAS-WIDTH RADIUS)]))

; bounce-direction-if-need-helper: Ball Direction -> Ball
; GIVEN: a Ball and a direction
; WHERE: the ball is unselected
; RETURNS:  the given ball with its direciton changed to the given direction
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Ball
(define (bounce-direction-if-need-helper b direciton)
  (make-ball (ball-x-pos b)
             (ball-y-pos b)
             (ball-x_vector b)
             (ball-y_vector b)
             (ball-selected? b)
             direciton))

; bounce-direction-if-need: Ball -> Ball
; GIVEN: a Ball
; WHERE: the ball is unselected
; RETURNS: a Ball with changed direciton if it's at the edge of the canvas
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Ball
(define (bounce-direction-if-need b)
  (cond
    [(= (ball-x-pos b) RADIUS) (bounce-direction-if-need-helper b RIGHT)]
    [(= (ball-x-pos b) (- CANVAS-WIDTH RADIUS)) (bounce-direction-if-need-helper b LEFT)]
    [else b]))

; move-one-ball-helper: NonNegReal Direction PosInt -> NonNegReal 
; GIVEN: the ball's x-position, its Direction and the balls' speed
; RETURNS: the ball's x-position after  being moved to right or left
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Direction
(define (move-one-ball-helper x d s)
  (cond
    [(= d RIGHT) (+ x s)]
    [else (- x s)]))

; move-one-ball: Ball PosInt -> Ball
; GIVEN: a Ball and the balls' speed
; WHERE: the ball is unselected
; RETURNS: the given Ball at the position after one tick
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Ball
(define (move-one-ball b s)
  (make-ball (bounce-x-position-if-need 
              (move-one-ball-helper (ball-x-pos b) 
                                    (ball-direction b) 
                                    s))
             (ball-y-pos b)
             (ball-x_vector b)
             (ball-y_vector b)
             (ball-selected? b)
             (ball-direction b)))

; one-ball-after-tick: Ball PosInt -> Ball
; GIVEN: a Ball and the its speed
; WHERE: the ball is unselected
; RETURNS: an Ball at the position after one tick and bounced back if at the 
;   edge of the canvas
; EXAMPLES: see tests below
; STRATEGY: function compostion
(define (one-ball-after-tick b s)
  (bounce-direction-if-need 
   (bounce-ball-position-back-if-need 
    (move-one-ball b s))))

; balls-after-tick: LOB PosInt -> LOB
; GIVEN: a LOB containing all the balls in the world and the balls' speed
; RETURNS: the GIVEN LOB at the next tick
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on Ball
(define (balls-after-tick blst s)
  (map 
   ; Ball -> Ball
   ; GIVEN: a Ball
   ; RETURNS: the GIVEN ball at the next tick, moved if unselected and 
   ;  unchanged if selected
   (lambda  (b)
     (if (ball-selected? b)
         b
         (one-ball-after-tick b s)))
   
   blst))

; world-after-tick : World -> World
; GIVEN: a World
; RETURNS: the given World after tick
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on World
(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world (balls-after-tick (world-balls w) (world-speed w))
                  (world-num_balls w)
                  (world-speed w)
                  (world-paused? w))))

(begin-for-test
  (check-equal? (world-after-tick world-balls-1-at-center-unselected)
                world-balls-1-at-center-unselected-after-tick
                "ball unselected should move after one tick"))

(begin-for-test 
  (check-equal?
   (initial-world BALL-TEST-SPEED)
   world-balls-0)
  
  (check-equal? (world-after-key-event world-balls-0 "n")
                world-balls-1-at-center-unselected
                "pressing n should generate a new circle")
  
  (check-equal? (world-after-key-event world-balls-0 "a")
                world-balls-0
                "pressing any key other than 'n' should do nothing")
  
  (check-equal? (world-after-key-event world-balls-1-at-center-unselected " ")
                world-balls-1-at-center-unselected-paused
                "pressing the space key should pause the world")
  
  (check-equal? (world-after-mouse-event world-balls-1-at-center-unselected 
                                         CENTER-WIDTH 
                                         CENTER-HEIGHT 
                                         "button-down")
                world-balls-1-at-center-selected
                "mouse button-down inside the circle should select the ball")
  
  (check-equal? (world-after-mouse-event world-balls-2 250 250 "button-down")
                world-balls-2
                "mouse button-down outside the ball shouldn't select it")
  
  (check-equal? (world-after-mouse-event world-balls-1-at-center-unselected 
                                         CENTER-WIDTH 
                                         CENTER-HEIGHT 
                                         "enter")
                world-balls-1-at-center-unselected
                "any other mouse event would do nothing")
  
  (check-equal? (world-after-mouse-event world-balls-1-at-center-selected 
                                         CENTER-WIDTH 
                                         CENTER-HEIGHT 
                                         "button-up")
                world-balls-1-at-center-unselected
                "mouse button-up should unselected the ball")
  
  (check-equal? (world-after-mouse-event 
                 world-balls-at-50-selected 150 150 "drag")
                world-balls-at-150-selected
                "drag the selected ball should drag it where the mouse is")
  
  (check-equal? (world-after-mouse-event 
                 world-balls-1-at-center-unselected 150 150 "drag")
                world-balls-1-at-center-unselected
                "drag event does nothing to an unselected ball")
  
  (check-equal? (world-after-mouse-event world-balls-2-select-150 
                                         150 
                                         150 
                                         "drag")
                world-balls-2-select-250
                "drag only infects the selected balls")
  
  (check-equal? (world-after-mouse-event world-balls-2-beyond-left-wall 
                                         0 
                                         150 
                                         "leave")
                world-balls-2-at-left-wall-selected-one
                "leaving the canvas should not drag the ball out of the canvas")
  
  (check-equal? (world-after-mouse-event world-balls-2-beyond-right-wall 
                                         0 
                                         150 
                                         "leave")
                world-balls-2-at-right-wall-seletcted-one
                "leaving the canvas should not drag the ball out of the canvas")
  
  (check-equal? (world-after-mouse-event world-balls-2-beyond-top-wall 
                                         150 
                                         0 
                                         "leave")
                world-balls-2-at-top-wall
                "leaving the canvas should not drag the ball out of the canvas")
  
  (check-equal? (world-after-mouse-event world-balls-2-beyond-down-wall 
                                         150 
                                         CANVAS-HEIGHT 
                                         "leave")
                world-balls-2-at-down-wall
                "leaving the canvas should not drag the ball out of the canvas")
  
  (check-equal? (world-after-mouse-event world-balls-1-at-center-unselected 
                                         150 
                                         CANVAS-HEIGHT 
                                         "leave")
                world-balls-1-at-center-unselected
                "leaving the canvas should not drag the ball out of the canvas")
  
  (check-equal? (world-after-tick world-balls-3-beyond-left-wall-unselected)
                world-balls-3-at-left-wall-after-tick-bounced
                "ball beyond the left wall should be bounced")
  
  (check-equal? (world-after-tick world-balls-3-beyond-right-wall-unselected)
                world-balls-3-at-right-wall-after-tick-bounced
                "ball beyond the right wall should be bounced")
  
  (check-equal? (world-after-tick world-balls-1-at-center-unselected-paused)
                world-balls-1-at-center-unselected-paused
                "puased world should remain the same if paused"))

