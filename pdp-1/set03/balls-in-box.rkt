;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname balls-in-box) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

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
 ball-selected?)


(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define RADIUS 20)
(define OUTLINE-BALL (circle RADIUS "outline" "red"))
(define SOLID-BALL (circle RADIUS "solid" "red"))
(define CENTER-WIDTH (/ CANVAS-WIDTH 2))
(define CENTER-HEIGHT (/ CANVAS-HEIGHT 2))

(define-struct ball (x-pos y-pos x_vector y_vector selected?))
; a Ball is a (make-ball NonNegInteger NonNegInteger Integer Integer Boolean)
; Interpretation:
;  --x is the x-coordinate of the ball
;  --y is the y-coordinate of the ball
;  --x_vector is the x-value of the vetor(from the ball's center to mouse position)
;  --y_vector is the y-value of the vetor(from the ball's center to mouse position)
;  Template:
;  (define (ball-fn b)
;   (...
;     (ball-x-pos b)
;     (ball-y-pos b)
;     (ball-x_vector b)
;     (ball-y_vector b)
;     (ball-selected? b))
;examples for tests:
(define ball-at-center-unselected (make-ball CENTER-WIDTH CENTER-HEIGHT 0 0 false))
(define ball-at-center-selected (make-ball CENTER-WIDTH CENTER-HEIGHT 0 0 #t))

(define ball-at-50-50-unselected (make-ball 50 50 0 0 false))
(define ball-at-150-150-unselected (make-ball 150 150 0 0 false))
(define ball-at-50-50-selected (make-ball 50 50 0 0 #t))
(define ball-at-150-150-selected (make-ball 150 150 0 0 #t))
(define ball-at-250-250-selected (make-ball 150 150 0 0 #t))
(define ball-at-right-wall (make-ball (- CANVAS-WIDTH RADIUS) 150 0 0 #f))
(define ball-beyong-right-wall (make-ball CANVAS-WIDTH 150 0 0 #t))

(define ball-at-left-wall (make-ball RADIUS 150 0 0 #f))
(define ball-beyong-left-wall (make-ball 0 150 0 0 #t))

(define ball-at-top-wall (make-ball 150 RADIUS 0 0 #f))
(define ball-beyong-top-wall (make-ball 150 0 0 0 #t))

(define ball-at-down-wall (make-ball 150 (- CANVAS-HEIGHT RADIUS) 0 0 #f))
(define ball-beyong-down-wall (make-ball 150 CANVAS-HEIGHT 0 0 #t))

; unselected-world
(define scene1 (place-image (text (number->string 2) 30 "blue")
                            (* 3 RADIUS)
                            (* 3 RADIUS)
                            (place-image OUTLINE-BALL
                                         (ball-x-pos ball-at-50-50-unselected)
                                         (ball-y-pos ball-at-50-50-unselected)
                                         (place-image OUTLINE-BALL
                                                      (ball-x-pos ball-at-150-150-unselected)
                                                      (ball-y-pos ball-at-150-150-unselected)
                                                      EMPTY-CANVAS))))
; world with one ball selected
(define scene2 (place-image (text (number->string 2) 30 "blue")
                            (* 3 RADIUS)
                            (* 3 RADIUS)
                            (place-image OUTLINE-BALL
                                         (ball-x-pos ball-at-50-50-unselected)
                                         (ball-y-pos ball-at-50-50-unselected)
                                         (place-image SOLID-BALL
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

(define-struct world (balls num_balls))
; a World is a (make-world LOB NonNegInteger)
; Interpretation: 
;  --LOB is a list of Balls
;  --num_balls is the number of the balls on the scene
; Template:
; (define (world-fn w)
;   (... (world-balls w) (world-num_balls w)))

; examples for tests:
(define world-balls-2 (make-world (list ball-at-50-50-unselected ball-at-150-150-unselected) 2))
(define world-balls-1 (make-world (list ball-at-center-unselected) 1))
(define world-balls-1-s (make-world (list ball-at-center-selected) 1))
(define world-balls-0 (make-world empty 0))

(define world-balls-2-select-50 (make-world (list ball-at-50-50-selected ball-at-150-150-selected) 2))
(define world-balls-2-select-150 (make-world (list ball-at-50-50-unselected ball-at-150-150-selected) 2))
(define world-balls-2-select-250 (make-world (list ball-at-50-50-unselected ball-at-250-250-selected) 2))
(define world-balls-2-at-right-wall (make-world (list ball-at-50-50-unselected ball-at-right-wall) 2))
(define world-balls-2-beyond-right-wall (make-world (list ball-at-50-50-unselected ball-beyong-right-wall) 2))

(define world-balls-2-at-left-wall (make-world (list ball-at-50-50-unselected ball-at-left-wall) 2))
(define world-balls-2-beyond-left-wall (make-world (list ball-at-50-50-unselected ball-beyong-left-wall) 2))

(define world-balls-2-at-top-wall (make-world (list ball-at-50-50-unselected ball-at-top-wall) 2))
(define world-balls-2-beyond-top-wall (make-world (list ball-at-50-50-unselected ball-beyong-top-wall) 2))

(define world-balls-2-at-down-wall (make-world (list ball-at-50-50-unselected ball-at-down-wall) 2))
(define world-balls-2-beyond-down-wall (make-world (list ball-at-50-50-unselected ball-beyong-down-wall) 2))

(define world-balls-at-50-selected (make-world (list ball-at-50-50-selected) 1))
(define world-balls-at-150-selected (make-world (list ball-at-150-150-selected) 1))

;; main : Number -> World
;; GIVEN: ignored the given arg
;; EFFECT: runs the simulation, starting from empty scene
;; RETURNS: the final state of the world
(define (run initial-pos)
  (big-bang (initial-world initial-pos)
            (on-tick world-after-tick 0.25)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

; initial-world: Any -> World
; GIVEN: anything (ignored)
; RETURNS: an empty world without any balls 
; examples: see tests below
; STRATEGY: function composition
(define (initial-world p)
  (make-world empty 0))


; place-is-solid-ball-on-image: Ball Image Boolean -> Image
; GIVEN: a Ball, a image and a boolean
; RETURNS: an image with the given ball draws on the given image, if is-solid is true, 
;  draw a solid ball, or draw a outline ball
; examples: see tests below
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
; RETURNS: a scene with all given balls on it
; examples: see tests below
; STRATEGY: structural decomposition on List Of Balls
(define (draw-balls balls)
  (cond 
    [(empty? balls) EMPTY-CANVAS]
    [else (if (ball-selected? (first balls)) 
              (place-is-solid-ball-on-image (first balls) (draw-balls (rest balls)) #t)
              (place-is-solid-ball-on-image (first balls) (draw-balls (rest balls)) #f))]))

; world-to-scene: World -> Image
; GIVEN: a World
; RETURNS: an image describes the whole world
; examples: see tests below
; STRATEGY: structural decomposition on World
(define (world-to-scene w)
  (place-image (text (number->string (world-num_balls w)) 30 "blue")
               (* 3 RADIUS)
               (* 3 RADIUS)
               (draw-balls (world-balls w))))

(begin-for-test
  (check-equal? (world-to-scene world-balls-2)
                scene1)
  (check-equal? (world-to-scene world-balls-2-select-150)
                scene2))

; world-after-key-event: World KeyEvent -> World
; GIVEN: a World and a KeyEvent
; RETURNS: a World after the key evnet
; examples: see tests below
; STRATEGY: cases on key event
(define (world-after-key-event w kev)
  (cond 
    [(key=? kev "n") (world-after-n-key w)]
    [else w]))

; world-after-mouse-event: World NonNegInteger NonNegInteger MouseEvent -> World
; GIVEN: a World and the position of the mouse and the mouse evnet
; RETURNS: the World after the mouse event 
; examples: see tests below
; STRATEGY: structural decomposition on World
(define (world-after-mouse-event w mx my mev)
  (make-world (balls-after-mouse-event (world-balls w) mx my mev) (world-num_balls w)))

; balls-after-mouse-event: LOB NonNegInteger NonNegInteger MouseEvent -> World
; GIVEN: a LOB, the position of the mouse(x-coordinate and y-coordinate) and a mouseEvent
; RETURNS: a World after the mouse event
; examples: see tests below
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
; RETURNS: #t if the given position is in the ball
; examples: see tests below
; STRATEGY: structural decomposition on Ball
(define (inball? b mx my)
  (<= (+ (sqr (- mx (ball-x-pos b)))
         (sqr (- my (ball-y-pos b))))
      (sqr RADIUS)))

; select-ball: Ball NonNegInteger NonNegInteger -> Ball
; GIVEN: a Ball and a position(x-corodinate and y-coordinate)
; RETURNS: a Ball with its x_vector and y_vector, its position and its boolean flag for selected updated
; examples: see tests below
; STRATEGY: structural decomposition on Ball
(define (select-ball b mx my)
  (make-ball mx
             my
             (- (ball-x-pos b) mx)
             (- (ball-y-pos b) my)
             true))

; unselect-ball: Ball -> Ball
; GIVEN: a Ball
; RETURNS: a Ball with its x_vector and y_vector, its position(x-corodinate, y-coordinate) 
;   and its boolean flag for if selected updated
; examples: see tests below
; STRATEGY: structural decomposition on Ball
(define (unselect-ball b)
  (make-ball (+ (ball-x_vector b) (ball-x-pos b))
             (+ (ball-y_vector b) (ball-y-pos b))
             0
             0
             false))

; balls-after-button-down: LOB NonNegInteger NonNegInteger -> LOB
; GIVEN: a LOB and a mouse position(x-corodinate and y-coordinate)
; RETURNS: a LOB with the balls updated if the mouse is in any one of the balls
;   only select the first ball in the list if the mouse in several balls' range
; examples: see tests below
; STRATEGY: structural decomposition on List of Balls
(define (balls-after-button-down balls mx my)
  (cond
    [(empty? balls) empty]
    [else (if (inball? (first balls) mx my)
              (cons (select-ball (first balls) mx my) (rest balls))
              (cons (first balls) (balls-after-button-down (rest balls) mx my)))]))

; balls-after-button-up: LOB -> LOB
; GIVEN: a LOB
; RETURNS: a LOB with the selected ball in the list upselected if exists
; examples: see tests below
; STRATEGY: structural decomposition on List of Balls
(define (balls-after-button-up balls)
  (cond
    [(empty? balls) empty]
    [else (cons (unselect-ball (first balls)) (balls-after-button-up (rest balls)))]))

; move-selected-ball: Ball NonNegInteger NonNegInteger -> Ball
; GIVEN: a selected Ball and a mouse position(x-corodinate and y-coordinate)
; RETURNS: a ball same as the given ball except that it's at the given position
; examples: see tests below
; STRATEGY: structural decomposition on Ball
(define (move-selected-ball b mx my)
  (make-ball mx
             my
             (ball-x_vector b)
             (ball-y_vector b)
             (ball-selected? b)))

; balls-after-drag: LOB NonNegInteger NonNegInteger -> LOB
; GIVEN: a LOB and a mouse position(x-corodinate and y-coordinate)
; RETURNS: a LOB with the ball selected move to the given position if exists
; examples: see tests below
; STRATEGY: structural decomposition on List of Balls
(define (balls-after-drag balls mx my)
  (cond
    [(empty? balls) empty]
    [else (if (ball-selected? (first balls))
              (cons (move-selected-ball (first balls) mx my) (rest balls))
              (cons (first balls) (balls-after-drag (rest balls) mx my)))]))

; balls-after-leave: LOB NonNegInteger NonNegInteger -> LOB
; GIVEN: a LOB and a mouse position(x-corodinate and y-coordinate)
; RETURNS: a LOB with the ball selected updated if exists
; examples: see tests below
; STRATEGY: structural decomposition on List of Balls
(define (balls-after-leave balls)
  (cond
    [(empty? balls) empty]
    [else (if (ball-selected? (first balls))
              (cons (put-ball-back (first balls)) (rest balls))
              (cons (first balls) (balls-after-leave (rest balls))))]))

; world-after-n-key: World -> World
; GIVEN: a World
; RETURNS: a new World with its balls added 1
; examples: see tests below
; STRATEGY: structural decomposition on World
(define (world-after-n-key w)
  (make-world (cons (initial-ball 0) (world-balls w))
              (+ 1 (world-num_balls w))))

; initial-ball: Any -> Ball
; GIVEN: ignored
; RETURNS: a unselected-ball at center
; examples: see tests below
; STRATEGY: function compostion
(define (initial-ball p)
  (make-ball CENTER-WIDTH
             CENTER-HEIGHT
             0
             0
             false))

; put-ball-x-pos: NonNegInteger -> NonNegInteger:
; GIVEN: a x-coordinate
; RETURNS: the adjusted x-coordinate if its center is out of canvas
; examples: see tests below
; STRATEGY: function compostion
(define (put-ball-x-pos x)
  (cond
    [(< x RADIUS) RADIUS]
    [(> x (- CANVAS-WIDTH RADIUS)) (- CANVAS-WIDTH RADIUS)]
    [else x]))

; put-ball-y-pos: NonNegInteger -> NonNegInteger:
; GIVEN: a y-coordinate
; RETURNS: the adjusted y-coordinate if its center is out of canvas
; examples: see tests below
; STRATEGY: function compostion
(define (put-ball-y-pos y)
  (cond
    [(< y RADIUS) RADIUS]
    [(> y (- CANVAS-HEIGHT RADIUS)) (- CANVAS-HEIGHT RADIUS)]
    [else y]))


; put-ball-back: Ball -> Ball
; GIVEN: a Ball
; RETURNS: a Ball with its postion adjusted if its center out of canvas
; examples: see tests below
; STRATEGY: structural decomposition on Ball
(define (put-ball-back b)
  (make-ball (put-ball-x-pos (ball-x-pos b))
             (put-ball-y-pos (ball-y-pos b))
             0
             0
             false))

; world-after-tick : World -> World
; GIVEN: a World
; RETURNS: a World after tick
; examples: see tests below
; STRATEGY: function compostion
(define (world-after-tick w)
  w)

(begin-for-test
  (check-equal? (world-after-tick world-balls-1)
                world-balls-1))

(begin-for-test 
  (check-equal?
   (initial-world 9)
   world-balls-0)
  
  (check-equal? (world-after-key-event world-balls-0 "n")
                world-balls-1
                "pressing n should generate a new circle")
  
  (check-equal? (world-after-key-event world-balls-0 "a")
                world-balls-0
                "pressing any key other than 'n' should do nothing")
  
  (check-equal? (world-after-mouse-event world-balls-1 CENTER-WIDTH CENTER-HEIGHT "button-down")
                world-balls-1-s
                "mouse button-down inside the circle should select the ball")
  
  (check-equal? (world-after-mouse-event world-balls-2 250 250 "button-down")
                world-balls-2
                "mouse button-down onside the circle should not select the ball")
  
  (check-equal? (world-after-mouse-event world-balls-1 CENTER-WIDTH CENTER-HEIGHT "enter")
                world-balls-1
                "any other mouse event would do nothing")
  
  (check-equal? (world-after-mouse-event world-balls-1-s CENTER-WIDTH CENTER-HEIGHT "button-up")
                world-balls-1
                "mouse button-up should unselected the ball")
  
  (check-equal? (world-after-mouse-event world-balls-at-50-selected 150 150 "drag")
                world-balls-at-150-selected
                "drag the selected ball should drag the ball where the mouse is")
  
  (check-equal? (world-after-mouse-event world-balls-1 150 150 "drag")
                world-balls-1
                "drag event does nothing to an unselected ball")
  
  (check-equal? (world-after-mouse-event world-balls-2-select-150 150 150 "drag")
                world-balls-2-select-250
                "drag only infects the selected balls")
  
  (check-equal? (world-after-mouse-event world-balls-2-beyond-left-wall 0 150 "leave")
                world-balls-2-at-left-wall
                "leaving the canvas should not drag the ball out of the canvas")
  
  (check-equal? (world-after-mouse-event world-balls-2-beyond-right-wall 0 150 "leave")
                world-balls-2-at-right-wall
                "leaving the canvas should not drag the ball out of the canvas")
  
  (check-equal? (world-after-mouse-event world-balls-2-beyond-top-wall 150 0 "leave")
                world-balls-2-at-top-wall
                "leaving the canvas should not drag the ball out of the canvas")
  
  (check-equal? (world-after-mouse-event world-balls-2-beyond-down-wall 150 CANVAS-HEIGHT "leave")
                world-balls-2-at-down-wall
                "leaving the canvas should not drag the ball out of the canvas")
  
  (check-equal? (world-after-mouse-event world-balls-1 150 CANVAS-HEIGHT "leave")
                world-balls-1
                "leaving the canvas should not drag the ball out of the canvas"))









