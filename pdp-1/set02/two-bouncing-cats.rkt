;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname two-bouncing-cats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; two draggable cats.
;; like draggable cat, but there are TWO cats.  They are individually
;; draggable.  But space pauses or unpauses the entire system.

;; draggable cat.
;; like falling cat, but user can drag the cat with the mouse.
;; button-down to select, drag to move, button-up to release.

;; falling cat.  
;; A cat falls from the top of the scene.
;; The user can pause/unpause the cat with the space bar.

;; start with (main 0)

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)


(provide
  initial-world
  world-after-tick
  world-after-mouse-event
  world-after-key-event
  world-cat1
  world-cat2
  world-paused?
  cat-x-pos
  cat-y-pos
  cat-selected?
  cat-west?
  cat-east?
  cat-north?
  cat-south?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION.

;; main : Number -> World
;; GIVEN: the initial y-position of the cats
;; EFFECT: runs the simulation, starting with the cats facing south
;; RETURNS: the final state of the world
(define (main initial-pos)
  (big-bang (initial-world initial-pos)
            (on-tick world-after-tick 0.5)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define CAT-IMAGE (bitmap "cat.png"))

;; how fast the cat falls, in pixels/tick
(define CATSPEED 8)

;; dimensions of the canvas
(define CANVAS-WIDTH 450)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CAT1-X-COORD (/ CANVAS-WIDTH 3))
(define CAT2-X-COORD (* 2 CAT1-X-COORD))

;; dimensions of the cat
(define HALF-CAT-WIDTH  (image-width  CAT-IMAGE))
(define HALF-CAT-HEIGHT (image-height CAT-IMAGE))

(define NORTH 1)
(define EAST -2)
(define SOUTH -1)
(define WEST 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

; a Direction is one of:
; --NORTH
; --SOUTH
; --EAST
; --WEST
;
; Template:
; Direction-fn: Direction -> ??
; (define (direction-fn d)
;   (cond
;     [(= d NORTH) ...]
;     [(= d SOUTH) ...]
;     [(= d WEST) ...]
;     [(= d EAST) ...]))


(define-struct world (cat1 cat2 paused?))
;; A World is a (make-world Cat Cat Boolean)
;; cat1 and cat2 are the two cats
;; paused? describes whether or not the world is paused

;; template:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (world-cat1 w) (world-cat2 w) (world-paused? w)))


(define-struct cat (x-pos y-pos selected? direction))
;; A Cat is a (make-cat Number Number Boolean Direction)
;; Interpretation: 
;; x-pos, y-pos give the position of the cat. 
;; selected? describes whether or not the cat is selected.
;; Direction describes which direction the cat is facing

;; template:
;; cat-fn : Cat -> ??
;(define (cat-fn c)
; (... (cat-x-pos c) (cat-y-pos c) (cat-selected? c)))

;; examples of cats, for testing
(define selected-cat1-at-120-south (make-cat CAT1-X-COORD 120 true SOUTH))
(define unselected-cat1-at-120-south (make-cat CAT1-X-COORD 120 false SOUTH))

(define selected-cat1-at-120-north (make-cat CAT1-X-COORD 136 true NORTH))
(define unselected-cat1-at-120-north (make-cat CAT1-X-COORD 136 false NORTH))

(define selected-cat1-at-120-east (make-cat CAT1-X-COORD 120 true EAST))
(define unselected-cat1-at-120-east (make-cat CAT1-X-COORD 120 false EAST))

(define selected-cat1-at-120-west (make-cat CAT1-X-COORD 120 true WEST))
(define unselected-cat1-at-120-west (make-cat CAT1-X-COORD 120 false WEST))

(define selected-cat1-at-beyond_edge-south (make-cat CAT1-X-COORD CANVAS-HEIGHT true SOUTH))
(define unselected-cat1-at-beyond_edge-south (make-cat CAT1-X-COORD CANVAS-HEIGHT false SOUTH))

(define selected-cat1-at-beyond_edge-north (make-cat CAT1-X-COORD 0 true NORTH))
(define unselected-cat1-at-beyond_edge-north (make-cat CAT1-X-COORD 0 false NORTH))

(define selected-cat1-at-beyond_edge-east (make-cat CANVAS-WIDTH 120 true EAST))
(define unselected-cat1-at-beyond_edge-east (make-cat CANVAS-WIDTH 120 false EAST))

(define selected-cat1-at-beyond_edge-west (make-cat 0 120 true WEST))
(define unselected-cat1-at-beyond_edge-west (make-cat 0 120 false WEST))

(define selected-cat1-at-edge-south (make-cat CAT1-X-COORD (- CANVAS-HEIGHT HALF-CAT-HEIGHT) true NORTH))
(define unselected-cat1-at-edge-south (make-cat CAT1-X-COORD (- CANVAS-HEIGHT HALF-CAT-HEIGHT) false NORTH))

(define selected-cat1-at-edge-north (make-cat CAT1-X-COORD HALF-CAT-HEIGHT true SOUTH))
(define unselected-cat1-at-edge-north (make-cat CAT1-X-COORD HALF-CAT-HEIGHT false SOUTH))

(define selected-cat1-at-edge-east (make-cat (- CANVAS-WIDTH HALF-CAT-WIDTH) 120 true WEST))
(define unselected-cat1-at-edge-east (make-cat (- CANVAS-WIDTH HALF-CAT-WIDTH) 120 false WEST))

(define selected-cat1-at-edge-west (make-cat HALF-CAT-WIDTH 120 true EAST))
(define unselected-cat1-at-edge-west (make-cat HALF-CAT-WIDTH 120 false EAST))

(define selected-cat1-at-128-south (make-cat CAT1-X-COORD 128 true SOUTH))
(define unselected-cat1-at-128-south (make-cat CAT1-X-COORD 128 false SOUTH))

(define selected-cat1-at-128-north (make-cat CAT1-X-COORD 136 true NORTH))
(define unselected-cat1-at-128-north (make-cat CAT1-X-COORD 128 false NORTH))

(define selected-cat1-at-128-east (make-cat CAT1-X-COORD 120 true EAST))
(define unselected-cat1-at-128-east (make-cat (+ 8 CAT1-X-COORD) 120 false EAST))

(define selected-cat1-at-128-west (make-cat CAT1-X-COORD 120 true WEST))
(define unselected-cat1-at-128-west (make-cat (- CAT1-X-COORD 8) 120 false WEST))

(define selected-cat2-at-135-north (make-cat CAT2-X-COORD 135 true NORTH))
(define unselected-cat2-at-135-north (make-cat CAT2-X-COORD 135 false NORTH))

(define selected-cat2-at-135-south (make-cat CAT2-X-COORD 135 true SOUTH))
(define unselected-cat2-at-135-south (make-cat CAT2-X-COORD 135 false SOUTH))

(define selected-cat2-at-135-west (make-cat CAT2-X-COORD 135 true WEST))
(define unselected-cat2-at-135-west (make-cat (- CAT2-X-COORD 8) 135 false WEST))

(define selected-cat2-at-135-east (make-cat CAT2-X-COORD 135 true EAST))
(define unselected-cat2-at-135-east (make-cat (+ 8 CAT2-X-COORD) 135 false EAST))


(define selected-cat1-at-235-north (make-cat CAT2-X-COORD 235 true NORTH))
(define unselected-cat1-at-235-north (make-cat CAT2-X-COORD 235 false NORTH))

(define selected-cat1-at-235-south (make-cat CAT2-X-COORD 235 true SOUTH))
(define unselected-cat1-at-235-south (make-cat CAT2-X-COORD 235 false SOUTH))

(define selected-cat1-at-235-west (make-cat CAT2-X-COORD 235 true WEST))
(define unselected-cat1-at-235-west (make-cat (- CAT2-X-COORD 8) 235 false WEST))

(define selected-cat1-at-235-east (make-cat CAT2-X-COORD 235 true EAST))
(define unselected-cat1-at-235-east (make-cat (+ 8 CAT2-X-COORD) 235 false EAST))


;; examples of worlds, for testing

(define paused-world-at-120
  (make-world
    unselected-cat1-at-120-south
    selected-cat2-at-135-south
    true))

(define unpaused-world-at-120
  (make-world
    unselected-cat1-at-120-south
    selected-cat2-at-135-south
    false))

;; in an unpaused world, the unselected cat falls, but the selected
;; cat stays pinned to the mouse.
(define unpaused-world-at-120-after-tick
  (make-world
    unselected-cat1-at-128-south
    selected-cat2-at-135-south
    false))
  
;; examples KeyEvents for testing
(define pause-key-event " ")
(define non-pause-key-event "q")   

;; example MouseEvents for testing:
(define button-down-event "button-down")
(define drag-event "drag")
(define button-up-event "button-up")
(define other-event "enter")

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: a World
;; RETURNS: the world that should follow w after a tick.
;; STRATEGY: structural decomposition on w : World
(define (world-after-tick w)
  (if (world-paused? w)
    w
    (make-world
      (cat-after-tick (world-cat1 w))
      (cat-after-tick (world-cat2 w))
      (world-paused? w))))

(begin-for-test

  (check-equal?
    (world-after-tick paused-world-at-120)
    paused-world-at-120)

  (check-equal?
   (world-after-tick unpaused-world-at-120)
   unpaused-world-at-120-after-tick))

;; cat-after-tick : Cat -> Cat
;; GIVEN: a cat c
;; RETURNS: the state of the given cat after a tick
;; unpaused world.

;; examples: 
;; cat selected
;; (cat-after-tick selected-cat1-at-120) = selected-cat1-at-120
;; cat paused:
;; (cat-after-tick unselected-cat1-at-120-south) = unselected-cat-at-128

;; STRATEGY: structural decomposition on c : Cat

(define (cat-after-tick c)
  (if (cat-selected? c)
      c
      (bounce-back-cat (move-cat c))))


; get-cat-x-pos: NonNegInt -> PosInt
; GIVEN: the cat's current x-pos(y-pos)
; RETURNS: the cat's adjusted x-pos(y-pos)
; EXAMPLE: see tests below
; STRATEGY: function composition
(define (get-cat-x-pos x-pos)
  (cond 
    [(< x-pos HALF-CAT-WIDTH) HALF-CAT-WIDTH]
    [(> x-pos (- CANVAS-WIDTH HALF-CAT-WIDTH)) (- CANVAS-WIDTH HALF-CAT-WIDTH)] 
    [else x-pos]))

(define (get-cat-y-pos y-pos)
  (cond
    [(< y-pos HALF-CAT-HEIGHT) HALF-CAT-HEIGHT]
    [(> y-pos (- CANVAS-HEIGHT HALF-CAT-HEIGHT)) (- CANVAS-HEIGHT HALF-CAT-HEIGHT)]
    [else y-pos]))

; put-back-cat: NonNegInt NonNegInt Direction -> Cat
; GIVEN: the cat's current x-pos and y-pos, and direction
; RETURNS: a cat with its position adjusted
; EXAMPLE: see tests below
; STRATEGY: function composition
(define (put-back-cat x-pos y-pos direction)
  (make-cat (get-cat-x-pos x-pos) 
            (get-cat-y-pos y-pos) 
            false 
            direction))

; cat-x-not-in-canvas: NonNegInt -> Boolean
; GIVEN: cat's x-pos(y-pos)
; RETURNS: #t iff the cat's x-pos(y-pos) is not in the canvas
; EXAMPLE: see tests below
; STRATEGY: function composition
(define (cat-x-not-in-canvas? x-pos)
  (not (<= HALF-CAT-WIDTH 
          x-pos
          (- CANVAS-WIDTH HALF-CAT-WIDTH))))

(define (cat-y-not-in-canvas? y-pos)
  (not (<= HALF-CAT-HEIGHT 
          y-pos
          (- CANVAS-HEIGHT HALF-CAT-HEIGHT))))

; cat-not-in-canvas?: NonNegInt NonNegInt -> Boolean
; GIVEN: cat's x-pos and y-pos
; RETURNS: #t iff the cat's position is not in the canvas
; EXAMPLE: see tests below
; STRATEGY: function composition
(define (cat-not-in-canvas? x-pos y-pos)
  (or (cat-x-not-in-canvas? x-pos)
      (cat-y-not-in-canvas? y-pos)))

; get-cat-speed: Direction -> Int
; GIVEN: a Direction
; RETURNS: a int number of the cat's current speed(based on the current direction)
; EXAMPLE: see tests below
; STRATEGY: function composition
(define (get-cat-speed direction)
  (* (/ direction (abs direction)) CATSPEED -1))


; move-cat: Cat -> Cat
; GIVEN: a cat to move
; RETURNS: a cat move to the right place
; EXAMPLE: see tests below
; STRATEGY: structural composition on Cat
(define (move-cat c)
  (if (or (= (cat-direction c) NORTH) (= (cat-direction c) SOUTH))
      (make-cat (cat-x-pos c)
                (+ (get-cat-speed (cat-direction c)) (cat-y-pos c))
                false
                (cat-direction c))
      (make-cat (+ (get-cat-speed (cat-direction c)) (cat-x-pos c))
                (cat-y-pos c)
                false
                (cat-direction c))))


; bounce-back-cat: Cat -> Cat
; GIVEN: a Cat
; RETURNS: a Cat that is bounced back iff it's out of canvas
; EXAMPLE: see tests below
; STRATEGY: structural decomposition on Cat
(define (bounce-back-cat c)
  (if (cat-not-in-canvas? (cat-x-pos c) (cat-y-pos c))
      (make-cat (get-cat-x-pos (cat-x-pos c)) 
                (get-cat-y-pos (cat-y-pos c)) 
                false 
                (- 0 (cat-direction c)))
      c))


;; tests: tests follow help function.
(begin-for-test
  ;; cat selected
  (check-equal?
    (cat-after-tick selected-cat1-at-120-south)
    selected-cat1-at-120-south
    "selected cat shouldn't move")

  ; cat unselected
  (check-equal? 
    (cat-after-tick unselected-cat1-at-120-north)
    unselected-cat1-at-128-north
   "unselected cat should fall CATSPEED pixels and remain unselected")

  (check-equal? 
    (cat-after-tick unselected-cat1-at-120-south)
    unselected-cat1-at-128-south
    "unselected cat should fall CATSPEED pixels and remain unselected")

  (check-equal? 
    (cat-after-tick unselected-cat1-at-120-west)
    unselected-cat1-at-128-west
    "unselected cat should fall CATSPEED pixels and remain unselected")

  (check-equal? 
    (cat-after-tick unselected-cat1-at-120-east)
    unselected-cat1-at-128-east
    "unselected cat should fall CATSPEED pixels and remain unselected")

  (check-equal?
    (cat-after-tick unselected-cat1-at-beyond_edge-west)
    unselected-cat1-at-edge-west
    "cat moving out of the canvas should be bounced back")

  (check-equal?
    (cat-after-tick unselected-cat1-at-beyond_edge-north)
    unselected-cat1-at-edge-north
    "cat moving out of the canvas should be bounced back")

  (check-equal?
    (cat-after-tick unselected-cat1-at-beyond_edge-east)
    unselected-cat1-at-edge-east
    "cat moving out of the canvas should be bounced back")

  (check-equal?
    (cat-after-tick unselected-cat1-at-beyond_edge-south)
    unselected-cat1-at-edge-south
    "cat moving out of the canvas should be bounced back")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
; GIVEN: a World
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene paused-world-at-120) should return a canvas with
;; two cats, one at (150,120) and one at (300,128)         
;; STRATEGY: structural decomposition w : World
(define (world-to-scene w)
  (place-cat
    (world-cat1 w)
    (place-cat
      (world-cat2 w)
      EMPTY-CANVAS)))

(define image-of-paused-world-at-120
  (place-image CAT-IMAGE 150 120
    (place-image CAT-IMAGE 300 135
      EMPTY-CANVAS)))

(begin-for-test
  (check-equal?
    (world-to-scene paused-world-at-120)
    image-of-paused-world-at-120)
    "paused world should remain unchanged")

;; place-cat : Cat Scene -> Scene
;; RETURNS: a scene like the given one, but with the given cat painted
;; on it.
; EXAMPLE: see tests below
; STRATEGY: structural decomposition on Cat
(define (place-cat c s)
  (place-image
    CAT-IMAGE
    (cat-x-pos c) (cat-y-pos c)
    s))


;;; check this visually to make sure it's what you want
(define image-at-120 (place-image CAT-IMAGE CAT1-X-COORD 120 EMPTY-CANVAS))


;; tests
;;; note: these only test whether world-to-scene calls place-image properly.
;;; it doesn't check to see whether image-at-120 is the right image!
(begin-for-test
 (check-equal? 
   (place-cat selected-cat1-at-120-south EMPTY-CANVAS)
   image-at-120
   "(place-cat selected-cat1-at-120 EMPTY-CANVAS) returned unexpected image or value")

 (check-equal?
   (place-cat unselected-cat1-at-120-south EMPTY-CANVAS)   
   image-at-120
   "(place-cat unselected-ca1t-at-120 EMPTY-CANVAS) returned unexpected image or value"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; on space, toggle paused?-- ignore all others
;; EXAMPLES: see tests below
;; STRATEGY: cases on kev : KeyEvent
(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ")
     (world-with-paused-toggled w)]
    [(key=? kev "left") (world-after-directionkey w WEST)]
    [(key=? kev "right") (world-after-directionkey w EAST)]
    [(key=? kev "up") (world-after-directionkey w NORTH)]
    [(key=? kev "down") (world-after-directionkey w SOUTH)]
    [else w]))

; world-after-directionkey: World Direction -> World
; GIVEN: a World and a Direction
; RETURNS: a World after the key event
; EXAMPLES: see tests below
; STRATEGY: structural decomposition on World
(define (world-after-directionkey w direction)
  (make-world
    (change-cat-direaction (world-cat1 w) direction)
    (change-cat-direaction (world-cat2 w) direction)
    (world-paused? w)))


; change-cat-direaction: Cat Direction
; GIVEN: a Cat and a Direction
; RETURNS: a cat who is facing the given direction if selected
; EXAMPLE: see tests below
; STRATEGY: structural decomposition on Cat
(define (change-cat-direaction c d)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c)
                (cat-y-pos c)
                (cat-selected? c)
                d)
      c))


;; world-with-paused-toggled : World -> World
;; GIVEN: a World
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: structural decomposition on: World
(define (world-with-paused-toggled w)
  (make-world
   (world-cat1 w)
   (world-cat2 w)
   (not (world-paused? w))))


;; for world-after-key-event, we need 4 tests: a paused world, and an
;; unpaused world, and a pause-key-event and a non-pause key event.

(begin-for-test
  (check-equal?
    (world-after-key-event paused-world-at-120 pause-key-event)
    unpaused-world-at-120
    "after pause key, a paused world should become unpaused")

  (check-equal?
    (world-after-key-event unpaused-world-at-120 pause-key-event)
    paused-world-at-120
    "after pause key, an unpaused world should become paused")

  (check-equal?
    (world-after-key-event paused-world-at-120 non-pause-key-event)
    paused-world-at-120
    "after a non-pause key, a paused world should be unchanged")

  (check-equal?
    (world-after-key-event unpaused-world-at-120 non-pause-key-event)
    unpaused-world-at-120
    "after a non-pause key, an unpaused world should be unchanged")

  (check-equal?
    (world-after-key-event
      (make-world
        selected-cat1-at-120-south
        unselected-cat2-at-135-east
        false)
      "right"
      )
      (make-world
        selected-cat1-at-120-east
        unselected-cat2-at-135-east
        false)
        "direction key should change the cat's direction")

  (check-equal?
    (world-after-key-event
      (make-world
        selected-cat1-at-235-south
        unselected-cat2-at-135-east
        false)
      "left"
      )
      (make-world
        selected-cat1-at-235-west
        unselected-cat2-at-135-east
        false)
        "direction key should change the cat's direction")

  (check-equal?
    (world-after-key-event
      (make-world
        selected-cat1-at-235-south
        unselected-cat2-at-135-east
        false)
      "up"
      )
      (make-world
        selected-cat1-at-235-north
        unselected-cat2-at-135-east
        false)
        "direction key should change the cat's direction")

  (check-equal?
    (world-after-key-event
      (make-world
        selected-cat1-at-235-north
        unselected-cat2-at-135-east
        false)
      "down"
      )
      (make-world
        selected-cat1-at-235-south
        unselected-cat2-at-135-east
        false)
        "direction key should change the cat's direction")

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Number Number MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
;; examples:  See slide on life cycle of dragged cat
;; strategy: struct decomp on World
(define (world-after-mouse-event w mx my mev)
  (make-world
    (cat-after-mouse-event (world-cat1 w) mx my mev)
    (cat-after-mouse-event (world-cat2 w) mx my mev)
    (world-paused? w)))



;; cat-after-mouse-event : Cat Number Number MouseEvent -> Cat
;; GIVEN: a cat and a description of a mouse event
;; RETURNS: the cat that should follow the given mouse event
;; examples:  See slide on life cycle of dragged cat
;; strategy: cases on mouse events
(define (cat-after-mouse-event c mx my mev)
  (cond
    [(mouse=? mev "leave") (cat-after-leave c)]
    [(mouse=? mev "button-down") (cat-after-button-down c mx my)]
    [(mouse=? mev "drag") (cat-after-drag c mx my)]
    [(mouse=? mev "button-up") (cat-after-button-up c)]
    [else c]))

; cat-north: Cat -> Boolean
; GIVEN: a cat
; RETURNS: Boolean is the cat is heading NORTH
; EXAMPLE: see tests below
; STRATEGY: structural decomposition on Cat
(define (cat-north? c)
  (= NORTH (cat-direction c)))

(define (cat-south? c)
  (= SOUTH (cat-direction c)))

(define (cat-east? c)
  (= EAST (cat-direction c)))

(define (cat-west? c)
  (= WEST (cat-direction c)))

;tests for direction
(begin-for-test 
  (check-equal? (cat-south? unselected-cat1-at-128-south) #t)
  (check-equal? (cat-south? unselected-cat1-at-128-north) #f)
  (check-equal? (cat-north? unselected-cat1-at-128-north) #t)
  (check-equal? (cat-north? unselected-cat1-at-128-south) #f)
  (check-equal? (cat-east? unselected-cat1-at-128-east) #t)
  (check-equal? (cat-east? unselected-cat1-at-128-south) #f)
  (check-equal? (cat-west? unselected-cat1-at-128-west) #t)
  (check-equal? (cat-west? unselected-cat1-at-128-south) #f)
  )


;tests
(begin-for-test

  ;; button-down:

  ;; button-down inside cat1
  (check-equal?
    (world-after-mouse-event 
      (make-world
        unselected-cat1-at-120-south
        unselected-cat2-at-135-south
        false)
      (+ CAT1-X-COORD 5) 115    ;; a coordinate inside cat1
      "button-down")
    (make-world
      selected-cat1-at-120-south
      unselected-cat2-at-135-south
      false)
    "button down inside cat1 should select it but didn't")


  ;; button-down inside cat2
  (check-equal?
    (world-after-mouse-event 
      (make-world
        unselected-cat1-at-120-south
        unselected-cat2-at-135-south
        false)
      (+ CAT2-X-COORD 5) 115    ;; a coordinate inside cat2
      "button-down")
    (make-world
      unselected-cat1-at-120-south
      selected-cat2-at-135-south
      false)
    "button down inside cat2 should select it but didn't")

  ;; button-down not inside any cat
  (check-equal?
    (world-after-mouse-event 
      (make-world
        unselected-cat1-at-120-south
        unselected-cat2-at-135-south
        false)
      (+ CAT1-X-COORD 5) 215    ;; a coordinate not inside cat1 or cat2
      "button-down")
    (make-world
      selected-cat1-at-120-south
      unselected-cat2-at-135-south
      false)
    "button down outside any cat should leave world unchanged, but didn't")

  ;; Question: is it possible to do a button-down on an
  ;; already-selected cat?  Is it possible to do a button-down on a
  ;; world in which ANY cat is selected?

  ;; tests for drag

  ;; don't care about paused, care only about which cat is selected. 

  ;; no cats selected: drag should not change anything
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat1-at-120-south
        unselected-cat2-at-135-south
        false)
      (+ CAT1-X-COORD 100) 15    ;; a large motion
      "drag")
    (make-world
        unselected-cat1-at-120-south
        unselected-cat2-at-135-south
        false)
    "drag with no cat selected didn't leave world unchanged")
    
  ;; cat1 selected
  (check-equal?
    (world-after-mouse-event
      (make-world
        selected-cat1-at-120-south
        unselected-cat2-at-135-south
        false)
      (+ CAT1-X-COORD 100) 15    ;; a large motion
      "drag")
    (make-world
      (make-cat (+ CAT1-X-COORD 100) 15 true SOUTH)
      unselected-cat2-at-135-south
      false)
    "drag when cat1 is selected should just move cat1, but didn't")

  ;; cat2 selected
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat2-at-135-south
        selected-cat1-at-120-south
        false)
      (+ CAT1-X-COORD 100) 15    ;; a large motion
      "drag")
    (make-world
      unselected-cat2-at-135-south
      (make-cat (+ CAT1-X-COORD 100) 15 true SOUTH)
      false)
    "drag when cat2 is selected should just move cat2, but didn't")

  ;; Question: is it possible to have both cat1 and cat2 selected?  If
  ;; so, what happens when they are both selected?

  ;; tests for button-up

  ;; button-up always unselects both cats

  ;; unselect cat1
  (check-equal?
    (world-after-mouse-event
      (make-world
        selected-cat2-at-135-south
        unselected-cat1-at-120-south
        true)
      (+ CAT1-X-COORD 100) 15    ;; arbitrary location
      "button-up")
    (make-world
        unselected-cat2-at-135-south
        unselected-cat1-at-120-south
        true)
    "button-up failed to unselect cat1")



  ;; unselect cat2
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat1-at-120-south
        selected-cat2-at-135-south
        true)
      (+ CAT1-X-COORD 100) 120   ;; arbitrary location
      "button-up")
    (make-world
        unselected-cat1-at-120-south
        unselected-cat2-at-135-south
        true)
    "button-up failed to unselect cat2")
  
  ;; unselect cat2
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat1-at-120-south
        unselected-cat2-at-135-south
        true)
      (+ CAT1-X-COORD 100) 15    ;; arbitrary location
      "button-up")
    (make-world
        unselected-cat1-at-120-south
        unselected-cat2-at-135-south
        true)
    "button-up with two unselected cats failed.")

  ;; tests for other mouse events

  (check-equal?
    (world-after-mouse-event unpaused-world-at-120 
      (+ CAT1-X-COORD 100) 15    ;; arbitrary coordinate
      "move")
    unpaused-world-at-120
    "other mouse events should leave the world unchanged, but didn't")

  (check-equal?
    (world-after-mouse-event
      (make-world 
        unselected-cat1-at-128-south
        (make-cat CANVAS-WIDTH 135 true SOUTH)
        false)
        CANVAS-WIDTH 120 
        "leave")
      (make-world 
        unselected-cat1-at-128-south
        (make-cat (- CANVAS-WIDTH HALF-CAT-WIDTH) 135 false SOUTH)
        false)
      "mouse leaving the canvas while selecting the cat should bounce back the cat")

  (check-equal?
    (world-after-mouse-event
      (make-world 
        unselected-cat1-at-128-south
        (make-cat 0 135 true SOUTH)
        false)
        CANVAS-WIDTH 120 
        "leave")
      (make-world 
        unselected-cat1-at-128-south
        (make-cat HALF-CAT-WIDTH 135 false SOUTH)
        false)
        "mouse leaving the canvas while selecting the cat should bounce back the cat")

  (check-equal?
    (world-after-mouse-event
      (make-world 
        unselected-cat1-at-128-south
        (make-cat 120 0 true SOUTH)
        false)
        CANVAS-WIDTH 120 
        "leave")
      (make-world 
        unselected-cat1-at-128-south
        (make-cat 120 HALF-CAT-HEIGHT false SOUTH)
        false)
        "mouse leaving the canvas while selecting the cat should bounce back the cat")

  (check-equal?
    (world-after-mouse-event
      (make-world 
        unselected-cat1-at-128-south
        (make-cat 120 CANVAS-HEIGHT true SOUTH)
        false)
        CANVAS-WIDTH 120 
        "leave")
      (make-world 
        unselected-cat1-at-128-south
        (make-cat 120 (- CANVAS-HEIGHT HALF-CAT-HEIGHT) false SOUTH)
        false)
        "mouse leaving the canvas while selecting the cat should bounce back the cat")

  (check-equal?
    (world-after-mouse-event
      (make-world 
        unselected-cat1-at-128-south
        (make-cat 0 CANVAS-HEIGHT true SOUTH)
        false)
        CANVAS-WIDTH 120 
        "leave")
      (make-world 
        unselected-cat1-at-128-south
        (make-cat HALF-CAT-WIDTH (- CANVAS-HEIGHT HALF-CAT-HEIGHT) false SOUTH)
        false)
        "mouse leaving the canvas while selecting the cat should bounce back the cat")

  (check-equal?
    (world-after-mouse-event
      (make-world 
        unselected-cat1-at-128-south
        (make-cat 200 200 true SOUTH)
        false)
        CANVAS-WIDTH 120 
        "leave")
      (make-world 
        unselected-cat1-at-128-south
        (make-cat 200 200 true SOUTH)
        false)
        "mouse leaving the canvas while selecting the cat should bounce back the cat")

  )

;; helper functions:

;; cat-after-button-down : Cat NonNegInt NonNegInt -> Cat
;; RETURNS: the cat following a button-down at the given location.
;; STRATEGY: struct decomp on cat
(define (cat-after-button-down c x y)
  (if (in-cat? c x y)
      (make-cat (cat-x-pos c) (cat-y-pos c) true (cat-direction c))
      c))

;; cat-after-drag : Cat NonNegInt NonNegInt -> Cat
;; RETURNS: the cat following a drag at the given location
;; STRATEGY: struct decomp on cat
(define (cat-after-drag c x y)
  (if (cat-selected? c)
      (make-cat x y true (cat-direction c))
      c))

;; cat-after-button-up : Cat NonNegInt NonNegInt -> Cat
;; RETURNS: the cat following a button-up at the given location
;; STRATEGY: struct decomp on cat
(define (cat-after-button-up c)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c) (cat-y-pos c) false (cat-direction c))
      c))

; cat-after-leave: Cat -> Cat
; GIVEN: a Cat
; RETURNS: the cat with its positon adjusted after dragged out of canvas
; STRATEGY: structural decomposition [cat]
(define (cat-after-leave c)
  (if (cat-selected? c)
      (if (or (cat-x-not-in-canvas? (cat-x-pos c)) (cat-y-not-in-canvas? (cat-y-pos c)))
          (put-back-cat (cat-x-pos c) (cat-y-pos c) (cat-direction c))
          c)
      c))


; in-cat? : Cat NonNegInt NonNegInt -> Cat
; GIVEN: a Cat
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given cat.
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on Cat
(define (in-cat? c x y)
  (and
    (<= 
      (- (cat-x-pos c) HALF-CAT-WIDTH)
      x
      (+ (cat-x-pos c) HALF-CAT-WIDTH))
    (<= 
      (- (cat-y-pos c) HALF-CAT-HEIGHT)
      y
      (+ (cat-y-pos c) HALF-CAT-HEIGHT))))

(begin-for-test
  
  ;; inside the cat
  (check-equal?
    (in-cat? unselected-cat1-at-120-south (+ CAT1-X-COORD 5) 120)
    true
    "test of in-cat? with nearby point")

  (check-equal?
    (in-cat? unselected-cat1-at-120-south 
      (+ CAT1-X-COORD 100) 15)    ;; a coordinate not inside the cat
    false
    "test of in-cat? with distant point")
  )


;; discussion question: are these tests sufficient to test in-cat?

;; initial-world : Number -> World
;; RETURNS: a world with two unselected cats at the given y coordinate
(define (initial-world y)
  (make-world
    (make-cat CAT1-X-COORD y false SOUTH)
    (make-cat CAT2-X-COORD y false SOUTH)
    false))

(begin-for-test
  (initial-world 200)
  (make-world
   (make-cat CAT1-X-COORD 100 false SOUTH)
   (make-cat CAT2-X-COORD 100 false SOUTH)
   false))



