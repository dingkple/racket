#lang scheme

;; version 7: We'll make the balls draggable.  We'll add a "selected"
;; field, so the ball will stop moving when it's selected.  
;; And we'll make a selected ball display differently.

;; version 6: OK, now we'll really add the ball factory.  The ball
;; factory, like the box, will be stateful.  It will turn out that
;; only stateful objects can add new objects to the world-- see
;; note below.

;; version 5: "n" adds a new ball.  To do this we'll do two things:
;; 1.  First, we'll write an add-object method in our world.
;; 2.  Then, we'll add a ball factory to our world.  The ball
;; factory will know about the world and the box, and on an "n" it
;; will add a ball by calling the add-object method on the world.

;; But wait: the factory needs to know about the world, so it can
;; send it add-object messages.  So this means the world must have
;; identity: it must be stateful.

;; So first, let's just make the world stateful.

;; =============================================================

;; version 4: In this version, we'll make the
;; box stateful.  To keep track of this we'll add a new interface:
;; StatefulWorldObj<%>. 

;; We use Void in the contracts to document this.
;; Void methods work by EFFECT.  They can return anything.
;; the contract Void means that whoever called the method promises to
;; ignore the value.

;; We'll expand the world to contain both ordinary (functional)
;; objects and stateful ones.

;; Then we'll redo Box% to be a StatefulWorldObj<%>.

;; version 2: add a bouncing ball to the world.  The ball will
;; bounce inside a box.

;; version 1: basic architecture for worlds with multiple objects in them.

;; start this with (run).

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;; Stateful Objects:  they don't return a new WorldObj-- they just
;; change their state
(define StatefulWorldObj<%>
  (interface ()
    on-tick  ; ->  Void
    on-mouse ; Integer Integer MouseEvt -> Void
    on-key   ; KeyEvt -> Void
    add-to-scene  ; Scene -> Scene
    ))

;; functional WorldObj's.
(define WorldObj<%>
  (interface ()
    on-tick       ; -> WorldObj<%>
    on-mouse      ; Integer Integer MouseEvt -> WorldObj<%>
    on-key        ; KeyEvt -> world-obj
    add-to-scene  ; Scene -> Scene
    ))

;; A World% contains a list of WorldObj<%>s and StatefulWorldObj<%>s
;; All it does is to distribute each of the world-obj methods to each
;; of its objects.
;; A World is a (new World% 
;;                     [objects ListOf<WorldObj<%>>]
;;                     [stateful-objects 
;;                       ListOf<StatefulWorldObj<%>>
;;                       default: empty])
(define World%
  (class* object% (StatefulWorldObj<%>)
    (init-field objects)          ;; ListOf<WorldObj<%>>
    (init-field [stateful-objects empty]) ;; ListOf<StatefulWorldObj<%>>
     
    (super-new)
    
    ;; WorldObj<%> -> Void
    ;; EFFECT: Add the given object to the objects field
    (define/public (add-object obj)
      (set! objects
            (cons obj objects)))

    ;; StatefulWorldObj<%> -> Void
    ;; EFFECT: Add the given stateful-object to the stateful-objects
    ;; field. 
    (define/public (add-stateful-object obj)
      (set! stateful-objects (cons obj stateful-objects)))

    ;; -> Void
    ;; EFFECT: pass on-tick to each of the functional objects, and
    ;; collect the result in the objects field; then perform the
    ;; effect on each of the stateful objects.
    (define/public (on-tick)
      (set! objects
            (map 
             (lambda (obj) (send obj on-tick))
             objects))
      (for-each
       (lambda (obj) (send obj on-tick))
       stateful-objects))

    ;; Num Num MouseEvt -> Void
    ;; We pass each mouse event to every object in the world
    ;; and let that object decide whether to respond to it.
    ;; Change is similar to on-tick
    (define/public (on-mouse x y mev)
      (set! objects
            (map
             (lambda (obj) (send obj on-mouse x y mev))
             objects))
      (for-each
       (lambda (obj) (send obj on-mouse x y mev))
       stateful-objects))
       
    
    ;; KeyEvent -> Void
    (define/public (on-key kev)
      (set! objects
            (map
             (lambda (obj) (send obj on-key kev))
             objects))
      (for-each
       (lambda (obj) (send obj on-key kev))
       stateful-objects))

    ;; note the "map" above.  That means that if you send a message to
    ;; a functional object, and the functional object adds an object
    ;; to the world, that new object will be thrown away!  [This
    ;; is hard-won knowledge, which I had forgotten and had to
    ;; painfully reconstruct.  Wed Nov 09 15:53:45 2011]
    
    ;; do you see repeated code here?  
    ;; Is there an opportunity for an abstraction?
    
    ; Scene -> Scene
    ; implementation: first paint the stateful objects, then the
    ; functional ones. 
    (define/public (add-to-scene s)
      (local
       ((define s2 (foldr
                    (lambda (obj s1) (send obj add-to-scene s1))
                    s
                    stateful-objects)))
      (foldr
       (lambda (obj s1) (send obj add-to-scene s1))
       s2
       objects)))

    ;; below this line are getters introduced for testing only

    ;; -> (list ListOf<<WorldObj<%>> ListOf<StatefulWorldObj<%>>)
    (define/public (for-test:get-all-objects)
      (list objects stateful-objects))

    ))   


;; A Box% represents a  resizeable rectangle
;; It will resize itself in response to mouse drags.
;; A Box is a (new Box% [x Integer][y Integer]
;;                      [w Integer][h Integer][selected? Boolean])
(define Box%
  (class* object% (StatefulWorldObj<%>)
    (init-field x)
    (init-field y)
    (init-field w)
    (init-field h)
     ;; x y : Integers: position of center
     ;; w h : Integers: width and height, in pixels
    (init-field selected?)
    
    (super-new)
    
    ;; these will be handy to have
    (define/public (left-edge) (- x (/ w 2)))
    (define/public (right-edge) (+ x (/ w 2)))
    
    ;; the box needs to let the factory look at its x and y
    (define/public (get-x) x)
    (define/public (get-y) y)
    
    ;; notice nobody gets to see its height

    ;; doesn't respond to ticks or key events
    (define/public (on-tick) this)
    (define/public (on-key kev) this)

    ;; on-mouse: Num Num MouseEvent -> Void

    ;; EFFECT:
    ;; mouse-down near right edge of box => mark box selected
    ;; drag & selected => make right edge follow mouse
    ;; mouse-up => mark box unselected

    ;; if the mouse is dragging "near" the right edge of this box,
    ;; resize the box to match the given mouse posn, otherwise return this
    ;; box unchanged.  
    ;; if we move the right edge, we have to adjust the center to match
    (define/public (on-mouse mouse-x mouse-y evt)
      (cond
        [(mouse=? evt "button-down")
         (if (near-right-edge? mouse-x mouse-y)
           (send this select)
           this)]
        [(mouse=? evt "drag")
         (if selected?
           (local
             ((define new-width (- mouse-x (left-edge))))
             (adjust-width new-width))
           this)]
        [(mouse=? evt "button-up")
         (send this unselect)]
        [else this]))

    (define/public (select)
      ;; (new Box%
      ;;   [x x][y y][w w][h h]
      ;;   [selected? true])
      (set! selected? true))

    (define/public (unselect)
      ;; (new Box%
      ;;   [x x][y y][w w][h h]
      ;;   [selected? false])
      (set! selected? false)) 

    ;; adjust-width : Integer -> Void
    ;; GIVEN: A value for the new width of this box
    ;; EFFECT: adjust the center and width so that the width of this
    ;; box is new-width and the left edge is unchanged.
    ;; ALGORITHM: To do this, we have to restore the invariant that 
    ;; right-edge = x + w/2
    ;; left-edge = x - w/2
    ;; left-edge isn't supposed to change, so we adjust x accordingly
    ;; the objects stay in the same place on the screen, so they stay
    ;; the same.
    (define (adjust-width new-width)
      (set! x (+ (send this left-edge) (/ new-width 2)))
      (set! w new-width))

    ;; you can put ordinary functions inside the class if you want
    ;; them to refer to fields
    (define (near-right-edge? other-x other-y)
      (near-vertical-line? 
       (send this right-edge)          ; the x-coordinate of the
                                       ; vertical line
       (- y (/ h 2))                   ; the y-coords of the endpoints
       (+ y (/ h 2))    
       other-x other-y))

    (define NEAR-VERTICAL-LINE-THRESHOLD 5)

    ;; Integer^5 -> Boolean
    ;; returns true iff other-pos is "near" the line from posn1 to posn2
    (define (near-vertical-line? line-x y1 y2 other-x other-y)
      (and
        (<= y1 other-y)
        (<= other-y y2)
        (<= (abs (- line-x other-x)) NEAR-VERTICAL-LINE-THRESHOLD)))

    (define/public (add-to-scene s)
      (place-image 
       (rectangle w h "outline" "black")
       x y s))

    ))


(begin-for-test

  (local
    ((define box1 (new Box% [x 200][y 50][w 100][h 20][selected? true])))
    (check-equal? (send box1 left-edge) 150)
    (check-equal? (send box1 right-edge) 250)
    (send box1 on-mouse 252 50 "drag")  
    (check-equal? (send box1 left-edge) 150)
    (check-equal? (send box1 right-edge) 252)) 
 
   )

;; an object of class Ball% represents a ball at the given
;; coordinates, at the given speed, living in the given box
;; the ball will ask the box for information about its left and
;; right edges.
;; A Ball is a (new Ball% [x Integer][y Integer][box Box][speed Integer])
(define Ball%
  (class* object% (WorldObj<%>)
    (init-field
     x y        ; the position of the center of the ball
     box        ; the Box% in which the ball lives
     speed)     ; ball speed in pixels/tick (either positive or
                ; negative)
    (init-field [selected? false])      ; is target
                                        ; selected?. Initially false.
    (init-field [mx false][my false])              
    ; MaybeInt: if selected, the coordinates of the mouse RELATIVE TO
    ; THE CENTER, otherwise false.
    
    (field [radius 20])
    
    (super-new)
    
    (define/public (add-to-scene s)
      (place-image
       (circle radius 
               (if selected? "solid" "outline")
               "red")
       x y s))
    

    (define/public (on-mouse nx ny mev)
      (cond
        [(mouse=? mev "button-down")
         (send this after-button-down nx ny)]
        [(mouse=? mev "drag")
         (send this after-drag nx ny)]
        [(mouse=? mev "button-up")
         (send this after-button-up)]
        [else this]))

    ;; set selected? to true and set mx, my to mouse position relative
    ;; to x,y
    (define/public (after-button-down nx ny)
      (if (inside-this? nx ny)
        (new Ball%
          [x x][y y][box box][speed speed]
          [selected? true][mx (- nx x)][my (- ny y)])
        this))

    ;; move x,y to match new mouse position
    ;; if the target is selected, then move it to correct relative position,
    (define/public (after-drag nx ny)
      (if selected?
        (new Ball%
          [x (- nx mx)]
          [y (- ny my)]
          [box box][speed speed]
          [mx mx][my my]
          [selected? true])
        this))

   (define/public (after-button-up)
      (new Ball% 
        [x x][y y][box box][speed speed]
        [selected? false][mx false][my false]))

    ;; ball doesn't respond to  key events
    ; (define/public (on-mouse x y evt) this)
    (define/public (on-key kev) this)

    ; -> Ball%
    ; create a ball just like the current one, but where it should be
    ; after a tick
    (define/public (on-tick)
      (cond
        (selected? this)
        ((would-hit-right-edge?) (place-at-right-edge))
        ((would-hit-left-edge?) (place-at-left-edge))
        (else (new Ball% 
                ;; at this point the ball is not selected, so
                ;; selected?, mx, and my are all false (their default values)
                   [x (+ x speed)][y y][box box][speed speed]))))
    
    ; -> Ball%
    ; create a ball at the right edge
    (define (place-at-right-edge)
      (local 
        ((define right-edge (send box right-edge)))
        (new Ball%
               [x (- right-edge radius)]
               [y y]
               [box box]
               [speed (- speed)])))

    ; -> Ball%
    ; create a ball at the left edge
    (define (place-at-left-edge)
      (local
        ((define left-edge (send box left-edge)))
       (new Ball% 
            [x (+ left-edge radius)]
            [y y]
            [box box]
            [speed (- speed)])))

    ;; wishlist functions for ball

    ;; would the right edge of the ball, travelling right, hit the
    ;; right edge?
    (define (would-hit-right-edge?)
      (>= (+ (+ x radius) speed) (send box right-edge)))
    
    ;; would the left edge of the ball, travelling left, hit the left
    ;; edge? 
    (define (would-hit-left-edge?)
      (<= (+ (- x radius) speed) (send box left-edge)))
    
    (define (inside-this? mouse-x mouse-y)
      (<= 
       (+
        (sqr (- x mouse-x))
        (sqr (- y mouse-y)))
       (sqr radius)))
    
 
    ;; getters for testing only
    (define/public (for-test:get-x) x)
    (define/public (for-test:get-y) y)
    (define/public (for-test:get-speed) speed)   
    ))

;; BallFactory% 
;; implements StatefulWorldObj<%>
;; on key event "n", adds a new Ball at the center of the given box in
;; the given world.
;; PRECONDITION: the box must be a stateful-object in the given
;; world.
(define BallFactory%
  (class*
   object% (StatefulWorldObj<%>)

   (init-field world)        ; the World%
   (init-field box)              ; the Box%
   (init-field [speed 3])        ; the speed of the balls to be
                                 ; created.
                                        ; we make this different from
                                        ; the initial ball.
   (super-new)

   ;; ignore on-tick, on-mouse
   (define/public (on-tick) this)
   (define/public (on-mouse x y mev) this)

   ;; don't add yourself to the scene
   (define/public (add-to-scene s) s)

   ;; KeyEvent -> Void
   ;; catch "n" events here and create a new ball. 
    (define/public (on-key kev)
      (cond
        [(key=? kev "n") 
         (send this add-ball)]
        [else this]))                   ; the else is mandatory, but
                                        ; the value is ignored.

    ;; -> Void
    ;; EFFECT: create a new ball at the center of the box, and add it
    ;; to the world as a functional object.
    (define/public (add-ball)
      (local
       ((define the-ball
          (new Ball% 
               [x (send box get-x)]
               [y (send box get-y)]
               [box box]
               [speed speed])))
       (send world add-object the-ball)))

    ))



;; World%
;; the initial world consists of a box and a ball
;; the ball is functional, so it goes in "objects".
;; the box is stateful, so it goes in "stateful-objects".
(define init-world
  (local
   ((define the-box  (new Box% [x 100][y 45][w 150][h 75][selected? false]))
    (define the-ball (new Ball% [x 100][y 45][box the-box][speed 5]))
    (define the-world
      (new World%
           [objects          (list the-ball)]
           [stateful-objects (list the-box)])))
   ;; puzzle: why can't we add the BallFactory above?
   (send the-world
         add-stateful-object
         (new BallFactory%
              [world the-world]
              [box   the-box]))
   the-world))

;; main function (run-world).  Create a new world and run it.
;; since on-tick, etc, return Void, we need to explicitly pass back w
;; to big-bang.
;; World% -> World%
(define (run-world initial-world)
  (big-bang 
   initial-world
   (on-tick
    (lambda (w) (send w on-tick) w))
   (on-mouse
    (lambda (w x y evt) (send w on-mouse x y evt) w))
   (on-draw
    (lambda (w) (send w add-to-scene (empty-scene 400 300))))
   (on-key
    (lambda (w kev) (send w on-key kev) w))))

;; -> Container%
(define (run) (run-world init-world))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Create Abstractions to eliminate repeated code in testing

;;  Box<%> -> (list Integer Integer)
(define (get-edges box) (list (send box left-edge) (send box right-edge)))

;; World% (list Integer Integer) [String] -> Check.
;; RETURNS: a check that checks whether the object and stateful-object
;; lists in the given world have the given lengths
;; str is optional error message, default is empty string
(define (check-lengths w lst [str ""])
  (check-equal?
   (map length (send w for-test:get-all-objects))
   lst
   str))

(begin-for-test

  ;; simple test for adding balls

  (local
    ((define w init-world))
    (check-lengths w '(1 2) 
    "world should start with 1 functional object and 2 stateful ones")
    ;; now tell the world to create a balln
    (send w on-key "n")
    (check-lengths w '(2 2) "after 'n', there should be 2 balls"))


  ;; check dragging of wall

  (local
    ;; create a box and check its edges
    ((define box1 (new Box% [x 200][y 50][w 100][h 20][selected? false])))
    (check-equal? (get-edges box1) 
      (list 150 250) 
      "edges should be initialized correctly")
    ;; send the box a button-down
    (send box1 on-mouse 252 50 "button-down")
    ;; send the box a drag message
    (send box1 on-mouse 252 50 "drag")
    ;; now the edges of the box should have changed
    (check-equal? (get-edges box1) 
      (list 150 252) 
      "left edge should stay the same, right edge should change"))

  ;; check simple ball motion"
  ;; this a little different because the ball is still functional!

  (local
    ;; first create the objects
    ((define the-box  (new Box% [x 100][y 45][w 100][h 75][selected? false]))
     (define the-ball (new Ball% [x 100][y 45][box the-box][speed 5]))
     (define ball-after-tick  (send the-ball on-tick)))
  (check-equal?
    (send ball-after-tick for-test:get-x)
    (+ 100 5) 
    "ball should have moved 5 pixels"))

  ;; check ball bouncing off wall

  (local
    ;; first create the objects
    ((define the-box  (new Box% [x 100][y 45][w 100][h 75][selected? false]))
     ;; right edge of box is at 150.  So put the ball close to the edge.
     ;; center at edge - speed - radius + 1, so ball should bounce on next tick
     ;; x = 150 - 10 - 15 + 1 = 126
     (define the-ball (new Ball% [x 126][y 45][box the-box][speed 10]))
     (define ball-after-tick  (send the-ball on-tick)))
    ;; check to see that the speed is now -10
    (check-equal? 
      (send ball-after-tick for-test:get-speed)
      -10
      "after bounce, ball speed should be -10"))
        
  ;; check that ball doesn't bounce when wall is dragged
  
  (local
    ;; first create the objects
    ((define the-box  (new Box% [x 100][y 45][w 100][h 75][selected? false])))
    ;; right edge of box is at 150.  So put the ball close to the edge.
    ;; center at edge - speed - radius + 1, so ball should bounce on next tick
    ;; x = 150 - 10 - 15 + 1 = 126
    (define the-ball (new Ball% [x 126][y 45][box the-box][speed 10]))
    ;; select the box
    (send the-box on-mouse 152 50 "button-down")
    ;; drag the box
    (send the-box on-mouse 165 50 "drag")
    (check-equal?
      (send the-box right-edge)
      165
      "right edge of box should have moved to mouse location 154")
    (local
      ;; now ball shouldn't be bouncing
      ((define ball-after-tick (send the-ball on-tick)))
      ;; check to see that the speed is 1still 10
      (check-equal? 
        (send ball-after-tick for-test:get-speed)
        10
        "after wall drag, ball should still be moving right")))


;; this was a complicated test sequence I made up when I was trying to
;; figure out why balls were not being added correctly.
;; I tried adding balls directly to the world, then adding them
;; through more and more layers.
;; Eventually I figured it out...

 ;; "check that balls are added correctly"
  (local
  ;; first create a box, a ball in that box, and a world containing 
  ;; the ball and the box
    ((define the-box  (new Box% [x 100][y 45][w 150][h 75][selected? false]))
     (define the-ball (new Ball% [x 100][y 45][box the-box][speed 5]))
     (define the-world
    (new World%
      [objects          (list the-ball)]
      [stateful-objects (list the-box)])))
  (check-equal?
    (map length (send the-world for-test:get-all-objects))
    '(1 1)
    "check initial lengths of object lists")
  ;; add a ball directly to the world
  (send the-world add-object
    (new Ball% [x 100][y 45][box the-box][speed 5]))
  (check-equal?
    (map length (send the-world for-test:get-all-objects))
    '(2 1)
    "check adding a ball to the world")
  (local
  ;; create a factory
    ((define factory1
       (new BallFactory%
         [world the-world]
         [box       the-box])))
    ;; tell the factory directly to add a ball to the world
    (send factory1 add-ball)
    ;; along around here I got tired of  writing (map (length ...)),
    ;; so I defined check-lengths
    (check-lengths the-world '(3 1)
      "check adding a ball through a world")
    ;; now put the factory in the world
    (send the-world add-stateful-object factory1)
    (check-lengths the-world '(3 2)
    "factory should show up as another stateful object")
    ;; see if factory responds to on-key "n"
    (send factory1 on-key "n")
    (check-lengths the-world '(4 2) "factory should respond to on-key 'n'")
    ;; when I made the factory functional, everything worked up to here.
    ;; now try sending the "n" to  the world instead of the factory
    (send the-world on-key "n")
    (check-lengths the-world '(5 2)
      "world should respond to on-key 'n'")))
  
  )
