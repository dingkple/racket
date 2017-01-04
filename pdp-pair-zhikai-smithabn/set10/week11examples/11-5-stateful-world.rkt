#lang scheme

;; run with (run-world.v1 init-world).  Observe that this doesn't work
;; then run with (run-world.v2 init-world)

;; version 5: "n" adds a new ball.  To do this we'll do two things:
;; 1.  First, we'll write an add-object method in our container.
;; 2.  Then, we'll add a ball factory to our container.  The ball
;; factory will know about the container and the box, and on an "n" it
;; will add a ball by calling the add-object method on the container.

;; But wait: the factory needs to know about the container, so it can
;; send it add-object messages.  So this means the container must have
;; identity: it must be stateful.

;; So first, let's just make the container stateful.

;; =============================================================

;; version 4: In this version, we'll make the
;; box stateful.  To keep track of this we'll add a new interface:
;; StatefulWorldObj<%>. 

;; We use Void in the contracts to document this.
;; Void methods work by EFFECT.  They can return anything.
;; the contract Void means that whoever called the method promises to
;; ignore the value.

;; We'll expand the container to contain both ordinary (functional)
;; objects and stateful ones.

;; Then we'll redo Box% to be a StatefulWorldObj<%>.

;; version 2: add a bouncing ball to the container.  The ball will
;; bounce inside a box.

;; version 1: basic architecture for worlds with multiple objects in them.

;; start this with (run1).

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
    (init-field objects)                  ;; ListOf<WorldObj<%>>
    (init-field [stateful-objects empty]) ;; ListOf<StatefulWorldObj<%>>
     
    (super-new)

    ;; -> Void
    ;; EFFECT: pass on-tick to each of the functional objects, and
    ;; collect the result in the objects field; then have each of the
    ;; stateful objects perform its on-tick effect.
    (define/public (on-tick)
      ;; (new World%
      ;;      [objects
      ;;       (map
      ;;        (lambda (obj) (send obj on-tick))
      ;;        objects)]
      ;;      [stateful-objects
      ;;       (begin
      ;;         (for-each
      ;;          (lambda (obj) (send obj on-tick))
      ;;          stateful-objects)
      ;;         stateful-objects)])
      (set! objects
            (map 
             (lambda (obj) (send obj on-tick))
             objects))
      (for-each
       (lambda (obj) (send obj on-tick))
       stateful-objects))

    ;; Integer Integer MouseEvt -> Void
    ;; EFFECT: pass on-mouse to each of the functional objects, and
    ;; collect the result in the objects field; then have each of the
    ;; stateful objects perform its on-mouse effect.
    (define/public (on-mouse x y mev)
      (set! objects
            (map
             (lambda (obj) (send obj on-mouse x y mev))
             objects))
      (for-each
       (lambda (obj) (send obj on-mouse x y mev))
       stateful-objects))
       
    
    ;; KeyEvent -> Void
    ;; EFFECT: pass on-key to each of the functional objects, and
    ;; collect the result in the objects field; then have each of the
    ;; stateful objects perform its on-key effect.
    (define/public (on-key kev)
      (set! objects
            (map
             (lambda (obj) (send obj on-key kev))
             objects))
      (for-each
       (lambda (obj) (send obj on-key kev))
       stateful-objects))
    
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

    ;; doesn't respond to ticks or key events
    (define/public (on-tick) this)
    (define/public (on-key kev) this)

    ;; on-mouse: Integer Integer MouseEvent -> Void

    ;; EFFECT:
    ;; mouse-down near right edge of box => mark box selected
    ;; drag & selected => make right edge follow mouse
    ;; mouse-up => mark box unselected

    ;; if the mouse is dragging "near" the right edge of this box,
    ;; resize the box to match the given mouse posn, otherwise leave this
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
      ;; (new Box%
      ;;      [x (+ (send this left-edge) (/ new-width 2))]
      ;;      [y y]
      ;;      [w new-width]
      ;;      [h h]
      ;;      [objects objects]
      ;;      [selected? selected?])
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
     speed      ; ball speed in pixels/tick (either positive or negative)
     )
    
    (field [radius 20])
    
    (super-new)
    
    (define/public (add-to-scene s)
      (place-image
       (circle radius "outline" "red")
       x y s))
    
    ;; ball doesn't respond to mouse events or key events
    (define/public (on-mouse x y evt) this)
    (define/public (on-key kev) this)

    ; -> Ball%
    ; create a ball just like the current one, but where it should be
    ; after a tick
    (define/public (on-tick)
      (cond
        ((would-hit-right-edge?) (place-at-right-edge))
        ((would-hit-left-edge?) (place-at-left-edge))
        (else (new Ball% 
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
    
    ))

(begin-for-test

  (local
    ((define box1  (new Box% [x 100][y 45][w 150][h 75][selected? false]))
     (define ball1 (new Ball% [x 100][y 45][box box1][speed 5])))
    (check-equal? (send box1 left-edge) (- 100 75)))

)



;; World%
;; the initial world consists of a box and a ball
;; the ball is functional, so it goes in "objects".
;; the box is stateful, so it goes in "stateful-objects".
(define init-world
  (local
   ((define the-box  (new Box% [x 100][y 45][w 150][h 75][selected? false]))
    (define the-ball (new Ball% [x 100][y 45][box the-box][speed 5])))
   (new World%
        [objects (list the-ball)]
        [stateful-objects (list the-box)])))

;; main function (run-world).  
;; World% -> World%
(define (run-world.v1 initial-world)
  (big-bang 
   initial-world
   (on-tick
    (lambda (w) (send w on-tick)))
   (on-mouse
    (lambda (w x y evt) (send w on-mouse x y evt)))
   (on-draw
    (lambda (w) (send w add-to-scene (empty-scene 400 300))))
   (on-key
    (lambda (w kev) (send w on-key kev)))))

;; remember: contract for StatefulWorldObj<%> is that methods return Void, so we shouldn't rely
;; on what they return!
;; Here we are always returning the SAME World, but in different states.  
;; This is called the Singleton Pattern.
(define (run-world.v2 initial-world)
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

(define run-world run-world.v2)




