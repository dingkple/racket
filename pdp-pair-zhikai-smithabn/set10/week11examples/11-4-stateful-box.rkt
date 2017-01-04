#lang scheme

;; version 3: well, that didn't work.  In this version, we'll make the
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
  (class* object% (WorldObj<%>)
    (init-field objects)                  ;; ListOf<WorldObj<%>>
    (init-field [stateful-objects empty]) ;; ListOf<StatefulWorldObj<%>>
     
    (super-new)

    ;; -> World%
    (define/public (on-tick)
      (new World%
           [objects
            (map
             (lambda (obj) (send obj on-tick))
             objects)]
           [stateful-objects
             ;; send on-tick to each of the stateful objects.
             ;; these will change their state in response.
            (begin
              (for-each
               (lambda (stateful-obj) (send stateful-obj on-tick))
               stateful-objects)
              ;; the list of stateful objects doesn't change
              stateful-objects)]))

    ;; Integer Integer MouseEvt -> World%
    ;; We pass each mouse event to every object in the container
    ;; and let that object decide whether to respond to it.
    ;; hmm, what if we wanted a mouse event to create or remove
    ;; objects in the world?  We'll think about that later...
    (define/public (on-mouse x y mev)
      (new World%
           [objects
            (map
             (lambda (obj) (send obj on-mouse x y mev))
             objects)]
           [stateful-objects
            (begin
              (for-each
               (lambda (obj) (send obj on-mouse x y mev))
               stateful-objects)
              stateful-objects)]))
    
    ;; KeyEvent -> World%
    (define/public (on-key kev)
      (new World%
           [objects
            (map
             (lambda (obj) (send obj on-key kev))
             objects)]
           [stateful-objects
            (begin
              (for-each
               (lambda (obj) (send obj on-key kev))
               stateful-objects)
              stateful-objects)]))
    
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


;; NOW THE BOX IS STATEFUL

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
    ;; where w is the width of the box
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
      (new Ball%
        [x (- (send box right-edge) radius)]
        [y y]
        [box box]
        [speed (- speed)]))

    ; -> Ball%
    ; create a ball at the left edge
    (define (place-at-left-edge)
      (new Ball% 
        [x (+ (send box left-edge) radius)]
        [y y]
        [box box]
        [speed (- speed)]))

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

;; These no longer work, because Box% is now a StatefulWorldObj<%>,
;; not a WorldObj<%>.

;; ;; -> World%
;; ;; a world containing a single box
;; (define init-world1
;;   (new World%
;;        [objects
;;         (list
;;          (new Box% [x 100][y 45][w 100][h 75][selected? false]))]))

;; ;; a world containing two boxes
;; (define init-world2
;;   (new World%
;;     [objects
;;       (list
;;         (new Box% [x 100][y 45] [w 100][h 75][selected? false])
;;         (new Box% [x 100][y 200][w 100][h 75][selected? false]))]))

;; ;; a world containing a box and a ball
;; (define init-world3
;;   (local
;;     ((define the-box  (new Box% [x 100][y 45][w 150][h 75][selected? false]))
;;      (define the-ball (new Ball% [x 100][y 45][box the-box][speed 5])))
;;     (new World%
;;       [objects
;;         (list the-box the-ball)])))

;; an initial world consisting of a ball and a stateful box.
;; the ball is functional, so it goes in "objects".
;; the box is stateful, so it goes in "stateful-objects".
(define init-world4
  (local
   ((define the-box  (new Box% [x 100][y 45][w 150][h 75][selected? false]))
    (define the-ball (new Ball% [x 100][y 45][box the-box][speed 5])))
   (new World%
        [objects (list the-ball)]
        [stateful-objects (list the-box)])))

;; main function (run-world w). 

;; World% -> World%
(define (run-world initial-world)
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

;; -> World%
(define (run) (run-world init-world4))




