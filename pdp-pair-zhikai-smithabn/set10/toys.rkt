#lang racket
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)      

(provide 
 make-world
 run
 make-square-toy
 make-circle-toy
 World%
 SquareToy%
 CircleToy%
 StatefulWorld<%>
 StatefulToy<%>)

;; A world consists of a canvas of size 400*500 which
;; contains a target and list of toys(initially empty).

;; target is a circle image with radius 10 in outline mode which appears
;; at the centre of the canvas initially and is smoothly draggable

;; Press s for new square-shaped toy to pop up with its center located 
;; at the center of the target. When a squaretoy appears, it begins 
;; travelling right at a given speed. When it touches left or right wall
;; of the canvas, it starts moving again in the opposite direction

;; Press c for new circle-shaped toy to pop up with its center located  
;; at the center of the target. These circletoy do not move, but they  
;; alternate between solid red and solid green every 5 ticks. 
;; The circle is initially green.

;; start with (run framerate speed).  Ex: (run 0.25 8)

;; A toy is any object from a class implements StatefulToy<%>

;;CONSTANTS
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 500)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define HALF-WIDTH 200)
(define HALF-HEIGHT 250)

(define RED "red")
(define GREEN "green")

(define S-KEY "s")
(define C-KEY "c")
(define D-KEY "d")

(define DRAG "drag")
(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define LEAVE "leave")

(define TARGET-RADIUS 10)
(define OUTLINE "outline")
(define SOLID "solid")

(define SQUARE-TOY-LENGTH 40)
(define HALF-SQUARE-LEN 20)

(define CIRCLE-TOY-RADIUS 5)


;; a ListOfStatefulToy<%> (LOT) is one of:
;;  --empty
;;  --(cons StatefulToy<%> LOT)
;; Interp:
;; a LOT is either empty, or a list of StatefulToy<%>(for StatefulToy<%> see 
;;  below)
;; Template:
;; lot-fn: LOT -> ??
;; (define (lot-fn lot)
;;   (cond
;;     [(empty? lot) ...]
;;     [else (...(first lot)
;;               (lot-fn (rest lot)))]))

;; a ColorString is one of:
;;  --GREEN
;;  --RED
;; Interp:
;; just colors
;; Template:
;; c-fn: ColorString -> ??
;; (define (c-fn c)
;;   (cond
;;     [(string=? c GREEN) ...]
;;     [(string=? c RED) ...]))

;; Interfaces:

(define StatefulWorld<%>
  (interface () 
    ;; -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this StatefulWorld<%> to the state that it should be 
    ;; in after a tick.
    on-tick                             
    
    ;; Integer Integer MouseEvent -> Void
    ;; GIVEN: a x-pos, a y-pos and a MouseEvent
    ;; EFFECT: updates this StatefulWorld<%> to the state that it should be in
    ;; after the given MouseEvent
    on-mouse
    
    ;; KeyEvent -> Void
    ;; GIVEN: a KeyEvent
    ;; EFFECT: updates this StatefulWorld<%> to the state that it should be in
    ;; after the given KeyEvent
    on-key
    
    ;; -> Scene
    ;; GIVEN: no arguments
    ;; Returns a Scene depicting this world StatefulWorld<%>
    ;; on it.
    on-draw 
    
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURN: the x and y coordinates of the target
    target-x
    target-y
    
    ;; -> Boolean
    ;; GIVEN: no arguments
    ;; RETURN: Is the target selected?
    target-selected?
    
    ;; -> ListOfStatefulToy<%>
    ;; GIVEN: no arguments
    ;; RETURN: the list of the toys in this world
    get-toys
    
    ))

(define StatefulToy<%> 
  (interface ()
    
    ;; -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this StatefulToy<%> to the state it should be 
    ;; in after a tick
    on-tick                             
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURN: a Scene like the given one, but with this StatefulToy<%> 
    ;; drawn on it.
    add-to-scene
    
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURN: the toy's x-pos
    ;; -> Integer
    ;; GIVEN: no argument
    ;; RETURN: the toy's y-pos
    toy-x
    toy-y 
    
    ;; -> ColorString
    ;; GIVEN: no arguments
    ;; RETURN: the current color of this StatefulToy<%>
    toy-color
    
    ;; -> Boolean
    ;; GIVEN: no arguments
    ;; RETURNS: true iff this StatefulToy<%> is SquareToy
    for-test:get-is-square?
    
    ;; for-test:get-t-num:  -> NonNegInt
    ;; GIVEN: no arguments
    ;; RETURNS: the tick counter of this StatefulToy<%>
    for-test:get-t-num
    
    ))

;; World%     -- a class implements the StatefulWorld<%> interface.
;; A World% is a (new World% [t-x Integer] [t-y Integer][speed PosInt]
;;       [vec-x Integer][vec-y Integer][t-selected? Boolean][toys LOT])
;; Interpretation: represents a world, containing a
;; target x and y center positions
;; speed at which squaretoys should move
;; vec-x and vec-y are relative x and y positions from target's 
;; x and y original position to the mouse's current x & y position
;; (i.e these are used for smooth dragging)
;; t-selelcted? indicates target selected status, true for selected
;; toys is the list of toys in this world
(define World%
  (class* object% (StatefulWorld<%>)
    
    ;;Integer, t-x is the target's center x position
    ;;Integer, t-y is the target's center y position
    (init-field 
     t-x
     t-y)
    
    ;;PosInt, speed is the moving speed of the square shaped toys
    (init-field speed)
    
    ;; Integer, vec-x is the relative x position from the target's 
    ;; original position to the mouse's current x position
    ;; Integer, vec-y is the relative y position from the target's 
    ;; original position to the mouse's current y position
    (init-field
     vec-x
     vec-y)
    
    ;; Boolean, t-selected? is the target's selected? status
    ;; initially false.
    (init-field [t-selected? false])
    
    ;; LOT, toys is the list of toys in the world
    ;; initially empty.
    (init-field [toys empty])  
    
    ;;Image, image which represent the target
    (field [TARGET-IMG (circle TARGET-RADIUS OUTLINE RED)])
    
    (super-new)
    
    ;; on-draw: -> Scene 
    ;; GIVEN: no arguments
    ;; RETURNS: a scene like the given one, but with this world painted
    ;; on it.
    ;; EXAMPLES: Refer tests
    (define/public (on-draw)
      (foldr
       ;; StatefulToy<%> Scene -> Scene
       ;; GIVEN: a toy and a scene
       ;; WHERE: the scence contains the toy already processed
       ;; RETURNS: the given scene with the given toy drew on
       (lambda (toy ans)
         (send toy add-to-scene ans))
       (place-image TARGET-IMG t-x t-y EMPTY-CANVAS)
       toys))
    
    ;; on-tick: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this World% to the state that it should be
    ;; in after a tick.
    ;; EXAMPLES: Refer tests
    (define/public (on-tick)
      (for-each
       ;; StatefulToy<%> -> StatefulToy<%>
       ;; GIVEN: a toy
       ;; RETURNS: void, but update the given toy to next tick
       (lambda (t)
         (send t on-tick))
       toys))     
    
    ;; on-key: KeyEvent -> Void
    ;; GIVEN: a KeyEvent
    ;; RETURN: void, but updates this World% to the state that it should be in
    ;; after the given KeyEvent
    ;; EXAMPLES: Refer tests
    ;; STRATEGY: Cases on kev: KeyEvent
    (define/public (on-key kev)
      (cond
        [(key=? kev S-KEY) 
         (set! toys (cons (make-square-toy t-x t-y speed) toys))]
        [(key=? kev C-KEY) 
         (set! toys (cons (make-circle-toy t-x t-y) toys))]
        [else this]))
    
    ;; on-mouse: Integer Integer MouseEvent -> Void
    ;; GIVEN: mouse mx and my positions and a MouseEvent
    ;; RETURNS: void, updates this world to the state that it should be in
    ;; after the given MouseEvent
    ;; EXAMPLES: Refer tests
    ;; STRATEGY: Cases on evt: MouseEvent
    (define/public (on-mouse mx my evt)
      (cond
        [(mouse=? evt DRAG)       
         (send this target-after-drag mx my)]
        [(mouse=? evt BUTTON-DOWN)       
         (send this target-after-button-down mx my)]
        [(mouse=? evt BUTTON-UP)       
         (send this target-after-button-up mx my)]
        [else this]))
    
    ;; target-after-drag: Integer Integer -> Void
    ;; GIVEN: a mouse-position(x-pos and y-pos)
    ;; RETURNS: void, but update the position of the target
    ;; EXAMPLES: if this world's fields are like:[t-x 100] [t-y 140]
    ;; [vec-x 4] [vec-y 4], and mx = 145, my = 150, after calling this method, 
    ;; the fields would be: [t-x 149] [t-y 154] [vec-x 4] [vec-y 4]
    (define/public (target-after-drag mx my)
      (if t-selected?
          (begin
            (set! t-x (+ mx vec-x))
            (set! t-y (+ my vec-y)))    
          this))
    
    ;; target-after-button-down : Integer Integer -> Void
    ;; GIVEN: a mouse-position(x-pos and y-pos)
    ;; RETURNS: void, but update vec-x, vec-y and t-selected?
    ;; EXAMPLES: if this world's fields are like:[t-x 100] [t-y 140]
    ;; [vec-x 0] [vec-y 0], and mx = 104, my = 141, after calling this method, 
    ;; the fields would be: [t-x 104] [t-y 141] [vec-x 4] [vec-y 1]    
    (define/public (target-after-button-down mx my)
      (if (in-target? mx my)
          (begin
            (set! vec-x (- t-x mx))
            (set! vec-y (- t-y my))
            (set! t-selected? true))
          this))
    
    ;; target-after-button-up: Integer Integer -> Void
    ;; GIVEN: a mouse-position(x-pos and y-pos)
    ;; RETURNS: Void, but update t-x t-y vec-x vec-y and t-selected?
    ;; EXAMPLES: if this world's fields are like:[t-x 100] [t-y 140]
    ;; [vec-x 4] [vec-y 4] [t-selected? false], and mx = 145, my = 150, after 
    ;; calling this method, the fields would be: [t-x 149] [t-y 154] [vec-x 0]
    ;; [vec-y 0] [t-selected? false]
    (define/public (target-after-button-up mx my)
      (if (and (in-target? mx my) t-selected?)      
          (begin
            (set! t-x (+ mx vec-x))
            (set! t-y (+ my vec-y))
            (set! vec-x 0)
            (set! vec-y 0)
            (set! t-selected? false))
          this))
    
    ;; in-target?: Integer Integer -> Boolean
    ;; GIVEN: mx and my are mouse x and y positions
    ;; RETURNS: iff the given mouse position is inside the target,
    ;;          return true, else return false
    ;; EXAMPLES: (send ori-world in-target? HALF-WIDTH HALF-WIDTH) ->
    ;;             -> true
    (define/public (in-target? mx my)
      (<= (+ (sqr (- mx t-x)) (sqr (- my t-y))) (sqr TARGET-RADIUS)))
    
    ;; target-x: -> Integer
    ;; GIVEN: no arguments
    ;; RETURN: the target's x position
    ;; EXAMPLES: (send ori-world target-x) -> HALF-WIDTH
    (define/public (target-x)
      t-x)
    
    ;; target-y: -> Integer
    ;; GIVEN: no arguments
    ;; RETURN: the target's y position
    ;; EXAMPLES: (send ori-world target-y) -> HALF-HEIGHT
    (define/public (target-y)
      t-y)
    
    ;; target-selected?:  -> Boolean
    ;; GIVEN: no arguments
    ;; RETURNS: if the target is selected
    ;; EXAMPLES: (send ori-world target-selected?) -> false
    (define/public (target-selected?)
      t-selected?)
    
    ;; get-toys:  -> LOT
    ;; GIVEN: no arguments
    ;; RETURNS: all the toys in this world
    ;; EXAMPLES: (send ori-world get-toys) -> empty
    (define/public (get-toys)
      toys)
    
    ))

;; SquareToy% -- a class satisfies the StatefulToy<%> interface
;; A SquareToy% is a (new SquareToy% [x Integer] [y Integer]
;;       [speed PosInt] [is-right? Boolean])
;; Interpretation: represents a squaretoy, containing a 
;; x and y center positions of a squaretoy
;; speed at which squaretoy should move
;; is-right? indicates whether the squaretoy is moving towards right,
;; true for right
(define SquareToy%
  (class* object% (StatefulToy<%>)
    ;;Integer, x is the center x position of this squaretoy
    ;;Integer, y is the center y position of this squaretoy
    (init-field 
     x
     y)
    
    ;;PosInt, speed is the SquareToy's moving speed
    (init-field speed)
    
    ;;Boolean, is-right? indicates which direction the toy is moving towards.
    ;;true iff right, so initially set to true
    (init-field [is-right? true])
    
    ;;ColorString, color indicates the square toy's color set to green
    (field [color GREEN])
    ;;Image, image which represents the squaretoy
    (field [SQUARE-TOY-IMG 
            (rectangle SQUARE-TOY-LENGTH SQUARE-TOY-LENGTH OUTLINE color)])
    
    
    (super-new)
    
    ;; toy-x:  -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: this toy's x position
    ;; EXAMPLES: (send ori-squaretoy toy-x) -> HALF-WIDTH
    (define/public (toy-x)
      x)
    
    ;; toy-y:  -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: this toy's y position
    ;; EXAMPLES: (send ori-squaretoy toy-y) -> HALF-HEIGHT
    (define/public (toy-y)
      y)
    
    ;; toy-color:  -> ColorString
    ;; GIVEN: no arguments
    ;; RETURNS: this toy's color
    ;; EXAMPLES: (send ori-squaretoy toy-color) -> GREEN
    (define/public (toy-color)
      color)
    
    ;; for-test:get-is-square?:  -> Boolean
    ;; GIVEN: no arguments
    ;; RETURNS: true, indicating this is a square-toy
    ;; EXAMPLES: (send ori-squaretoy for-test:get-is-square?) -> true
    (define/public (for-test:get-is-square?)
      true)
    
    ;; for-test:get-t-num:  -> NonNegInt
    ;; GIVEN: no arguments
    ;; RETURNS: the tick counter of the squaretoy
    ;; EXAMPLES: (send ori-squaret`oy for-test:get-t-num) -> 0
    (define/public (for-test:get-t-num)
      0)   
    
    ;; on-tick:  -> Void
    ;; GIVEN: no arguments
    ;; RETURNS: updates this SquareToy to the state it should be in 
    ;; after a tick. 
    ;; EXAMPLES: Refer tests
    (define/public (on-tick)
      (local
        ((define new-x (send this next-x-pos)))
        (set! x (send this bounce-position-back new-x))
        (set! is-right? (send this bounce-direction-back new-x))
        this))
    
    
    ;; next-x-pos:  -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: the x-pos of the toy at next tick before bounced back
    ;; EXAMPLES: (send ori-squaretoy next-x-pos) -> (+ HALF-WIDTH speed-example)
    (define/public (next-x-pos)
      (if is-right?
          (+ x speed)
          (- x speed)))
    
    ;; bounce-position-back: Integer -> PosInt
    ;; GIVEN: the x-pos of the toy after a tick
    ;; RETURNS: the squaretoy's x position after a tick, bounced back
    ;; EXAMPLES: (send ori-squaretoy bounce-position-back (+ HALF-WIDTH 8))
    ;;             -> (+ HALF-WIDTH 8)
    ;; STRATEGY: Cases on new-x: Integer
    (define/public (bounce-position-back new-x)
      (cond
        [(< new-x  HALF-SQUARE-LEN) HALF-SQUARE-LEN]
        [(<= HALF-SQUARE-LEN new-x (- CANVAS-WIDTH HALF-SQUARE-LEN)) 
         new-x]
        [(> new-x (- CANVAS-WIDTH HALF-SQUARE-LEN)) 
         (- CANVAS-WIDTH HALF-SQUARE-LEN)]))
    
    ;; bounce-direction-back: Integer -> Boolean
    ;; GIVEN:  the x-pos of the toy at next tick
    ;; RETURNS: the direction of the squaretoy's direction after one tick
    ;;       set to opposite direction iff touching left or right wall of 
    ;;       the canvas
    ;; EXAMPLES: (send ori-squaretoy bounce-direction-back (+ HALF-WIDTH 8))
    ;;             -> true
    (define/public (bounce-direction-back new-x)
      (if (< HALF-SQUARE-LEN new-x (- CANVAS-WIDTH HALF-SQUARE-LEN))
          is-right?
          (not is-right?)))
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: the given scene with this toy drew on it
    ;; EXAMPLES: Refer tests
    (define/public (add-to-scene scene)
      (place-image SQUARE-TOY-IMG x y scene))
    
    ))

;; CircleToy% -- a class satisfies the StatefulToy<%> interface
;; A CircleToy% is a (new CircleToy% [t-num NonNegInt] [x Integer] 
;;                                   [y Integer][color ColorString])
;; Interpretation: represents a circletoy, containing a 
;; t-num is a counter to maintain number of ticks used for setting
;;    the color of this circletoy 
;; x and y center positions of a circle
;; color of the current color of circletoy 
(define CircleToy%
  (class* object% (StatefulToy<%>)
    ;;NonNegInt, t-num is a counter to maintain number of ticks 
    ;;used for setting the color of this circletoy
    ;;where: 0 <= t-num <= 9
    (init-field t-num)
    
    ;;Integer, x is the center x position of this circletoy
    (init-field x)
    ;;Integer, y is the center y position of this circletoy
    (init-field y)
    
    ;;ColorString, color is the current toy's color
    ;;Initial color of circletoy is set to GREEN
    (init-field [color GREEN])
    
    (super-new)
    
    ;; toy-x:  -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: this toy's current x position
    ;; EXAMPLES: (send ori-circletoy toy-x) -> HALF-WIDTH
    (define/public (toy-x)
      x)
    
    ;; toy-y:  -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: this toy's current y position
    ;; EXAMPLES: (send ori-circletoy toy-y) -> HALF-HEIGHT
    (define/public (toy-y)
      y)
    
    ;; toy-color:  -> ColorString
    ;; GIVEN: no arguments
    ;; RETURNS: the current color of this toy
    ;; EXAMPLES: (send ori-circletoy toy-color) -> GREEN
    (define/public (toy-color)
      color)
    
    ;; for test:get-t-num:  -> NonNegInt
    ;; GIVEN: no arguments
    ;; RETURNS: the counter of the circletoy
    ;; EXAMPLES: (send ori-circletoy for-test:get-t-num) -> 0
    (define/public (for-test:get-t-num)
      t-num)
    
    ;; for-test:get-is-square?:  -> Boolean
    ;; GIVEN: no arguments
    ;; RETURNS: true iff this is a sqaure-toy
    ;; EXAMPLES: (send ori-circletoy for-test:get-is-square?) -> false
    (define/public (for-test:get-is-square?)
      false)
    
    ;; on-tick:  -> Void
    ;; GIVEN: no arguments
    ;; RETURNS: updates this CircleToy to the state it should be in after a
    ;; tick. 
    ;; EXAMPLES: Refer tests
    
    (define/public (on-tick)
      (local
        ((define new-color 
           (if (< (modulo (+ 1 t-num) 10) 5) GREEN RED)))
        (set! t-num (if (= (modulo (+ 1 t-num) 10) 0) 0 (+ t-num 1)))
        (set! color new-color)
        this))
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: the given scene with this circletoy drew on it
    ;; EXAMPLES: Refer tests
    (define/public (add-to-scene scene)
      (local 
        ((define IMG (circle CIRCLE-TOY-RADIUS SOLID color)))
        (place-image IMG x y scene)))
    
    ))

;; make-world : PosInt -> World%
;; GIVEN: a speed
;; RETURNS: a world with a target with its center at center of canvas, 
;; but no toys, and in which any toys created in the future will travel
;; at the given speed (in pixels/tick).
;; EXAMPLES: Refer tests
(define (make-world s)
  (new World%
       [t-x HALF-WIDTH]
       [t-y HALF-HEIGHT]
       [speed s]
       [vec-x 0]
       [vec-y 0]))


;; make-square-toy : PosInt PosInt PosInt -> SquareToy%
;; GIVEN: an x and a y center positions of a squaretoy, 
;; and a speed(in pixels/tick)
;; RETURNS: an object representing a square toy at the given position,
;; travelling right at the given speed.
;; EXAMPLES: Refer tests
(define (make-square-toy x y s)
  (new SquareToy%
       [x x]
       [y y]
       [speed s]))

;; make-circle-toy : PosInt PosInt -> CircleToy%
;; GIVEN: an x and a y center positions of a circletoy
;; RETURNS: an object representing a circle toy at the given position.
;; EXAMPLES: Refer tests
(define (make-circle-toy x y)
  (new CircleToy%
       [t-num 0]
       [x x]
       [y y]
       [color GREEN]))

;; run : PosNum PosInt -> StatefulWorld<%>
;; GIVEN: a frame rate (in seconds/tick) and a square-speed (in pixels/tick),
;; creates and runs a world.  
;; RETURN: the final state of the world
(define (run rate speed)
  (big-bang (make-world speed)
            (on-tick
             ;; World% -> World%
             ;; GIVEN: a world
             ;; EFFECT: updates this World% to the state  
             ;; that it should be in after a tick.
             ;; RETURN: the given world after updating
             (lambda (w) (send w on-tick) w)
             rate)
            (on-draw
             ;; World% -> Scene
             ;; GIVEN: a world
             ;; RETURNS: a scene that describes the world
             (lambda (w) (send w on-draw)))
            (on-key
             ;; World% KeyEvent -> World%
             ;; GIVEN: a world
             ;; EFFECT: updates this World% to the state that it 
             ;; should be in after the given KeyEvent
             ;; RETURN: the given world after updating             
             (lambda (w kev) (send w on-key kev) w))
            (on-mouse
             ;; World% Integer Integer MouseEvent -> World%
             ;; GIVEN: a world, a x and y mouse posiitions and a MouseEvent
             ;; EFFECT: updates this World% to the state that 
             ;; it should be in after the given MouseEvent
             ;; RETURN: the given world after updating             
             (lambda (w x y evt) (send w on-mouse x y evt) w))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For TEST:

(define speed-example 8)

;; world-equal?: World% World% -> Boolean
;; GIVEN: two world
;; RETURNS: true iff the given two world are the same
;; EXAMPLES:(world-equal ori-world ori-world-drag) -> false
;; STRATEGY: Morally, it's structure decomposition on w1 and w2(both world)
(define (world-equal? w1 w2)
  (and
   (= (send w1 target-x)
      (send w2 target-x))
   (= (send w1 target-y)
      (send w2 target-y))
   (toys-equal? (send w1 get-toys) (send w2 get-toys))
   (equal? (send w1 target-selected?) (send w2 target-selected?))))

;; toy-equal?: StatefulToy<%> StatefulToy<%> -> Boolean
;; GIVEN: two toys
;; RETURNS: true iff the given two toys are equal
;; EXAMPLES: (circle-toy-equal? ori-squaretoy ori-circletoy) -> false
;; STRATEGY: function composition
(define (toy-equal? t1 t2)
  (and
   (or (and (send t1 for-test:get-is-square?) 
            (send t2 for-test:get-is-square?))
       (and (not (send t1 for-test:get-is-square?)) 
            (not (send t2 for-test:get-is-square?))))
   (or (and (send t1 for-test:get-is-square?) (square-toy-equal? t1 t2))
       (circle-toy-equal? t1 t2))))

;; sqaure-toy-equal?: SquareToy% SquareToy% -> Boolean
;; GIVEN: two squaretoys
;; RETURNS: true iff the given two squaretoys are equal
;; EXAMPLES: (sqaure-toy-equal? ori-squaretoy-1-tick ori-squaretoy-5-tick)
;;               -> false
;; STRATEGY: Morally, this is structure decomposition on t1 and t2:
;;            (both SquareToy)
(define (square-toy-equal? t1 t2)
  (and 
   (= (send t1 toy-x) (send t2 toy-x))
   (= (send t1 toy-y) (send t2 toy-y))
   (= (send t1 for-test:get-t-num) (send t2 for-test:get-t-num))
   (equal? (send t1 toy-color) (send t2 toy-color))))

;; circle-toy-equal?: CircleToy% CircleToy% -> Boolean
;; GIVEN: two circletoys
;; RETURNS: true iff the given two circletoys are equal
;; EXAMPLES: (circle-toy-equal? ori-circletoy-1-tick ori-circletoy-5-tick)
;;             -> false
;; STRATEGY: morally, it's struction decomposition on t1 and t2
;;              (both CircleToy)
(define (circle-toy-equal? t1 t2)
  (and
   (= (send t1 for-test:get-t-num) (send t2 for-test:get-t-num))
   (= (send t1 toy-x) (send t2 toy-x))
   (= (send t1 toy-y) (send t2 toy-y))
   (equal? (send t1 toy-color) (send t2 toy-color))))


;; toys-equal?: LOT LOT -> Boolean
;; GIVEN: two lists of toys
;; RETURNS: true iff the given two list of toys are the same
;; EXAMPLES: (toys-equal? (list ori-squaretoy) empty) -> false
;; STRATEGY: structure decomposition on tl1 and tl2(both LOT)
(define (toys-equal? tl1 tl2)
  (cond
    [(and (empty? tl1) (empty? tl2)) true]
    [(or (empty? tl1) (empty? tl2)) false]
    [else (and (toy-equal? (first tl1) (first tl2))
               (toys-equal? (rest tl2) (rest tl2)))]))

;; test-world-after-tick: StatefulWorld<%> PosInt -> StatefulWorld<%>
;; GIVEN: a world and a positive integer
;; RETURNS: the given world after the given number of ticks
;; HALTING MEASURE: n
;; TERMINATION ARGUMENT: every time n is decreased by 1, when it's 0, it halts
;; EXAMPLES: (test-world-after-tick ori-world-0-tick 1) -> ori-world-1-tick
;; STRATEGY: general recursion
(define (test-world-after-tick w n)
  (for-each
   (lambda (p)
     (send w on-tick))
   (make-list n 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES for tests:

(begin-for-test  
  (local
    ((define ori-world 
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]))
     (define ori-squaretoy
       (new SquareToy%
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [speed speed-example]))
     (define ori-world-s-key
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]
            [toys (list ori-squaretoy)])))
    (send ori-world on-key S-KEY)
    (check world-equal? 
           ori-world
           ori-world-s-key
           "check S-KEY key event creates squaretoy"))
  
  
  (local
    ((define ori-world 
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]))
     (define ori-circletoy
       (new CircleToy%
            [t-num 0]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [color GREEN]))     
     (define ori-world-c-key
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]
            [toys (list ori-circletoy)])))
    (send ori-world on-key C-KEY)
    (check world-equal? 
           ori-world 
           ori-world-c-key
           "check C-KEY key event creates circle"))
  
  (local
    ((define ori-world 
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0])))
    (send ori-world on-key D-KEY)
    (check world-equal? 
           ori-world 
           ori-world
           "check any other key event, no change in world"))
  
  (local
    ((define ori-world 
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]))
     (define ori-world-button-down 
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x -5]
            [vec-y 0]
            [t-selected? true])))
    (send ori-world on-mouse 
          (+ HALF-WIDTH 5)
          HALF-HEIGHT
          BUTTON-DOWN)
    (check world-equal? 
           ori-world 
           ori-world-button-down
           "check button down mouse event"))
  
  (local
    ((define ori-world-button-down 
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x -5]
            [vec-y 0]
            [t-selected? true]))
     (define ori-world-drag 
       (new World%
            [t-x (+ HALF-WIDTH 3)]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x -5]
            [vec-y 0]
            [t-selected? true])))
    (send ori-world-button-down on-mouse
          (+ HALF-WIDTH 8)
          HALF-HEIGHT
          DRAG)
    (check world-equal? 
           ori-world-button-down 
           ori-world-drag
           "check drag mouse event"))
  
  (local
    ((define ori-world-drag 
       (new World%
            [t-x (+ HALF-WIDTH 3)]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x -5]
            [vec-y 0]
            [t-selected? true]))
     (define ori-world-button-up
       (new World%
            [t-x (+ HALF-WIDTH 3)]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]))) 
    (send ori-world-drag on-mouse
          (+ HALF-WIDTH 8)
          HALF-HEIGHT
          BUTTON-UP)
    (check world-equal? 
           ori-world-drag 
           ori-world-button-up
           "check button up mouse event"))
  
  (local
    ((define ori-world 
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0])))
    (send ori-world on-mouse
          (+ HALF-WIDTH 8)
          HALF-HEIGHT
          BUTTON-UP)
    (check world-equal? 
           ori-world 
           ori-world
           "check button up mouse event"))
  
  (local
    ((define ori-world 
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0])))
    (send ori-world on-mouse
          (+ HALF-WIDTH 8)
          HALF-HEIGHT
          DRAG)
    (check world-equal? 
           ori-world 
           ori-world
           "check drag mouse event"))
  
  
  (local
    ((define ori-world 
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0])))
    (send ori-world on-mouse
          (+ HALF-WIDTH 18)
          HALF-HEIGHT
          BUTTON-DOWN)
    (check world-equal? 
           ori-world 
           ori-world
           "check button down mouse event"))
  
  
  (local
    ((define ori-world 
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0])))
    (send ori-world on-mouse
          (+ HALF-WIDTH 600)
          HALF-HEIGHT
          LEAVE)
    (check world-equal? 
           ori-world 
           ori-world
           "check any other mouse event"))
  
  (local
    ((define ori-squaretoy-need-bounce-at-left
       (new SquareToy%
            [x (+ HALF-SQUARE-LEN 3)]
            [y HALF-HEIGHT]
            [speed speed-example]
            [is-right? false]))
     (define ori-squaretoy-at-left
       (new SquareToy%
            [x HALF-SQUARE-LEN]
            [y HALF-HEIGHT]
            [speed speed-example]
            [is-right? true])))
    (send ori-squaretoy-need-bounce-at-left on-tick)
    (check toy-equal? 
           ori-squaretoy-need-bounce-at-left 
           ori-squaretoy-at-left
           "check square toy bouncing of left wall"))
  
  
  (local
    ((define ori-squaretoy-need-bounce-at-right
       (new SquareToy%
            [x (- CANVAS-WIDTH HALF-SQUARE-LEN 5)]
            [y HALF-HEIGHT]
            [speed speed-example]))
     (define ori-squaretoy-at-right
       (new SquareToy%
            [x (- CANVAS-WIDTH HALF-SQUARE-LEN)]
            [y HALF-HEIGHT]
            [speed speed-example]
            [is-right? false])))
    (send ori-squaretoy-need-bounce-at-right on-tick)
    (check toy-equal? 
           ori-squaretoy-need-bounce-at-right 
           ori-squaretoy-at-right
           "check square toy bouncing of right wall"))
  
  
  (local
    ((define ori-squaretoy
       (new SquareToy%
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [speed speed-example]))
     (define ori-circletoy
       (new CircleToy%
            [t-num 0]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [color GREEN]))
     (define ori-world-0-tick
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]
            [toys (list ori-circletoy ori-squaretoy)]))
     (define ori-squaretoy-1-tick
       (new SquareToy%
            [x (+ HALF-WIDTH speed-example)]
            [y HALF-HEIGHT]
            [speed speed-example]))     
     (define ori-circletoy-1-tick
       (new CircleToy%
            [t-num 1]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]))     
     (define ori-world-1-tick
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]
            [toys (list ori-circletoy-1-tick ori-squaretoy-1-tick)])))
    (test-world-after-tick ori-world-0-tick 1)
    (check world-equal? 
           ori-world-0-tick 
           ori-world-1-tick
           "check world after one tick"))
  
  
  (local
    ((define ori-squaretoy
       (new SquareToy%
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [speed speed-example]))
     (define ori-circletoy
       (new CircleToy%
            [t-num 0]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [color GREEN]))
     (define ori-world-0-tick
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]
            [toys (list ori-circletoy ori-squaretoy)]))
     (define ori-squaretoy-5-tick
       (new SquareToy%
            [x (+ HALF-WIDTH (* 5 speed-example))]
            [y HALF-HEIGHT]
            [speed speed-example]))     
     (define ori-circletoy-5-tick
       (new CircleToy%
            [t-num 5]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [color RED]))
     (define ori-world-5-tick
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]
            [toys (list ori-circletoy-5-tick ori-squaretoy-5-tick)])))
    (test-world-after-tick ori-world-0-tick 5)
    (check world-equal? 
           ori-world-0-tick 
           ori-world-5-tick
           "check world after five ticks"))
  
  (check world-equal?
         (make-world speed-example) 
         (new World%
              [t-x HALF-WIDTH]
              [t-y HALF-HEIGHT]
              [speed speed-example]
              [vec-x 0]
              [vec-y 0])
         "check if make-world creates world as given")
  
  (local
    ((define ori-squaretoy
       (new SquareToy%
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [speed speed-example]))
     (define ori-circletoy
       (new CircleToy%
            [t-num 0]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [color GREEN]))
     (define ori-world-0-tick
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]
            [toys (list ori-circletoy ori-squaretoy)])))
    (check-equal? 
     (send ori-world-0-tick on-draw)
     (place-image
      (circle CIRCLE-TOY-RADIUS SOLID GREEN)
      HALF-WIDTH
      HALF-HEIGHT
      (place-image
       (rectangle SQUARE-TOY-LENGTH SQUARE-TOY-LENGTH OUTLINE GREEN)
       HALF-WIDTH
       HALF-HEIGHT
       (place-image
        (circle TARGET-RADIUS OUTLINE RED)
        HALF-WIDTH
        HALF-HEIGHT
        EMPTY-CANVAS)))
     "check on-draw function of the world"))
  
  
  (local
    ((define ori-squaretoy-5-tick
       (new SquareToy%
            [x (+ HALF-WIDTH (* 5 speed-example))]
            [y HALF-HEIGHT]
            [speed speed-example]))     
     (define ori-circletoy-5-tick
       (new CircleToy%
            [t-num 5]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [color RED]))
     (define ori-world-5-tick
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]
            [toys (list ori-circletoy-5-tick ori-squaretoy-5-tick)])))   
    (check-equal? 
     (send ori-world-5-tick on-draw)
     (place-image
      (circle CIRCLE-TOY-RADIUS SOLID RED)
      HALF-WIDTH
      HALF-HEIGHT
      (place-image
       (rectangle SQUARE-TOY-LENGTH SQUARE-TOY-LENGTH OUTLINE GREEN)
       (+ HALF-WIDTH (* 5 speed-example))
       HALF-HEIGHT
       (place-image
        (circle TARGET-RADIUS OUTLINE RED)
        HALF-WIDTH
        HALF-HEIGHT
        EMPTY-CANVAS)))
     "check on-draw function of the world after five ticks"))
  
  (local
    ((define ori-squaretoy
       (new SquareToy%
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [speed speed-example]))
     (define ori-circletoy
       (new CircleToy%
            [t-num 0]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [color GREEN]))
     (define ori-world-0-tick
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]
            [toys (list ori-circletoy ori-squaretoy)]))
     (define ori-squaretoy-10-tick
       (new SquareToy%
            [x (+ HALF-WIDTH (* 10 speed-example))]
            [y HALF-HEIGHT]
            [speed speed-example]))
     (define ori-circletoy-10-tick
       (new CircleToy%
            [t-num 0]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [color GREEN]))
     (define ori-world-10-tick
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]
            [toys (list ori-circletoy-10-tick ori-squaretoy-10-tick)])))
    (test-world-after-tick ori-world-0-tick 10)
    (check world-equal? 
           ori-world-0-tick 
           ori-world-10-tick
           "check world after ten ticks"))
  
  
  (local
    ((define ori-squaretoy
       (new SquareToy%
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [speed speed-example]))
     (define ori-circletoy
       (new CircleToy%
            [t-num 0]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [color GREEN]))
     (define ori-world-0-tick
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [speed speed-example]
            [vec-x 0]
            [vec-y 0]
            [toys (list ori-circletoy ori-squaretoy)])))
    (check-equal? 
     (send ori-world-0-tick target-x)
     HALF-WIDTH
     "check target x position")
    
    (check-equal? 
     (send ori-world-0-tick target-y)
     HALF-HEIGHT
     "check target y position")
    
    (check-equal? 
     (send ori-world-0-tick target-selected?)
     false
     "check target-selected? status")
    
    (check-equal? 
     (send ori-world-0-tick get-toys)
     (list ori-circletoy ori-squaretoy)
     "check get-toys"))
  
  
  (local
    ((define ori-squaretoy
       (new SquareToy%
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [speed speed-example])))
    (check-equal? 
     (send ori-squaretoy toy-x)
     HALF-WIDTH
     "check squaretoy x position")  
    
    (check-equal? 
     (send ori-squaretoy toy-y)
     HALF-HEIGHT
     "check squaretoy y position")
    
    (check-equal? 
     (send ori-squaretoy toy-color)
     GREEN
     "check squaretoy color"))
  
  (local
    ((define ori-circletoy
       (new CircleToy%
            [t-num 0]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [color GREEN])))
    (check-equal? 
     (send ori-circletoy toy-x)
     HALF-WIDTH
     "check circletoy x position")
    
    (check-equal? 
     (send ori-circletoy toy-y)
     HALF-HEIGHT
     "check circletoy y position")
    
    (check-equal? 
     (send ori-circletoy toy-color)
     GREEN
     "check circletoy color"))
  
  
  (local
    ((define ori-squaretoy
       (new SquareToy%
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [speed speed-example])))
    (check-equal? 
     (send ori-squaretoy add-to-scene EMPTY-CANVAS)
     (place-image
      (rectangle SQUARE-TOY-LENGTH SQUARE-TOY-LENGTH OUTLINE GREEN)
      HALF-WIDTH
      HALF-HEIGHT
      EMPTY-CANVAS)
     "check squaretoy add to scene"))
  
  (local
    ((define ori-circletoy
       (new CircleToy%
            [t-num 0]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [color GREEN])))  
    (check-equal? 
     (send ori-circletoy add-to-scene EMPTY-CANVAS)
     (place-image
      (circle CIRCLE-TOY-RADIUS SOLID GREEN)
      HALF-WIDTH
      HALF-HEIGHT
      EMPTY-CANVAS)
     "check circletoy add to scene"))
  
  
  (local
    ((define ori-squaretoy
       (new SquareToy%
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [speed speed-example])))
    (check toy-equal?
           (make-square-toy HALF-WIDTH HALF-HEIGHT speed-example)
           ori-squaretoy
           "check make-square-toy makes ori-squaretoy"))
  
  (local
    ((define ori-circletoy
       (new CircleToy%
            [t-num 0]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [color GREEN])))  
    (check toy-equal?
           (make-circle-toy HALF-WIDTH HALF-HEIGHT)
           ori-circletoy
           "check make-circle-toy makes ori-circletoy"))
  
  
  
  (local
    ((define ori-squaretoy
       (new SquareToy%
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [speed speed-example]))
     (define ori-squaretoy-1-tick
       (new SquareToy%
            [x (+ HALF-WIDTH speed-example)]
            [y HALF-HEIGHT]
            [speed speed-example])))
    (check-equal? 
     (toys-equal? (list ori-squaretoy) empty) 
     false
     "check if toys are equal")
    (check toy-equal? 
           (send ori-squaretoy on-tick)
           ori-squaretoy-1-tick
           "check squaretoy after a tick"))
  
  (local
    ((define ori-circletoy
       (new CircleToy%
            [t-num 0]
            [x HALF-WIDTH]
            [y HALF-HEIGHT]
            [color GREEN]))
     (define ori-circletoy-1-tick
       (new CircleToy%
            [t-num 1]
            [x HALF-WIDTH]
            [y HALF-HEIGHT])))     
    (check toy-equal? 
           (send ori-circletoy on-tick)
           ori-circletoy-1-tick
           "check circletoy after a tick")))




