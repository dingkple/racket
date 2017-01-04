#lang racket
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)
(require "sets.rkt")    

(provide 
 make-world
 run
 World%
 SquareToy%
 StatefulWorld<%>
 StatefulToy<%>)

;; For when expression: https://piazza.com/class/hv42z9ncl8v1cr?cid=1342

;; A world consists of a canvas of size 400*500 which
;; contains a target and list of toys(initially empty).

;; target is a circle image with radius 10 in outline mode which appears
;; at the centre of the canvas and it's is smoothly draggable

;; Press s for new square-shaped toy to pop up with its center located 
;; at the center of the target. When a square is created, it initially has 
;; no buddies, even if it already overlaps another square. 
;; However, as soon as it or the other square is moves, buddies are 
;; created in the normal way.

;; The square toys no longer move on their own. 
;; Instead, they move by smooth dragging, like the target.
;; Squares may also move if they are buddies with another toy. 
;; Squares become buddies when they overlap while one of those squares 
;; is moving. Once two squares are buddies they stay that way forever. 
;; And any toy in this program's documents refers to a stateful-toy

;; In this program, World is a publisher, it adds some toys as subscribers and
;; publishes on-mosue to these toys, and since we won't need toy itself to let
;; world add it as as subscriber, we do not implement square-toy as a subscriber

;; start with (run framerate).  Ex: (run 0.25)

;;CONSTANTS
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 500)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define HALF-WIDTH 200)
(define HALF-HEIGHT 250)

(define RED "red")
(define GREEN "green")
(define BLACK "black")
(define ORANGE "orange")

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

(define SQUARE-TOY-LENGTH 30)
(define HALF-SQUARE-LEN 15)



;; a ListOfStatefulToy<%> (LOT) is one of:
;;  --empty
;;  --(cons StatefulToy<%> LOT)
;; Interp:
;; a LOT is either empty, or a list of StatefulToy<%>(for StatefulToy<%> 
;; see below)
;; Template:
;; lot-fn: LOT -> ??
;; (define (lot-fn lot)
;;   (cond
;;     [(empty? lot) ...]
;;     [else (...(first lot)
;;               (lot-fn (rest lot)))]))


;; a ColorString is one of:
;;  --GREEN
;;  --BLACK
;;  --ORANGE
;;  --RED
;; Interp:
;; just colors
;; Template:
;; c-fn: ColorString -> ??
;; (define (c-fn c)
;;   (cond
;;     [(string=? c GREEN) ...]
;;     [(string=? c BLACK) ...]
;;     [(string=? c ORANGE) ...]
;;     [(string=? c RED) ...]))



;; Interfaces:
(define StatefulWorld<%>
  (interface () 
    ;; -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this StatefulWorld<%> to the 
    ;;         state that it should be in after a tick.
    on-tick                           
    
    ;; Integer Integer MouseEvent -> Void
    ;; GIVEN: a x-pos, a y-pos mouse positions and a MouseEvent
    ;; EFFECT: updates this StatefulWorld<%> to the 
    ;;         state that it should be in after the given MouseEvent
    on-mouse
    
    ;; KeyEvent -> Void
    ;; GIVEN: a KeyEvent
    ;; EFFECT: updates this StatefulWorld<%> to the 
    ;;         state that it should be in after the given KeyEvent
    on-key
    
    ;; -> Scene
    ;; GIVEN: no arguments
    ;; RETURN: a Scene depicting this StatefulWorld<%> on it.
    on-draw 
    
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURN: the x coordinate of the target
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURN: the y coordinate of the target
    target-x
    target-y
    
    ;; -> Boolean
    ;; GIVEN: no arguments
    ;; RETURN: Is the target selected?
    target-selected?
    
    ;; -> ColorString
    ;; GIVEN: no arguments
    ;; RETURN: color of the target
    target-color
    
    ;; -> ListOfStatefulToy<%>
    ;; GIVEN: no arguments
    ;; RETURN: the toys in the world
    get-toys
    
    ))


(define StatefulToy<%> 
  (interface ()
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURN: a Scene like the given one, but with this  
    ;; StatefulToy<%> drawn on it.
    add-to-scene
    
    ;; Integer Integer StatefulToy<%> Boolean -> Void
    ;; GIVEN: a mouse-position(x-pos and y-pos) and a stateful-toy
    ;; RETURN: void, but update the buddies list depend on whether the given
    ;; toy is a buddy and if checking is needed
    add-buddy
    
    ;; Integer Integer -> Void
    ;; GIVEN: a mouse-position(x-pos and y-pos)
    ;; RETURN: Void, but update the toy's vector fields(the vector between
    ;;  center to the given position, and the boolean field about whether toy
    ;;  is selected)
    set-vector
    
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
    ;; RETURN: returns the current color of this StatefulToy<%>
    toy-color
    
    ;; -> Boolean
    ;; GIVEN: no arguments
    ;; RETURNS: Is this StatefulToy<%> selected?
    toy-selected?
    
    ;; -> LOT
    ;; GIVEN: no arguments
    ;; RETURNS: this toy's buddies 
    toy-buddies
    
    ;; Integer Integer -> Boolean
    ;; GIVEN: a mouse-position(x-pos and y-pos)
    ;; RETURNS: true iff the given mouse-position is in this toy
    in-toy?
    
    ;; Integer Integer MouseEvent-> Void
    ;; GIVEN: the mouse's position(x-pos and y-pos) and a mouse-event
    ;; EFFECT: updates this StatefulToy<%> to the 
    ;;         state that it should be in after the given MouseEvent
    on-mouse
    
    ))


(define Publisher<%>
  (interface ()
    ;; Any<%> -> Void
    ;; GIVEN: any object meets the publisher's subcriber-list needs
    ;; RETURN: void, but add the given object to the publisher's subscirber-list
    subscribe
    
    ))

;; World%     -- a class that satisfies the StatefulWorld<%> interface
;;               and Publisher<%> interface.
;; A World% is a (new World% 
;;                      [t-x Integer] [t-y Integer][vec-x Integer]
;;                      [vec-y Integer][t-selected? Boolean]
;;                      [target-col ColorString][toys LOT])
;; Interpretation: represents a world, containing a
;; target x and y center positions
;; vec-x and vec-y are relative x and y positions from target's 
;; x and y original position to the mouse's current x & y position (used
;; for smooth dragging of target)
;; t-selelcted? indicates target selected status, true for selected
;; toys is the list of toys in this world
(define World%
  (class* object% (StatefulWorld<%> Publisher<%>)
    
    ;;Integer, t-x is the target's center x position
    ;;Integer, t-y is the target's center y position
    (init-field 
     t-x
     t-y)
    
    ;; Integer, vec-x is the relative x position from the target's 
    ;; original position to the mouse's current x position
    ;; Integer, vec-y is the relative y position from the target's 
    ;; original position to the mouse's current y position
    (init-field
     [vec-x 0]
     [vec-y 0])
    
    ;; Boolean, t-selected? is the target's selected? status
    ;; initially false.
    (init-field [t-selected? false])
    
    ;; ColorString, target-col is the target's current color
    ;; initially BLACK.
    (init-field [target-col BLACK])
    
    ;; LOT, toys is the list of toys in the world
    ;; initially empty.
    (init-field [toys empty])  
    
    ;; LOT, selected-toys is the list of toys selected by mouse
    (field [selected-toys empty])
    
    ;; LOT, subscribers, this is the list of toys who are buddies of the toys 
    ;; selected by mouse
    (field [subscribers empty])
    
    (super-new)
    
    ;; publish: Integer Integer MouseEvent -> Void 
    ;; GIVEN: a mouse position and a mouse-event
    ;; EFFECT: call every toy(in subscriber-list)'s on-mouse method, using given
    ;; mouse-position and mouse-event
    ;; EXAMPLES: if subscribers contains toyA and toyB, then calling this method
    ;; will do (send toyA on-mouse mx my evt) and (send toyB mx my evt)
    (define/public (publish mx my evt)
      (for-each
       ;; StatefulToy<%> -> Void
       ;; GIVEN : toy
       ;; RETURN: void, but call the given toy's on-mouse method using the given
       ;; mouse-event and mouse-position
       (lambda (t)
         (send t on-mouse mx my evt))
       subscribers))
    
    ;; subscribe: StatefulToy<%> -> Void
    ;; GIVEN: any object of a class implements the StatefulToy<%> interface
    ;; EFFECT: Update this World's selected-toys (i.e. add the given object
    ;;   to the selected-toys)
    ;; EXAMPLES: (send this subscribe t1) -> add t1 to this world's 
    ;;   selected-toys
    (define/public (subscribe sel-toy)
      (set! selected-toys (cons sel-toy selected-toys)))
    
    ;; on-draw: -> Scene 
    ;; GIVEN: no arguments
    ;; RETURNS: a scene like the given one, but with this world painted
    ;; on it.
    ;; EXAMPLES: Refer tests
    (define/public (on-draw)
      (local
        ((define target-image (circle TARGET-RADIUS OUTLINE target-col)))
        (foldr
         ;; StatefulToy<%> Scene -> Scene
         ;; GIVEN: a toy and a previously formed scene
         ;; RETURNS: the given scene with the given toy drawn on
         (lambda (toy ans)
           (send toy add-to-scene ans))
         (place-image target-image t-x t-y EMPTY-CANVAS)
         toys)))
    
    ;; on-tick: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this World% to the 
    ;;         state that it should be in after a tick.
    ;; EXAMPLES: Refer tests
    (define/public (on-tick)
      this)
    
    ;; on-key: KeyEvent -> Void
    ;; GIVEN: a KeyEvent
    ;; EFFECT: updates this World% to the 
    ;;         state that it should be in after the given KeyEvent
    ;; EXAMPLES: Refer tests
    ;; STRATEGY: Cases on kev: KeyEvent
    (define/public (on-key kev)
      (cond
        [(key=? kev S-KEY) (send this world-after-s-key)]
        [else this]))
    
    ;; world-after-s-key:  -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this world's toys(i.e add a new SquareToy at target's
    ;; center position)
    ;; EXAMPLES: calling this method will add a new SquareToy to the toys
    (define/public (world-after-s-key)
      (set! toys (cons (new SquareToy% [x t-x] [y t-y]) toys)))
    
    ;; on-mouse: Integer Integer MouseEvent -> Void
    ;; GIVEN: mouse x and y positions and a MouseEvent
    ;; EFFECT: updates this World% to the state that it
    ;;         should be in after the given MouseEvent
    ;; EXAMPLES: Refer tests
    ;; STRATEGY: Cases on evt: MouseEvent
    (define/public (on-mouse mx my evt)
      (cond
        [(mouse=? evt BUTTON-DOWN)
         (update-and-publish-down mx my evt)]
        [(mouse=? evt DRAG)
         (begin
           (when (not (empty? selected-toys))
             (send this update-all-toy-buddies mx my))
           (update-and-publish-drag mx my evt)
           (when (not (empty? selected-toys))
             (send this update-all-toy-buddies mx my)))]
        [(mouse=? evt BUTTON-UP)
         (update-and-publish-up mx my evt)]
        [else void]))
    
    
    ;; update-and-publish-down: Integer Integer MouseEvent -> Void
    ;; GIVEN: mouse x and y positions and a MouseEvent
    ;; EFFECT: update this world's selected-toys, subscribers, 
    ;;   target, and send mouse button-down event to proper toys
    ;; EXAMPLES: 
    ;;   Consider the previous state of this world object as target
    ;;   is in t-x=200 and t-y=250 and t-selected?=false and selected-toys=
    ;;   empty and subcribers=empty and target-col=BLACK 
    ;;   and vec-x=0 ,vec-y=0 and toys contain a list of 3 unselected toys
    ;;   toyA centered at 200,250 and toyB and toyC at other non 
    ;;   overlapping positons. toyA has toyC and toyB as buddies.
    ;;   On calling this method with mx = 201 and my = 251 and 
    ;;   evt=BUTTON-DOWN, this world object is now left with t-x = 201
    ;;   and t-y = 251, t-selected? = true , vec-x = 1, vec-y = 1
    ;;   and target-col changed to ORANGE and selected-toys contains
    ;;   toyA in the list and subscribers contain toyA, toyB and toyC
    ;;   in the list and and toys is updated to reflect color of toyA is 
    ;;   changed to RED along with its vec-x and vec-y now containing 1.
    (define/public (update-and-publish-down mx my evt)
      (begin
        (send this target-after-button-down mx my)
        (send this find-world-selected-toys mx my)
        (send this find-all-subscribers)
        (send this publish mx my evt)))
    
    ;; update-publish-drag: Integer Integer MouseEvent -> Void
    ;; GIVEN: mouse x and y positions and a MouseEvent
    ;; EFFECT: update this world's selected-toys, subscribers, 
    ;;   target, and send mouse drag event to proper toys
    ;; EXAMPLES: 
    ;;   Consider the previous state of this world object as target
    ;;   is in t-x=50 and t-y=75 and t-selected?=false and selected-toys=
    ;;   {toyA} and subcribers={toyA, toyB, toyC} and target-col=BLACK 
    ;;   and vec-x=0 ,vec-y=0 and toys contain a list of 3 toys
    ;;   toyA centered at 200,250 and toyB centered at 300,350 and 
    ;;   toyC centered at 100,150. toyA has toyC and toyB as buddies.
    ;;   On calling this method with mx = 205 and my = 255 and 
    ;;   evt=DRAG, this world object selected-toys contains
    ;;   toyA in the list and subscribers contain toyA, toyB and toyC
    ;;   in the list and toys is updated such that the color of toyA , 
    ;;   toyB and toyC is changed to RED and toyA, toyB and toyC moved right
    ;;   and down by 5 pixels and no other fields are changed. 
    (define/public (update-and-publish-drag mx my evt)
      (begin
        (send this target-after-drag mx my)
        (send this find-all-subscribers)
        (send this publish mx my evt)))
    
    ;; update-and-publish-up: Integer Integer MouseEvent -> Void
    ;; GIVEN: mouse x and y positions and a MouseEvent
    ;; EFFECT: update this world's selected-toys, subscribers, 
    ;;   target, and send mouse button-up event to proper toys
    ;; EXAMPLES: 
    ;;   Consider the previous state of this world object as target
    ;;   is in t-x=200 and t-y=250 and t-selected?=false and selected-toys=
    ;;   empty and subcribers=empty and target-col=BLACK 
    ;;   and vec-x=0 ,vec-y=0 and toys contain a list of 3 unselected toys
    ;;   toyA centered at 200,250 and toyB and toyC at other non 
    ;;   overlapping positons. toyA has toyC and toyB as buddies.
    ;;   On calling this method with mx = 200 and my = 250 and 
    ;;   evt=BUTTON-UP, this world object is now left with t-x = 200
    ;;   and t-y = 250, t-selected? = false , vec-x = 0, vec-y = 0
    ;;   and target-col changed to BLACK and selected-toys is set to
    ;;   empty in the list and subscribers set to empty and toys is
    ;;   updated such that Color of all toys is changed to GREEN.
    (define/public (update-and-publish-up mx my evt)
      (begin
        (send this target-after-button-up mx my)
        (send this publish mx my evt)
        (set! selected-toys empty)
        (set! subscribers empty)))
    
    ;; find-all-subscribers:  -> Void
    ;; GIVEN: no arguments
    ;; RETURNS: Void, but find all the buddies of the toys selected by mouse,
    ;; and change this world's subscribers to the result
    ;; EXAMPLES: On calling this method, if toy A is in selected-toys list
    ;;   and toy A has toy B and toy C as buddies then subscribers field of  
    ;;   this world object contains a list of toy A toy B and toy C 
    ;;   and no other fields are changed. 
    (define/public (find-all-subscribers)
      (local 
        ((define all-buddies
           (foldr
            ;;StatefulToy<%> LOT -> LOT
            ;;GIVEN : a stateful-toy and a list of stateful-toys
            ;;RETURN : all the buddies of the toys selected by mouse
            (lambda (t ans)
              (set-union ans (send t toy-buddies)))
            empty
            selected-toys)))
        (set! subscribers (set-union all-buddies selected-toys))))
    
    ;; find-toy-buddies: Integer Integer StatefulToy<%> -> Void
    ;; GIVEN: a mouse position(x and y) and a toy as toy1
    ;; RETURNS: Void, but update the buddies of toy1
    ;; EXAMPLES: refer tests
    (define/public (find-toy-buddies mx my toy)
      (for-each
       ;;StatefulToy<%> -> Void
       ;;GIVEN : a stateful-toy as toy2
       ;;RETURN : void, but call toy1's add-buddy method, let toy1 decide 
       ;; whether it should add toy2 as a buddy 
       (lambda (t)
         (when (not (equal? t toy))
           (send toy add-buddy mx my t true)))
       toys))
    
    ;; find-world-selected-toys: Integer Integer -> Void
    ;; GIVEN: a mouse position(x-pos and y-pos)
    ;; RETURNS: Void, but update the selected-toys of this world(the toys 
    ;;  selected by the mouse)
    ;; EXAMPLES: refer tests
    (define/public (find-world-selected-toys mx my)
      (for-each
       ;;StatefulToy<%> -> void
       ;;GIVEN : a stateful-toy
       ;;RETURN : void, but update this world's selected-toy list 
       (lambda (t)
         (when (send t in-toy? mx my)
           (send this subscribe t)))
       toys))
    
    ;; update-all-toy-buddies: Integer Integer -> Void
    ;; GIVEN: a mouse position(x-pos and y-pos)
    ;; RETURNS: void, but update all toy's buddy-list
    ;; EXAMPLES: On calling this method with mx = 205 and my = 105, 
    ;;   update the buddies of the all toys in subscribers of this 
    ;;   world object no other fields are changed. 
    (define/public (update-all-toy-buddies mx my)
      (for-each
       ;;StatefulToy<%> -> Void
       ;;GIVEN : a stateful toy
       ;;RETURN : void, but update given toy's buddies
       (lambda (t)
         (send this find-toy-buddies mx my t))
       subscribers))
    
    
    ;; target-after-button-down: Integer Integer -> Void
    ;; GIVEN: mx and my are mouse positions
    ;; RETURNS: Void, but update the vec-x and vec-y and t-selected?
    ;;          and target-col
    ;; EXAMPLES: if the t-x field contains 100 and the t-y field contains
    ;;   200, mx = 205 and my = 105, this world object is left with vec-x = 5 
    ;;   and vec-y = 5, and t-selected? is set to true and target-col changed 
    ;;   to ORANGE and no other fields are changed.
    (define/public (target-after-button-down mx my)
      (when (in-target? mx my)
        (begin
          (set! vec-x (- t-x mx))
          (set! vec-y (- t-y my))
          (set! t-selected? true)
          (set! target-col ORANGE))))
    
    ;; target-after-button-up: Integer Integer -> Void
    ;; GIVEN: mx and my are mouse x and y positions
    ;; RETURNS: update this world's target's position, and vec-x, vec-y,
    ;;   t-selected? and target-col
    ;; EXAMPLES: On calling this method,if the vec-x field contains 5 
    ;;   and the vec-y field contains 5, mx = 205 and my = 105, this world
    ;;   object is left with t-x = 210 and t-y = 110, and t-selected? is 
    ;;   set to false and target-col changed to BLACK and no other 
    ;;   fields are changed.
    (define/public (target-after-button-up mx my)
      (when (and (in-target? mx my) t-selected?)      
        (begin
          (set! t-x (+ mx vec-x))
          (set! t-y (+ my vec-y))
          (set! vec-x 0)
          (set! vec-y 0)
          (set! t-selected? false)
          (set! target-col BLACK))))
    
    ;; target-after-drag: Integer Integer -> Void
    ;; GIVEN: mx and my are mouse x and y positions
    ;; RETURN: Void, but udpate the target's position to given position
    ;; EXAMPLES: On calling this method,if the vec-x field contains 5 
    ;;   and the vec-y field contains 5, mx = 205 and my = 105, this world
    ;;   object is left with t-x = 210 and t-y = 110 and target-col changed
    ;;   to ORANGE and no other fields are changed. 
    (define/public (target-after-drag mx my)
      (when t-selected?
        (begin
          (set! t-x (+ mx vec-x))
          (set! t-y (+ my vec-y))
          (set! target-col ORANGE))))
    
    
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
    
    ;;target-color: -> ColorString
    ;; GIVEN: no arguments
    ;; RETURNS: color of the target
    ;; EXAMPLES: (send ori-world target-color) -> BLACK
    (define/public (target-color)
      target-col)      
    
    ;; get-toys:  -> LOT
    ;; GIVEN: no arguments
    ;; RETURNS: all the toys in this world
    ;; EXAMPLES: (send ori-world get-toys) -> empty
    (define/public (get-toys)
      toys)
    
    ))

;; SquareToy% -- a class satisfies the StatefulToy<%> interface
;; A SquareToy% is a (new SquareToy% [x Integer] [y Integer]
;;       [vec-x Integer][vec-y Integer][t-selected? Boolean])
;; Interpretation: represents a squaretoy, containing a 
;; x and y center positions of a squaretoy
;; vec-x and vec-y record the vector between given position to the 
;; center of the toy(used for smooth draggring of a toy)
;; t-selected? indicates whether the toy is selected
(define SquareToy%
  (class* object% (StatefulToy<%>)
    ;;Integer, x is the center x position of this squaretoy
    ;;Integer, y is the center y position of this squaretoy
    (init-field 
     x
     y)
    
    ;; both vec-x and vec-y are integers, (vec-x, vec-y) is the vector from
    ;; current mouse position to the current center of the toy
    (init-field
     [vec-x 0]
     [vec-y 0])
    
    ;; Boolean, t-selected? indicates whether the toy is selected
    (init-field
     [t-selected? false])
    
    ;;ColorString, color indicates the square toy's color 
    ;;initially set to green
    (field [color GREEN])
    
    ;; LOT, buddies is the list of toys who are buddies of this toy
    ;;initially empty
    (field [buddies empty])
    
    
    (super-new)
    
    ;; toy-buddies:  -> LOT
    ;; GIVEN: no arguments
    ;; RETURNS: the buddies of this toy
    ;; EXAMPLES: (send this toy-buddies) -> empty
    (define/public (toy-buddies)
      buddies)  
    
    ;; add-buddy: Integer Integer StatefulToy<%> Boolean -> Void
    ;; GIVEN: a mouse position(x-pos and y-pos), a stateful toy and a boolean
    ;;  check indicates whether we need to check if we should add the given
    ;;  toy as a buddy, when check? is false, we add it directly, since we've
    ;;  already checked the relationship somewhere else
    ;; RETURNS: void, if the given toy is a buddy of this toy and have not been
    ;; added to subscribers, add it to subscribers add let the given toy also
    ;; add this toy as s buddy, meanwhile, update the given toy's vec-x and
    ;; vec-y to prepare being dragged, else just add the given toy to 
    ;; subscriber-list
    ;; EXAMPLES: On calling this method with mx = 205 ,my = 255, 
    ;;   t=toyB(centered at 210,260) and check?=true and buddies is empty,
    ;;   this toy object(consider it as toyA is centered at 200, 250)
    ;;   is left with toyB becoming toyA's buddy and toyB's vec-x = 5 and
    ;;   vec-y = 5 and t-selected?=true and buddies list of toyB={toyA} 
    ;;   and also buddies list of toyA = {toyB}   
    (define/public (add-buddy mx my t check?)
      (if check? 
          (when (and (send this is-buddy? 
                           (send t toy-x)
                           (send t toy-y))
                     (not (my-member? t buddies)))
            (begin
              (send t set-vector mx my)
              (send t add-buddy mx my this false)
              (set! buddies (cons t buddies))))
          (set! buddies (set-cons t buddies))))
    
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
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: the given scene with this toy drawn on it
    ;; EXAMPLES: Refer tests
    (define/public (add-to-scene scene)
      (place-image 
       (rectangle SQUARE-TOY-LENGTH SQUARE-TOY-LENGTH OUTLINE color)
       x y scene))
    
    ;; set-vector: Integer Integer -> Void
    ;; GIVEN: a mouse position(x-pos and y-pos)
    ;; RETURNS: void, but set the this toy's vec-x and vec-y
    ;; EXAMPLES: (send this set-vector HALF-WIDTH HALF-HEIGHT) -> this
    (define/public (set-vector mx my)
      (begin
        (set! vec-x (- x mx))
        (set! vec-y (- y my))
        (set! t-selected? true)))
    
    ;; on-mouse: Integer Integer MouseEvent -> void
    ;; GIVEN: a mouse-position(x-pos and y-pos) and a mouse-event
    ;; RETURNS: void, but update this toys based on the given infomation
    ;; EXAMPLES: see tests
    ;; STRATEGY: Cases on evt: MouseEvent
    (define/public (on-mouse mx my evt)
      (begin
        (cond
          [(mouse=? evt BUTTON-DOWN)
           (toy-after-button-down mx my)]
          [(mouse=? evt DRAG)
           (toy-after-drag mx my)]
          [(mouse=? evt BUTTON-UP)
           (toy-after-button-up mx my)]
          [else void])))
    
    ;; toy-after-button-down: Integer Integer -> Void
    ;; GIVEN: a mouse-position(x-pos and y-pos)
    ;; RETURNS: void, but update vec-x vec-y color t-selected
    ;; EXAMPLES: before button-down, if fields are: [vec-x 0] [vec-y 0] 
    ;; [color GREEN] [t-selected? false] [x 100] [y 100], and mx=105 my=95, 
    ;; after calling this method: [x 100] [y 100] [vec-x -5] [vec-y 5]
    ;; [color RED] [t-selected? true], nothing else is changed
    (define/public (toy-after-button-down mx my)
      (begin
        (set! vec-x (- x mx))
        (set! vec-y (- y my))
        (set! color RED)
        (set! t-selected? true)))
    
    ;; toy-after-drag: Integer Integer -> Void
    ;; GIVEN: a mouse-position(x-pos and y-pos)
    ;; RETURNS: void, but update vec-x vec-y and color
    ;; EXAMPLES: before button-down, if fields are: 
    ;; [color GREEN] [x 100] [y 100], and mx=115 my=95, 
    ;; after calling this method: [x 110] [y 100] 
    ;; [color RED], nothing else is changed
    (define/public (toy-after-drag mx my)
      (when t-selected?
        (begin
          (set! x (+ mx vec-x))
          (set! y (+ my vec-y))
          (set! color RED))))
    
    ;; toy-after-button-up: Integer Integer -> Void
    ;; GIVEN: a mouse-position(x-pos and y-pos)
    ;; RETURNS: void, but update x y vec-x vec-y color t-selected?
    ;; EXAMPLES: before button-down, if fields are: [vec-x -5] [vec-y 5] 
    ;; [color RED] [t-selected? true] [x 100] [y 100], and mx=105 my=95, 
    ;; after calling this method: [x 100] [y 100] [vec-x 0] [vec-y 0]
    ;; [color GREEN] [t-selected? false], nothing else is changed    
    (define/public (toy-after-button-up mx my)
      (when t-selected?
        (begin
          (set! x (+ mx vec-x))
          (set! y (+ my vec-y))
          (set! vec-x 0)
          (set! vec-y 0)
          (set! color GREEN)
          (set! t-selected? false))))
    
    ;; is-buddy?: Integer Integer -> Boolean
    ;; GIVEN: a mouse position(x-pos and y-pos)
    ;; RETURNS: true iff the toy centered at the given position is a buddy of
    ;;   this toy
    ;; EXAMPLES: (send this is-buddy? x y) -> true
    ;;           (send this is-buddy? (+ x 35) y) -> false
    (define/public (is-buddy? mx my)
      (and (<= (- x SQUARE-TOY-LENGTH) mx (+ x SQUARE-TOY-LENGTH))
           (<= (- y SQUARE-TOY-LENGTH) my (+ y SQUARE-TOY-LENGTH))))
    
    ;; in-toy?: Integer Integer -> Boolean
    ;; GIVEN: a mouse position(x-pos and y-pos)
    ;; RETURNS: true iff the given position is in this toy
    ;; EXAMPLES: (send this in-toy? x y) -> true
    (define/public (in-toy? mx my)
      (and (<= (- x HALF-SQUARE-LEN) mx (+ x HALF-SQUARE-LEN))
           (<= (- y HALF-SQUARE-LEN) my (+ y HALF-SQUARE-LEN))))
    
    ;; toy-selected?:  -> Boolean
    ;; GIVEN: no arguments
    ;; RETURNS: true iff this toy is selected, selected means this toy is either
    ;; selected by mouse or it's a budddy of a toy selected by mouse
    ;; EXAMPLES: (send this toy-selected?) -> true
    (define/public (toy-selected?)
      t-selected?)
    
    ))

;; make-world : -> World%
;; GIVEN: no arguments
;; RETURNS: A World% with no toys and target initially at center of the canvas
;; EXAMPLES: Refer tests
(define (make-world)
  (new World%
       [t-x HALF-WIDTH]
       [t-y HALF-HEIGHT]))

;; run : PosNum -> World%
;; GIVEN: a frame rate (in seconds/tick)
;; EFFECT: creates and runs a world that runs at the given rate.
;; RETURNS: the final world.
(define (run rate)
  (big-bang (make-world)
            (on-tick
             ;; World% -> World%
             ;; GIVEN: a world
             ;; RETURNS: the given world, update this world to the state of 
             ;; next tick
             (lambda (w) (send w on-tick) w)
             rate)
            (on-draw
             ;; World% -> World%
             ;; GIVEN: a world
             ;; RETURNS: a scene that describes the world
             (lambda (w) (send w on-draw)))
            (on-key
             ;; World% KeyEvent -> World%
             ;; GIVEN: a world
             ;; RETURNS: the given world, update this world to the state 
             ;; after the given key-event
             (lambda (w kev) (send w on-key kev) w))
            (on-mouse
             ;; World% Integer Integer MouseEvent -> World%
             ;; GIVEN: a world, a x and y mouse posiitions and a MouseEvent
             ;; RETURNS: the given world, butupdate this world to the state 
             ;; after the given mouse-event
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
   (equal? (send t1 toy-color) (send t2 toy-color))
   (equal? (send t1 toy-selected?) (send t2 toy-selected?))))


;; toys-equal?: LOT LOT -> Boolean
;; GIVEN: two lists of toys
;; RETURNS: true iff the given two list of toys are the same
;; EXAMPLES: (toys-equal? (list ori-squaretoy) empty) -> false
;; STRATEGY: structure decomposition on tl1 and tl2(both LOT)
(define (toys-equal? tl1 tl2)
  (cond
    [(and (empty? tl1) (empty? tl2)) true]
    [else (and (square-toy-equal? (first tl1) (first tl2))
               (toys-equal? (rest tl2) (rest tl2)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES for tests:

(define ori-world 
  (new World%
       [t-x HALF-WIDTH]
       [t-y HALF-HEIGHT]))


(define ori-world-button-down 
  (new World%
       [t-x HALF-WIDTH]
       [t-y HALF-HEIGHT]
       [vec-x -5]
       [vec-y 0]
       [t-selected? true])) 


(define ori-world-drag 
  (new World%
       [t-x (+ HALF-WIDTH 3)]
       [t-y HALF-HEIGHT]
       [vec-x -5]
       [vec-y 0]
       [t-selected? true]))

(define ori-world-button-up
  (new World%
       [t-x (+ HALF-WIDTH 3)]
       [t-y HALF-HEIGHT]))

(define ori-squaretoy
  (new SquareToy%
       [x HALF-WIDTH]
       [y HALF-HEIGHT]))

(define ori-world-s-key
  (new World%
       [t-x HALF-WIDTH]
       [t-y HALF-HEIGHT]
       [toys (list ori-squaretoy)]))

(begin-for-test
  (local
    ((define ori-world (make-world)))
    (check world-equal? 
           ori-world 
           (new World% [t-x HALF-WIDTH] [t-y HALF-HEIGHT])
           "make-world should create a new world"))
  
  (local
    ((define ori-world (make-world))
     (define ori-world-button-down
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [t-selected? true])))
    (send ori-world on-mouse HALF-WIDTH HALF-HEIGHT BUTTON-DOWN)
    (check world-equal?
           ori-world 
           ori-world-button-down 
           "BUTTON-DOWN should selected the target"))
  
  (local
    ((define ori-world (make-world))
     (define ori-world-drag
       (new World%
            [t-x (+ 5 HALF-WIDTH)]
            [t-y HALF-HEIGHT]
            [t-selected? true])))
    (send ori-world on-mouse HALF-WIDTH HALF-HEIGHT BUTTON-DOWN)
    (send ori-world on-mouse (+ 5 HALF-WIDTH) HALF-HEIGHT DRAG)
    (check world-equal? ori-world ori-world-drag
           "drag the selected target should change its position"))
  
  (local
    ((define ori-world (make-world))
     (define ori-world-button-up
       (new World%
            [t-x (+ 5 HALF-WIDTH)]
            [t-y HALF-HEIGHT])))
    (send ori-world on-mouse HALF-WIDTH HALF-HEIGHT BUTTON-DOWN)
    (send ori-world on-mouse (+ 5 HALF-WIDTH) HALF-HEIGHT DRAG)
    (send ori-world on-mouse (+ 5 HALF-WIDTH) HALF-HEIGHT BUTTON-UP)
    (check world-equal? ori-world ori-world-button-up
           "BUTTON-UP should unselect the target"))
  
  (local
    ((define ori-world (make-world))
     (define toy-ex-1
       (new SquareToy% [x HALF-WIDTH] [y HALF-HEIGHT]))
     (define toy-ex-2
       (new SquareToy% [x (+ 10 HALF-WIDTH)] [y HALF-HEIGHT]))
     (define ori-world-s-key-1
       (new World%
            [t-x HALF-WIDTH]
            [t-y HALF-HEIGHT]
            [t-selected? true]
            [toys (list toy-ex-1)]))
     (define ori-world-s-key-2
       (new World%
            [t-x (+ 10 HALF-WIDTH)]
            [t-y HALF-HEIGHT]
            [toys (list toy-ex-2 toy-ex-1)])))
    (check-equal? (send ori-world target-color) BLACK "should be black")
    (send ori-world on-mouse HALF-WIDTH HALF-HEIGHT BUTTON-DOWN)
    (check-equal? (send ori-world target-color) ORANGE "should be orange now")
    (check-equal?
     (send ori-world on-draw)
     (place-image
      (circle TARGET-RADIUS OUTLINE ORANGE)
      HALF-WIDTH
      HALF-HEIGHT
      EMPTY-CANVAS)
     "target should be orange at center of the canvas")
    (send ori-world on-key S-KEY)
    (check world-equal? ori-world ori-world-s-key-1
           "should add a toy at center of the canvas")
    (send ori-world on-mouse (+ 10 HALF-WIDTH) HALF-HEIGHT DRAG)
    (send ori-world on-mouse (+ 10 HALF-WIDTH) HALF-HEIGHT BUTTON-UP)
    (send ori-world on-key S-KEY)
    (check world-equal? ori-world ori-world-s-key-2
           "should add a new toy at (210 250")
    (send toy-ex-2
          on-mouse (+ 21 HALF-WIDTH) HALF-HEIGHT BUTTON-DOWN)
    (check-equal? (send toy-ex-2 toy-x) (+ 10 HALF-WIDTH)
                  "x-pos of the toy-ex-2 should be 210")
    (check-equal? (send toy-ex-2 toy-y) HALF-HEIGHT
                  "y-pos of the toy-ex-2 should be 250")
    (check-equal? (send toy-ex-2 toy-color) RED
                  "since toy-ex-2 is selected, it should be red")
    (check-equal? (send ori-world target-x) (+ 10 HALF-WIDTH)
                  "target-x should be 210")
    (check-equal? (send ori-world target-y) HALF-HEIGHT
                  "target-y should be 250")
    (check-equal? 
     (send ori-world target-selected?)
     false
     "target is not selected, t-selected should be false")
    (send ori-world on-mouse (+ 21 HALF-WIDTH) HALF-HEIGHT BUTTON-DOWN)
    (check-equal?
     (map
      ;;StatefulToy<%> -> Integer
      ;;GIVEN : a stateful-toy
      ;;RETURN: the toy's x-pos
      (lambda (t)
        (send t toy-x))
      (send ori-world get-toys))
     '(210 200)
     "the two toys should stay original position")
    (check-equal?
     (map
      ;;StatefulToy<%> -> Boolean
      ;;GIVEN : a stateful-toy
      ;;RETURN: the toy's t-selecte? field
      (lambda (t)
        (send t toy-selected?))
      (send ori-world get-toys))
     '(#t #f)
     "toy-ex-2 is selected and toy-ex-1 is not")
    (check-equal?
     (map
      ;;StatefulToy<%> -> Integer
      ;;GIVEN : a stateful-toy
      ;;RETURN: the toy's x-pos
      (lambda (t)
        (send t toy-x))
      (send ori-world-s-key-2 get-toys))
     '(210 200)
     "two toys of in ori-world-s-key-2 is not moved")
    (check world-equal? ori-world ori-world-s-key-2)
    (send ori-world on-mouse (+ 21 HALF-WIDTH) HALF-HEIGHT DRAG)
    (send ori-world on-mouse (+ 26 HALF-WIDTH) HALF-HEIGHT DRAG)
    (check-equal?
     (map
      ;;StatefulToy<%> -> Integer
      ;;GIVEN : a stateful-toy
      ;;RETURN: the toy's x-pos
      (lambda (t)
        (send t toy-x))
      (send ori-world get-toys))
     '(215 205)
     "the two toys should stay original position")
    (check-equal?
     (map
      ;;StatefulToy<%> -> Integer
      ;;GIVEN : a stateful-toy
      ;;RETURN: the toy's t-selecte? field
      (lambda (t)
        (send t toy-selected?))
      (send ori-world get-toys))
     '(#t #t))
    (send ori-world on-mouse (+ 26 HALF-WIDTH) HALF-HEIGHT BUTTON-UP)
    (send toy-ex-2 on-mouse (+ 26 HALF-WIDTH) HALF-HEIGHT DRAG)
    (send toy-ex-2 on-mouse (+ 26 HALF-WIDTH) HALF-HEIGHT BUTTON-UP)
    (send toy-ex-1 on-mouse (+ 21 HALF-WIDTH) HALF-HEIGHT BUTTON-DOWN)
    (send toy-ex-1 on-mouse (+ 26 HALF-WIDTH) HALF-HEIGHT DRAG)
    (send toy-ex-1 on-mouse (+ 26 HALF-WIDTH) HALF-HEIGHT BUTTON-UP)
    (send toy-ex-1 on-mouse HALF-WIDTH HALF-HEIGHT LEAVE)
    (send ori-world on-mouse HALF-WIDTH HALF-HEIGHT LEAVE)
    (send ori-world on-tick)
    (check-equal? 
     (send ori-world on-key C-KEY)
     ori-world
     "any key-evt other than S has no effects")
    (check-equal?
     (map
      ;;StatefulToy<%> -> Integer
      ;;GIVEN : a stateful-toy
      ;;RETURN: the toy's x-pos
      (lambda (t)
        (send t toy-x))
      (send ori-world get-toys))
     '(215 205)
     "drag shoud move both of the two toys, since they're buddies")
    (check-equal?
     (map
      ;;StatefulToy<%> -> Integer
      ;;GIVEN : a stateful-toy
      ;;RETURN: the toy's t-selecte? field
      (lambda (t)
        (send t toy-selected?))
      (send ori-world get-toys))
     '(#f #f)
     "both toys should be unselected")
    (check-equal?
     (map
      ;;StatefulToy<%> -> Integer
      ;;GIVEN : a stateful-toy
      ;;RETURN: the toy's x-pos
      (lambda (t)
        (send t toy-x))
      (send ori-world-s-key-2 get-toys))
     '(215 205)
     "both toys in ori-world-s-key-2 is moved")
    (check world-equal? ori-world ori-world-s-key-2
           "two world should be equal")
    (check-equal?
     (send ori-world on-draw)
     (place-image
      (rectangle SQUARE-TOY-LENGTH SQUARE-TOY-LENGTH OUTLINE GREEN)
      215
      250
      (place-image
       (rectangle SQUARE-TOY-LENGTH SQUARE-TOY-LENGTH OUTLINE GREEN)
       205
       250
       (place-image
        (circle TARGET-RADIUS OUTLINE BLACK)
        210
        250
        EMPTY-CANVAS)))
     "world should be the same with the image generated by place-image")
    ))


;; (run .25)




















