#lang racket

(require "pdpunit-scribble.rkt")
(require "two-bouncing-cats.rkt")
(require 2htdp/image)

(provide tests02 run-pdp-tests)

(define-pdp-test-suite 
  tests02 "02" "This test suite tests your implementation of Problem Set 02"
  (test-file
   "two-bouncing-cats.rkt" "Tests your implementation of Bouncing cats" 
   ([CANVAS-WIDTH 450]
    [CANVAS-HEIGHT 400]
    [CANVAS-HALF-WIDTH (/ CANVAS-WIDTH 2)]
    [CANVAS-HALF-HEIGHT (/ CANVAS-HEIGHT 2)]
    [CAT-IMAGE (bitmap "cat.png")]
    [HALF-CAT-WIDTH  (/ (image-width  CAT-IMAGE) 2)]
    [HALF-CAT-HEIGHT (/ (image-height CAT-IMAGE) 2)]
    [INITIAL-Y-POS 100]
    [CAT1-X-COORD (/ CANVAS-WIDTH 3)]
    [CAT2-X-COORD (* 2 CAT1-X-COORD)]
    [INITIAL_WORLD (initial-world INITIAL-Y-POS)]
    [RBORDER (- CANVAS-WIDTH HALF-CAT-WIDTH)]
    [LBORDER HALF-CAT-WIDTH]
    [BBORDER (- CANVAS-HEIGHT HALF-CAT-HEIGHT)]
    [TBORDER HALF-CAT-HEIGHT]
    [cat-1 (lambda (world) (world-cat1 world))]
    [cat-2 (lambda (world) (world-cat2 world))]
    [cat1-posn (lambda (world) (list (cat-x-pos (cat-1 world)) (cat-y-pos (cat-1 world))))]
    [cat2-posn (lambda (world) (list (cat-x-pos (cat-2 world)) (cat-y-pos (cat-2 world))))]
    [get-cat-direction (lambda (cat) (cond 
                             [(cat-north? cat) "north"]
                             [(cat-south? cat) "south"]
                             [(cat-east? cat) "east"]
                             [(cat-west? cat) "west"]))]
    [get-cats-direction (lambda (world) (list (get-cat-direction (cat-1 world))
                                    (get-cat-direction (cat-2 world))))]
    [get-cat (lambda (cat w) (list (cat-x-pos (cat w)) (cat-y-pos (cat w)) 
                                     (cat-selected? (cat w)) (get-cat-direction (cat w))))]
    [cat-speed (- (cat-y-pos (cat-1 (world-after-tick INITIAL_WORLD)))  (cat-y-pos (cat-1 INITIAL_WORLD)))]
                 
    [get-cats (lambda (w) `(,(get-cat cat-1 w) ,(get-cat cat-2 w)))]
    [simulate-until-at-wall (lambda (cat w)
                              (let ([cat-towards-east? (cat-east? (cat w))]
                                    [cat-towards-west? (cat-west? (cat w))]
                                    [cat-towards-north? (cat-north? (cat w))]
                                    [cat-towards-south? (cat-south? (cat w))]
                                    [cat-curr-x (cat-x-pos (cat w))]
                                    [cat-curr-y (cat-y-pos (cat w))])                                
                                (cond 
                                  [(or (and cat-towards-east? (> cat-curr-x RBORDER))
                                       (and cat-towards-west? (< cat-curr-x LBORDER))
                                       (and cat-towards-south? (> cat-curr-y BBORDER))
                                       (and cat-towards-north? (< cat-curr-y TBORDER)))
                                   (error "Moved past the edge")]
                                  [(and cat-towards-west? (or (= cat-curr-x RBORDER) (= cat-curr-x (- RBORDER 1))))
                                   w]
                                  [(and cat-towards-east? (or (= cat-curr-x (+ LBORDER 1)) (= cat-curr-x LBORDER)))
                                   w]
                                  [(and cat-towards-north? (or (= cat-curr-y BBORDER) (= cat-curr-y (- BBORDER 1))))
                                   w]
                                  [(and cat-towards-south? (or (= cat-curr-y (+ TBORDER 1)) (= cat-curr-y TBORDER)))
                                   w]
                                  [else 
                                   (begin 
                                     (let* ([after-tick (world-after-tick w)]
                                            [cat-next-x (cat-x-pos (cat after-tick))]
                                            [cat-next-y (cat-y-pos (cat after-tick))])
                                       (if (or (and cat-towards-east? (> cat-next-x cat-curr-x))
                                               (and cat-towards-west? (< cat-next-x cat-curr-x))
                                               (and cat-towards-south? (> cat-next-y cat-curr-y))
                                               (and cat-towards-north? (< cat-next-y cat-curr-y)))
                                           (if (or (equal? (abs (- cat-next-x cat-curr-x)) cat-speed)
                                                   (equal? (abs (- cat-next-y cat-curr-y)) cat-speed)
                                                   (= cat-next-x RBORDER)
                                                   (= cat-next-x (- RBORDER 1))
                                                   (= cat-next-x (- RBORDER 2))
                                                   (= cat-next-x LBORDER)
                                                   (= cat-next-x (+ LBORDER 1))
                                                   (= cat-next-x (+ LBORDER 2))
                                                   (= cat-next-y BBORDER)
                                                   (= cat-next-y (- BBORDER 1))
                                                   (= cat-next-y (- BBORDER 2))
                                                   (= cat-next-y TBORDER)
                                                   (= cat-next-y (+ TBORDER 1))
                                                   (= cat-next-y (+ TBORDER 2)))
                                               (begin (simulate-until-at-wall cat after-tick))
                                               (error "Does not move at full speed when it should"))
                                           (error "Does not move towards correct wall"))))])))]
    [world-after-kev (lambda (world kev) (world-after-key-event world kev))]
    [world-after-mev (lambda (world x y mev) (world-after-mouse-event world x y mev))]
    [CX 300]
    [CY 300]
    [change-dir (lambda (world cat mx my key)
                  (let* ([select-cat (world-after-mev world (+ (cat-x-pos (cat INITIAL_WORLD)) 10)
                                                       (+ (cat-y-pos (cat INITIAL_WORLD)) 10)
                                                       "button-down")]
                         [drag-cat (world-after-mev select-cat mx
                                                    my
                                                    "drag")]
                         [cat-after-key (world-after-kev drag-cat key)]
                         [release-cat (world-after-mev cat-after-key mx
                                                        my
                                                        "button-up")])
                    release-cat))])
   
   (test-group-with-points
    "Basic Cats behavior" "" 2
    ([INITWORLD-AFTER-A-TICK (world-after-tick INITIAL_WORLD)])
    
        (test-equal? (get-cats INITIAL_WORLD)
                 `(,`(,CAT1-X-COORD ,INITIAL-Y-POS ,false ,"south") 
                   ,`(,CAT2-X-COORD ,INITIAL-Y-POS ,false ,"south"))
                 1/2
                 "The cats should be created in given initial y position")
        
    (test-equal? (get-cats INITWORLD-AFTER-A-TICK)
                 `(,`(,CAT1-X-COORD ,(+ INITIAL-Y-POS cat-speed) ,false ,"south") 
                   ,`(,CAT2-X-COORD ,(+ INITIAL-Y-POS cat-speed) ,false ,"south"))
                 1/2
                 "The cats should fall at specified speed")
    (test-or
     1
     "The cat should bounce, once it reaches the bottom"
     (test-equal? (get-cat cat-1 (simulate-until-at-wall cat-1 INITWORLD-AFTER-A-TICK))
                 `(,CAT1-X-COORD ,BBORDER  ,false ,"north")
                 1
                 "The cat should bounce, once it reaches the bottom no threshold")
     (test-equal? (get-cat cat-1 (simulate-until-at-wall cat-1 INITWORLD-AFTER-A-TICK))
                 `(,CAT1-X-COORD ,(- BBORDER 2) ,false ,"north")
                 1
                 "The cat should bounce, once it reaches the bottom threshold 2")
     (test-equal? (get-cat cat-1 (simulate-until-at-wall cat-1 INITWORLD-AFTER-A-TICK))
                 `(,CAT1-X-COORD ,(- BBORDER 1) ,false ,"north")
                 1
                 "The cat should bounce, once it reaches the bottom threshold 1")))
   
   (test-group-with-points
    "Dragging Cat(s)" "These tests will check behavior of cats after mouse events" 5
    ([WORLD-AFTER-MEV-BD-1 (world-after-mev INITIAL_WORLD (/ CANVAS-WIDTH 2) 
                               (/ CANVAS-HEIGHT 2) "button-down")]
     [WORLD-AFTER-MEV-BD-2 (world-after-mev INITIAL_WORLD (+ (cat-x-pos (cat-1 INITIAL_WORLD)) 15) 
                               (+ (cat-y-pos (cat-1 INITIAL_WORLD)) 15) "button-down")]
     [WORLD-AFTER-MEV-DRAG (world-after-mev WORLD-AFTER-MEV-BD-2 CX CY "drag")]
     [WORLD-AFTER-MEV-BU (world-after-mev WORLD-AFTER-MEV-DRAG CX CY "button-up")]
     [WORLD-AFTER-MEV-BD-3 (world-after-mev WORLD-AFTER-MEV-BU (+ CAT2-X-COORD 15)
                               (- (cat-y-pos (cat-2 WORLD-AFTER-MEV-BU)) 10) "button-down")]
     [WORLD-AFTER-MEV-DRAG-2 (world-after-mev WORLD-AFTER-MEV-BD-3 CX CY "drag")]
     [WORLD-AFTER-MEV-DRAG-RBORDER (world-after-mev WORLD-AFTER-MEV-DRAG-2 RBORDER CY "drag")]
     [WORLD-AFTER-MEV-BU-RBORDER (world-after-mev WORLD-AFTER-MEV-DRAG-RBORDER RBORDER CY "button-up")]
     [WORLD-AFTER-SELECTING-BOTH-CATS (world-after-mev WORLD-AFTER-MEV-DRAG-2 CX CY "button-down")]
     [WORLD-AFTER-DRAGGING-BOTH-CATS (world-after-mev WORLD-AFTER-SELECTING-BOTH-CATS (+ CX 50)
                                                 (+ CY 50) "drag")]
     [WORLD-AFTER-DROPPING-BOTH-CATS (world-after-mev WORLD-AFTER-DRAGGING-BOTH-CATS (+ CX 50)
                                                      (+ CY 50) "button-up")]
     [WORLD-AFTER-MEV-DRAG-CAT2 (world-after-mev WORLD-AFTER-SELECTING-BOTH-CATS CX CY "drag")])
    
    (test-equal? (get-cats WORLD-AFTER-MEV-BD-1)
                 `(,`(,CAT1-X-COORD ,(cat-y-pos (cat-1 WORLD-AFTER-MEV-BD-1)) ,false ,"south") 
                   ,`(,CAT2-X-COORD ,(cat-y-pos (cat-2 WORLD-AFTER-MEV-BD-1)) ,false ,"south"))
                 1/2
                 "The cats should not get selected if button down occurs outside them")    
    
    (test-equal? (get-cats WORLD-AFTER-MEV-BD-2)
                 `(,`(,CAT1-X-COORD ,(cat-y-pos (cat-1 WORLD-AFTER-MEV-BD-1)) ,true ,"south") 
                   ,`(,CAT2-X-COORD ,(cat-y-pos (cat-2 WORLD-AFTER-MEV-BD-1)) ,false ,"south"))
                 1/2
                 "The cat should be selected on button down, only if the mouse is inside it.
It's position and direction should not change on button down. Other cat should not be affected")
    
    (test-equal? (get-cats WORLD-AFTER-MEV-DRAG)
                 `(,`(,CX ,CY ,true ,"south") 
                   ,`(,CAT2-X-COORD ,(cat-y-pos (cat-2 WORLD-AFTER-MEV-BD-1)) ,false ,"south"))
                 1/2
                 "The cat's position should change when it is dragged but its direction shouldn't change
Other cat should not be affected")
        
    (test-equal? (get-cats WORLD-AFTER-MEV-BU)
                 `(,`(,CX ,CY ,false ,"south") 
                   ,`(,CAT2-X-COORD ,(cat-y-pos (cat-2 WORLD-AFTER-MEV-BD-1)) ,false ,"south"))
                 1/2
                 "The selected cat should be unselected when the mouse button is released, the cat should
be placed in the new position")
    
    (test-equal? (get-cats WORLD-AFTER-MEV-DRAG-2)
                 `(,`(,CX ,CY ,false ,"south")
                   ,`(,CX ,CY ,true ,"south"))
                 1/2
                 "Dragging a cat over the other should not affect the underlying cat")
    
    (test-equal? (get-cats WORLD-AFTER-SELECTING-BOTH-CATS)
                 `(,`(,CX ,CY ,true ,"south")
                   ,`(,CX ,CY ,true ,"south"))
                 1/2
                 "Both the cats should be selected on button down, when one of them is overlapping the other")
    
         (test-equal? (get-cats WORLD-AFTER-DRAGGING-BOTH-CATS)
                 `(,`(,(+ CX 50) ,(+ CY 50) ,true ,"south")
                   ,`(,(+ CX 50) ,(+ CY 50) ,true ,"south"))
                 1/2
                 "Both cats should be dragged together when both of them are selected")
         
         (test-equal? (get-cats WORLD-AFTER-DROPPING-BOTH-CATS)
                 `(,`(,(+ CX 50) ,(+ CY 50) ,false ,"south")
                   ,`(,(+ CX 50) ,(+ CY 50) ,false ,"south"))
                 1/2
                 "Both cats should be unselected and their direction should not change")
    
    (test-equal? (get-cats WORLD-AFTER-MEV-BU-RBORDER)
                 `(,`(,CX ,CY ,false ,"south")
                   ,`(,RBORDER ,CY ,false ,"south"))
                 1
                 "Placing the cat near the border should not affect its direction and its position"))
   
   (test-group-with-points
    "Ket events test" "These tests will check behavior of world after key events" 8
    ([CX 200]
     [CY 200]
     [PAUSED-WORLD (world-after-key-event INITIAL_WORLD " ")]
     [PAUSED-WORLD-after-tick (world-after-tick PAUSED-WORLD)]
     [UNPAUSED-WORLD (world-after-kev PAUSED-WORLD-after-tick " ")]
     [UNPAUSED-WORLD-AFTER-TICK (world-after-tick UNPAUSED-WORLD)]
     [WORLD-WITH-CAT1-DIR->RIGHT (change-dir INITIAL_WORLD cat-1 CX CY "right")]
     [WORLD-WITH-CAT1-DIR->LEFT (change-dir INITIAL_WORLD cat-1 CX CY "left")]
     [WORLD-WITH-CAT2-DIR->UP (change-dir INITIAL_WORLD cat-2 CX CY "up")]
     [WORLD-WITH-CAT2-DIR->DOWN (change-dir INITIAL_WORLD cat-2 CX CY "down")]
     
     [WORLD-WITH-CATS->BD (world-after-mev WORLD-WITH-CAT2-DIR->UP CAT1-X-COORD INITIAL-Y-POS "button-down")]
     [WORLD-WITH-CAT1->LEFT (world-after-kev WORLD-WITH-CATS->BD "left")]
     [WORLD-WITH-CATS-DIFF-DIR-DRAG (world-after-mev WORLD-WITH-CAT1->LEFT CX CY "drag")]     
     [WORLD-WITH-CATS-DIFF-DIR-RELEASE (world-after-mev WORLD-WITH-CATS-DIFF-DIR-DRAG CX CY "button-up")]    
     [WORLD-WITH-SELECTED-CATS (world-after-mev WORLD-WITH-CATS-DIFF-DIR-RELEASE CX CY "button-down")]
     [WORLD-WITH-DRAGGED-CATS (world-after-mev WORLD-WITH-SELECTED-CATS (+ CX 50) (+ CY 50) "drag")]
     [WORLD-WITH-RELEASED-CATS (world-after-mev WORLD-WITH-DRAGGED-CATS (+ CX 50) (+ CY 50) "button-up")]
     
     [WORLD-WITH-CATS-IN-DIFF-DIR (change-dir WORLD-WITH-CAT2-DIR->UP cat-1 CX CY "right")]
     [WORLD-WITH-CATS-DIFF-DIR->BD (world-after-mev WORLD-WITH-CATS-IN-DIFF-DIR CX CY "button-down")]
     [WORLD-WITH-CATS->LEFT (world-after-kev WORLD-WITH-CATS-DIFF-DIR->BD "left")] 
     [WORLD-WITH-CATS->BU (world-after-mev WORLD-WITH-CATS->LEFT CX CY "button-up")])
    
    
    (test-equal? (get-cats PAUSED-WORLD)
                 `(,`(,CAT1-X-COORD ,(cat-y-pos (cat-1 INITIAL_WORLD)) ,false ,"south")
                   ,`(,CAT2-X-COORD ,(cat-y-pos (cat-2 INITIAL_WORLD)) ,false ,"south"))
                 1/2
                 "The cats should not move on tick, if the world is paused")
    
    (test-equal? (get-cats UNPAUSED-WORLD-AFTER-TICK)
                 `(,`(,CAT1-X-COORD ,(+ (cat-y-pos (cat-1 INITIAL_WORLD)) cat-speed) ,false ,"south")
                   ,`(,CAT2-X-COORD ,(+ (cat-y-pos (cat-2 INITIAL_WORLD)) cat-speed),false ,"south"))
                 1/2
                 "The cats should move after a tick if the world is unpaused")
    
    (test-equal? (get-cats WORLD-WITH-CAT1-DIR->RIGHT)
                 `(,`(,CX ,CY ,false ,"east")
                   ,`(,(cat-x-pos (cat-2 INITIAL_WORLD)) ,(cat-y-pos (cat-2 INITIAL_WORLD)) ,false ,"south"))
                 1/2
                 "The selected cat should change its direction as east, if it is selected, right key is pressed and unselected.
The other cat should be unaffected by this behavior")
    
    (test-equal? (get-cats (world-after-tick WORLD-WITH-CAT1-DIR->RIGHT))
                 `(,`(,(+ CX cat-speed) ,CY ,false ,"east")
                   ,`(,(cat-x-pos (cat-2 INITIAL_WORLD)) ,(+ (cat-y-pos (cat-2 INITIAL_WORLD)) cat-speed) ,false ,"south"))
                 1/2
                 "Cat 1 should move towards east after a tick at specified speed, if it is selected, right key is pressed and unselected")
    
    (test-equal? (get-cats WORLD-WITH-CAT1-DIR->LEFT)
                 `(,`(,CX ,CY ,false ,"west")
                   ,`(,(cat-x-pos (cat-2 INITIAL_WORLD)) ,(cat-y-pos (cat-2 INITIAL_WORLD)) ,false ,"south"))
                 1/2
                  "The selected cat should change its direction as west, if it is selected, left key is pressed and unselected.
The other cat should be unaffected by this behavior")
    
    (test-equal? (get-cats (world-after-tick WORLD-WITH-CAT1-DIR->LEFT))
                 `(,`(,(- CX cat-speed) ,CY ,false ,"west")
                   ,`(,(cat-x-pos (cat-2 INITIAL_WORLD)) ,(+ (cat-y-pos (cat-2 INITIAL_WORLD)) cat-speed) ,false ,"south"))
                 1/2
                 "Cat 1 should move towards west after a tick at specified speed, if it is selected, left key is pressed and unselected")
    
    (test-equal? (get-cats (world-after-tick WORLD-WITH-CAT2-DIR->UP))
                 `(,`(,(cat-x-pos (cat-1 INITIAL_WORLD)) ,(+ (cat-y-pos (cat-1 INITIAL_WORLD)) cat-speed) ,false ,"south")
                   ,`(,CX ,(- CY cat-speed) ,false ,"north"))
                 1/2
                 "Cat 2 should move towards north after a tick at specified speed, if it is selected, up key is pressed and unselected.
Other cat should be unaffected")
    
    (test-equal? (get-cats (world-after-tick WORLD-WITH-CAT2-DIR->DOWN))
                 `(,`(,(cat-x-pos (cat-1 INITIAL_WORLD)) ,(+ (cat-y-pos (cat-1 INITIAL_WORLD)) cat-speed) ,false ,"south")
                   ,`(,CX ,(+ CY cat-speed) ,false ,"south"))
                 1/2
                 "Cat 2 should move towards south after a tick at specified speed, if it is selected, down key is pressed and unselected.
Other cat should be unaffected")
    
    (test-equal? (get-cats (world-after-tick WORLD-WITH-RELEASED-CATS))
                 `(,`(,(- (+ CX 50) cat-speed) ,(+ CY 50) ,false ,"west")
                   ,`(,(+ CX 50) ,(- (+ CY 50) cat-speed) ,false ,"north"))
                 1
                 "The cats directions are changed separately, then selected together, dragged and released. 
They should continue moving in their directions unchanged")
    
    (test-equal? (get-cats (world-after-tick WORLD-WITH-CATS->BU))
                 `(,`(,(- CX cat-speed) ,CY ,false ,"west")
                   ,`(,(- CX cat-speed) ,CY ,false ,"west"))
                 1/2
                 "The cats were selected together, then left key is pressed, and released. They both should be travelling together towards west")
    (test-or
     1/2
     "The cat should hit the west wall, bounces back and start travelling towards east"
     (test-equal? (get-cat cat-1 (simulate-until-at-wall cat-1 WORLD-WITH-CAT1-DIR->LEFT))
                 `(,LBORDER ,CY ,false ,"east")
                 1/2
                 "The cat should hit the west wall, bounces back and start travelling towards east with no threshold")
    (test-equal? (get-cat cat-1 (simulate-until-at-wall cat-1 WORLD-WITH-CAT1-DIR->LEFT))
                 `(,(+ LBORDER 1) ,CY ,false ,"east")
                 1/2
                 "The cat should hit the west wall, bounces back and start travelling towards east with threshold 1")
    (test-equal? (get-cat cat-1 (simulate-until-at-wall cat-1 WORLD-WITH-CAT1-DIR->LEFT))
                 `(,(+ LBORDER 2) ,CY ,false ,"east")
                 1/2
                 "The cat should hit the west wall, bounces back and start travelling towards east  with threshold 2"))
    (test-or
     1/2
     "The cat should hit the east wall, bounces back and start travelling towards west"
     (test-equal? (get-cat cat-1 (simulate-until-at-wall cat-1 WORLD-WITH-CAT1-DIR->RIGHT))
                 `(,RBORDER ,CY ,false ,"west")
                 1/2
                 "The cat should hit the east wall, bounces back and start travelling towards west no threshold")
     (test-equal? (get-cat cat-1 (simulate-until-at-wall cat-1 WORLD-WITH-CAT1-DIR->RIGHT))
                 `(,(- RBORDER 1) ,CY ,false ,"west")
                 1/2
                 "The cat should hit the east wall, bounces back and start travelling towards west with threshold 1")
     (test-equal? (get-cat cat-1 (simulate-until-at-wall cat-1 WORLD-WITH-CAT1-DIR->RIGHT))
                 `(,(- RBORDER 2) ,CY ,false ,"west")
                 1/2
                 "The cat should hit the east wall, bounces back and start travelling towards west with threshold 2"))
    (test-or
     1/2
     "The cat should hit the north wall, bounces back and start travelling towards south"     
     (test-equal? (get-cat cat-2 (simulate-until-at-wall cat-2 WORLD-WITH-CAT2-DIR->UP))
                 `(,CX ,TBORDER ,false ,"south")
                 1/2
                 "The cat should hit the north wall, bounces back and start travelling towards south no threshold")
     (test-equal? (get-cat cat-2 (simulate-until-at-wall cat-2 WORLD-WITH-CAT2-DIR->UP))
                 `(,CX ,(+ TBORDER 1),false ,"south")
                 1/2
                 "The cat should hit the north wall, bounces back and start travelling towards south with threshold 1")
     (test-equal? (get-cat cat-2 (simulate-until-at-wall cat-2 WORLD-WITH-CAT2-DIR->UP))
                 `(,CX ,(+ TBORDER 2) ,false ,"south")
                 1/2
                 "The cat should hit the north wall, bounces back and start travelling towards south with threshold 2"))
    (test-or
     1/2
     "The cat should hit the south wall, bounces back and start travelling towards north no threshold"
    (test-equal? (get-cat cat-2 (simulate-until-at-wall cat-2 WORLD-WITH-CAT2-DIR->DOWN))
                 `(,CX ,BBORDER ,false ,"north")
                 1/2
                 "The cat should hit the south wall, bounces back and start travelling towards north no threshold")
    (test-equal? (get-cat cat-2 (simulate-until-at-wall cat-2 WORLD-WITH-CAT2-DIR->DOWN))
                 `(,CX ,(- BBORDER 1) ,false ,"north")
                 1/2
                 "The cat should hit the south wall, bounces back and start travelling towards north with threshold 1")
    (test-equal? (get-cat cat-2 (simulate-until-at-wall cat-2 WORLD-WITH-CAT2-DIR->DOWN))
                 `(,CX ,(- BBORDER 2) ,false ,"north")
                 1/2
                 "The cat should hit the south wall, bounces back and start travelling towards north with threshold 2"))
   )))

(displayln "Run tests with (run-pdp-tests tests05 [CCIS-username-as-string])")
(displayln "There will most probably be error messages, you can disregard them so long as it says [Output to xxx.pdf] at the end and the return value is #t")
(displayln "If the output fails, see if you already have a PDF of the same name open somewhere")

(let ([cla (vector->list (current-command-line-arguments))])
  (if (empty? cla)
      (begin
        (displayln "Run tests with (run-pdp-tests tests05 [CCIS-username-as-string])")
        (displayln "There will most probably be error messages, you can disregard them so long as it says [Output to xxx.pdf] at the end and the return value is #t")
        (displayln "If the output fails, see if you already have a PDF of the same name open somewhere"))
      (run-pdp-tests tests02 (first cla) (format "test-results-~a" (first (string-split (first cla) ":"))))))

