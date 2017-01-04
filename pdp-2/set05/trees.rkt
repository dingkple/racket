;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

;; run with (run 0)

(provide
 run
 initial-world
 world-after-key-event
 world-after-mouse-event
 world-to-roots
 node-to-center
 node-to-sons
 node-to-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RUN FUNCTION

;; run : Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world
;; EXAMPLES: (run 0) creates and runs a world in which shows a tree
(define (run x)
  (big-bang (initial-world x)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANT

;; dimensions of canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 400)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))

;; dimensions of square
(define SQUARE-SIZE 20)
(define HALF-SQUARE-SIZE (/ SQUARE-SIZE 2))

;; initial position of the root
(define INI-RX HALF-CANVAS-HEIGHT)
(define INI-RY HALF-SQUARE-SIZE)

;; colors
(define GREEN "green")
(define RED "red")
(define BLUE "blue")

;; scenes
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define OL-NODE (rectangle SQUARE-SIZE SQUARE-SIZE "outline" GREEN))
(define SL-NODE (rectangle SQUARE-SIZE SQUARE-SIZE "solid" GREEN))
(define SL-NODE-BY-BD (rectangle SQUARE-SIZE SQUARE-SIZE "solid" RED))

;; space for tree
(define NODES-SPACE-HEIGHT (* 3 SQUARE-SIZE))
(define NODES-SPACE-WIDTH (* 2 SQUARE-SIZE))
(define LEVEL-1-HEIGHT INI-RY)
(define LEVEL-2-HEIGHT (+ LEVEL-1-HEIGHT NODES-SPACE-HEIGHT))
(define LEVEL-3-HEIGHT (+ LEVEL-2-HEIGHT NODES-SPACE-HEIGHT))
(define LEVEL-1-WIDTH INI-RX)
(define LEVEL-2-WIDTH (+ LEVEL-1-WIDTH NODES-SPACE-WIDTH)) 
(define LEVEL-3-WIDTH (+ LEVEL-2-WIDTH NODES-SPACE-WIDTH))
(define CANVAS-BOTTOM-SPACE-NEED (+ (* 3 SQUARE-SIZE) HALF-SQUARE-SIZE))

;; numbers
(define ZERO 0)
(define NONE -1)
(define ONE 1)
(define TWO 2)

;; key event
(define T-KEY "t")
(define N-KEY "n")
(define D-KEY "d")
(define U-KEY "u")
(define SPACE-KEY " ")

;; mouse event
(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")
(define MOVE "move")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITION

(define-struct node (x-pos y-pos selected? children))
;; A Node is a (make-node Real Real Boolean ListOf<Node>)
;; Interp:
;; x-pos and y-pos represent positon of the center of the node
;; selected? represents whether the node has been selected
;; children represnets a list of nodes which are children of the given node

;; TEMPLATE
;; node-fn : Node -> ??
;; (define (node-fn n)
;;   (...
;;    (node-x-pos n)
;;    (node-y-pos n)
;;    (node-selected? n)
;;    (lon-fn (node-children n))))

;; EXAMPLES:
;; (make-node 200 20 false empty) => (make-node 200 20 false empty)

;; A ListOf<Node> is one of
;; -- empty
;; -- (cons Node ListOf<Node>)

;; TEMPLATE
;; lon-fn : ListOf<Node> -> ??
;; (define (lon-fn lon)
;;   (cond
;;     [(empty? lon) ...]
;;     [else (...
;;            (node-fn (first lon))
;;            (lon-fn (rest lon)))]))

;; EXAMPLES:
;; (list (make-node 200 20 false empty)) => 
;; (list (make-node 200 20 false empty))

(define-struct world (lor))
;; A World is a (make-world ListOf<Node>)
;; Interp:
;; lor represents a list of roots in the given world

;; TEMPLATE
;; w-fn : World -> ??
;; (define (w-fn w)
;;   (...
;;    (world-lor w)))

;; EXAMPLES:
;; (make-world (list (make-node 200 20 false empty))) =>
;; (make-world (list (make-node 200 20 false empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tests for roots
(define node-original
  (make-node
   98
   86
   false
   (list (make-node 58 146 false empty) (make-node 98 146 false empty))))

(define node-original-sons
  (list (make-node 58 146 false empty) (make-node 98 146 false empty)))  

(define node-original-selected
  (make-node
   98
   86
   true
   (list (make-node 58 146 false empty) (make-node 98 146 false empty))))

(define node-original-center
  (make-posn 98 86))

(define root-node
  (make-node 200 20 false empty))

(define root-node-selected
  (make-node 200 20 true empty))

(define root-with-children
  (make-node 200 20 false (list (make-node 200 80 false empty))))

(define root-with-children-selected
  (make-node 200 20 true (list (make-node 200 80 false empty))))

(define root-with-selected-children
  (make-node 200 20 false (list (make-node 200 80 true empty))))

(define lower-root 
  (make-node 200 300 false empty))

(define root-with-upper-children 
  (make-node 200 300 false (list (make-node 200 20 false empty))))

;; tests for world
(define world-empty (make-world empty))

(define world-original 
  (make-world
   (list
    (make-node
     279
     251
     false
     (list
      (make-node
       98
       86
       false
       (list (make-node 58 146 false empty) (make-node 98 146 false empty)))
      (make-node
       300
       310
       false
       (list
        (make-node 220 370 false empty)
        (make-node 260 370 false empty)
        (make-node 300 370 false empty))))))))

(define world-node-selected
  (make-world
   (list
    (make-node
     279
     251
     false
     (list
      (make-node
       98
       86
       true
       (list (make-node 58 146 false empty) (make-node 98 146 false empty)))
      (make-node
       300
       310
       false
       (list
        (make-node 220 370 false empty)
        (make-node 260 370 false empty)
        (make-node 300 370 false empty))))))))

(define world-node-selected-drag
  (make-world
   (list
    (make-node
     279
     251
     false
     (list
      (make-node
       108
       96
       true
       (list (make-node 68 156 false empty) (make-node 108 156 false empty)))
      (make-node
       300
       310
       false
       (list
        (make-node 220 370 false empty)
        (make-node 260 370 false empty)
        (make-node 300 370 false empty))))))))

(define world-original-N-KEY
  (make-world
   (list
    (make-node
     279
     251
     false
     (list
      (make-node
       98
       86
       true
       (list
        (make-node 18 146 false empty)
        (make-node 58 146 false empty) 
        (make-node 98 146 false empty)))
      (make-node
       300
       310
       false
       (list
        (make-node 220 370 false empty)
        (make-node 260 370 false empty)
        (make-node 300 370 false empty))))))))

(define world-empty-T-key
  (make-world 
   (list
    (make-node INI-RX INI-RY false empty))))

(define world-original-T-KEY
  (make-world
   (list
    (make-node INI-RX INI-RY false empty)
    (make-node
     279
     251
     false
     (list
      (make-node
       98
       86
       false
       (list 
        (make-node 58 146 false empty) 
        (make-node 98 146 false empty)))
      (make-node
       300
       310
       false
       (list
        (make-node 220 370 false empty)
        (make-node 260 370 false empty)
        (make-node 300 370 false empty))))))))

(define world-original-T-KEY-T-KEY
  (make-world
   (list
    (make-node INI-RX INI-RY false empty)
    (make-node
     279
     251
     false
     (list
      (make-node
       98
       86
       false
       (list 
        (make-node 58 146 false empty) 
        (make-node 98 146 false empty)))
      (make-node
       300
       310
       false
       (list
        (make-node 220 370 false empty)
        (make-node 260 370 false empty)
        (make-node 300 370 false empty))))))))

(define world-original-U-KEY
  (make-world
   (list
    (make-node
     279
     251
     false
     (list
      (make-node
       300
       310
       false
       (list
        (make-node 220 370 false empty)
        (make-node 260 370 false empty)
        (make-node 300 370 false empty)))))))) 

(define world-original-D-KEY
  (make-world
   (list
    (make-node
     279
     251
     false
     (list
      (make-node
       300
       310
       false
       (list
        (make-node 220 370 false empty)
        (make-node 260 370 false empty)
        (make-node 300 370 false empty)))))))) 

(define world-original-T-KEY-roots
  (list
   (make-node INI-RX INI-RY false empty)
   (make-node
    279
    251
    false
    (list
     (make-node
      98
      86
      false
      (list 
       (make-node 58 146 false empty) 
       (make-node 98 146 false empty)))
     (make-node
      300
      310
      false
      (list
       (make-node 220 370 false empty)
       (make-node 260 370 false empty)
       (make-node 300 370 false empty)))))))

(define world-root-node 
  (list root-node))

(define world-root-node-selected
  (list root-node-selected))

(define world-root-with-children
  (list root-with-children))

(define world-root-with-children-selected
  (list root-with-children-selected))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: an initial world
;; EXAMPLES: (initial-world 0) => world-empty
;; STRATEGY: function composition
(define (initial-world x)
  (make-world empty))

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world and a key event
;; RETURNS: a world after the given key event
;; EXAMPLES: see tests below
;; STRATEGY: cases on kev : KeyEvent
(define (world-after-key-event w kev)
  (cond
    [(key=? kev T-KEY) (world-after-t-key w)]
    [(key=? kev N-KEY) (world-after-n-key w)]
    [(key=? kev D-KEY) (world-after-d-key w)]
    [(key=? kev U-KEY) (world-after-u-key w)]
    [else w]))

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world, a mouse postion (x-pos and y-pos) and a mouse event
;; RETURNS: a world after the given MouseEvent at the given position
;; EXAMPLES: see tests below
;; STRATEGY: cases on mev : MouseEvent
(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN) (world-after-button-down w mx my)]
    [(mouse=? mev BUTTON-UP) (world-after-button-up w)]
    [(mouse=? mev DRAG) (world-after-drag w mx my)]
    [else w]))

;; world-to-scene : World -> Scene
;; GIVEN: a world 
;; RETURNS: a scene which describes the present world
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on w : World
(define (world-to-scene w)
  (world-to-scene-helper (world-lor w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene-helper : ListOf<Node> -> Scene
;; GIVEN: a list of nodes
;; RETURNS: a scene that describes the list of nodes on the canvas
;; EXAMPLES: see tests below
;; STRATEGY: function composition
(define (world-to-scene-helper lon)
  (trees-drawn-on-scene lon EMPTY-CANVAS))

;; trees-drawn-on-scene : ListOf<Node> Scene -> Scene
;; GIVEN: a list of nodes and a scene
;; RETURNS: a scene with the given list of nodes including their descendants 
;; drawn on the given scene
;; EXAMPLES: see tests below
;; STRATEGY: HOFC
(define (trees-drawn-on-scene lon s)
  (foldr
   ;; Node Scene -> Scene
   ;; GIVEN: a node and a scene
   ;; RETURNS: a scene with the given node and all its descendants drew on
   ;; the canvas
   (lambda (n s-til-now)
     (node-drawn-on-scene n s-til-now))
   s
   lon))

;; node-drawn-on-scene : Node Scene -> Scene
;; GIVEN: a node and a scene  
;; RETURNS: if the given node is not selected, draw an outline square in the 
;; scene; else draw a solid square according to the position of the current
;; boundary
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on n : Node
(define (node-drawn-on-scene n s)
  (if (node-selected? n)
      (selected-node-drawn-on-scene 
       n
       (node-drawn-on-scene-helper n s))
      (unselected-node-drawn-on-scene
       n
       (node-drawn-on-scene-helper n s))))

;; selected-node-drawn-on-scene : Node Scene -> Scene
;; GIVEN: a node and a scene
;; RETURNS: a scene with a selected node drawn on it
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on n : Node
(define (selected-node-drawn-on-scene n s)
  (selected-node-drawn-on-scene-helper
   (node-x-pos n)
   (node-y-pos n)
   (boundary-x-find-from-children (node-children n) (node-x-pos n))
   s))

;; selected-node-drawn-on-scene-helper : Real Real Real Scene -> Scene
;; GIVEN: x and y coordinates of node's center, a real representing the x 
;; coordinate of the boundary and a scene and a scene
;; RETURNS: if the boundary is in the canvas, draw a solid green square 
;; in the scene at the given position and a boundary line; else, draw a red 
;; solid square in the scene at the given position
;; EXAMPLES: see tests below
;; STRATEGY: function composition
(define (selected-node-drawn-on-scene-helper x y boundary s)
  (if (< boundary ZERO)
      (place-image SL-NODE-BY-BD x y s)
      (place-image 
       SL-NODE 
       x 
       y 
       (boundary-line-add-to-scene s boundary))))

;; boundary-x-find-from-children : ListOf<Node> Real -> Real
;; GIVEN: a list of nodes and the current x-pos of boundary
;; RETURNS: a real presenting where should the son node created,
;; if the node is a root and with empty subtree, then the current x-pos
;; boundary should be its left edge; else, return the x-pos of boundary
;; for son to be created 
;; EXAMPLES: (boundary-x-find-from-children empty 200) => 190
;; STRATEGY: function composition
(define (boundary-x-find-from-children lon x)
  (if (empty? lon)
      (- x HALF-SQUARE-SIZE)
      (boundary-x-find-from-children-helper lon x)))

;; boundary-x-find-from-children-helper : ListOf<Node> Real -> Real
;; GIVEN: a list of nodes and the current x-pos of boundary
;; RETURNS: the updated x-pos of the boundary for a list of nodes
;; EXAMPLES: (boundary-x-find-from-children 
;;;            (list (make-node 200 80 false empty))) => 150
;; STRATEGY: structural decomposition on n: Node
(define (boundary-x-find-from-children-helper lon x)
  (foldr
   ;; Node Real -> Real
   ;; GIVEN: a node and a current x-pos of the boundary
   ;; RETURNS: an updated x-pos of boundary for the given node
   (lambda (n num-ans)
     (min (- (node-x-pos n) NODES-SPACE-WIDTH HALF-SQUARE-SIZE)
          num-ans))       
   (* TWO CANVAS-WIDTH)
   lon))

;; unselected-node-drawn-on-scene : Node Scene -> Scene
;; GIVEN: a node and a scene
;; RETURNS: a scene with a unselected node drawn on it
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on n : Node
(define (unselected-node-drawn-on-scene n s)
  (place-image
   OL-NODE
   (node-x-pos n)
   (node-y-pos n)
   s))

;; node-drawn-on-scene-helper : Node Scene -> Scene 
;; GIVEN: a node and a scene
;; RETURNS: a scene with a tree drawn on it
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on n : Node
(define (node-drawn-on-scene-helper n s)
  (line-add-from-node-to-children
   (node-children n)
   (node-x-pos n)
   (node-y-pos n)
   (trees-drawn-on-scene (node-children n) s)))

;; line-add-from-node-to-children : ListOf<Node> Real Real Scene -> Scene
;; GIVEN: a list of node, the position of the node (x-pos and y-pos) and 
;; a scene
;; RETURNS: a scene with lines added from the a list of nodes to all their
;; children
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on n : Node
(define (line-add-from-node-to-children lon x y s)
  (foldr
   ;; Node Scene -> Scene
   ;; GIVEN: a node and a scene
   ;; RETURNS: a scene with lines added from the given node's center 
   ;; to its children's
   (lambda (n s-til-now)
     (scene+line s-til-now x y (node-x-pos n) (node-y-pos n) BLUE))
   s
   lon))

;; boundary-line-add-to-scene : Scene Real -> Scene
;; GIVEN: a scene and the boundary's x-pos
;; RETURNS: add a red line in the given scene at the given x-pos if it's
;; in the canvas
;; EXAMPLES: see tests below
;; STRATEGY: function composition
(define (boundary-line-add-to-scene s boundary)
  (if (<= ZERO boundary CANVAS-WIDTH)
      (scene+line s boundary ZERO boundary CANVAS-HEIGHT RED)
      s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-button-down : World Integer Integer -> World
;; GIVEN: a world and a mouse position (x-pos and y-pos)
;; RETURNS: the given world after button down at the given position
;; EXAMPLES: (wrold-after-button-down world-root-node 200 20) => 
;; world-root-node-selected
;; STRATEGY: structural decomposition on w : World
(define (world-after-button-down w mx my)
  (make-world (trees-after-button-down (world-lor w) mx my)))

;; trees-after-button-down : ListOf<Node> Integer Integer -> ListOf<Node>
;; GIVEN: a list of nodes and a mouse position (x-pos and y-pos)
;; RETURNS: a list of nodes after button down at the given position
;; EXAMPLES: (trees-after-button-down (world-lor world-root-node) 200 20) => 
;; (list root-node-selected)
;; STRATEGY: HOFC
(define (trees-after-button-down lon mx my)
  (map
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node after button down at the given position
   (lambda (n)
     (if (in-node? n mx my)
         (node-set-selected n mx my)
         (trees-after-button-down-helper n mx my)))
   lon))

;; in-node? : Node Integer Integer -> Boolean
;; GIVEN: a node and the position(x-pos and y-pos) of the mouse
;; RETURNS: true iff the position the mouse is inside the node(the square area)
;; EXAMPLES: (in-node? root-node 200 200) => true
;; STRATEGY: structural decomposition on n : Node
(define (in-node? n mx my)
  (and (<= (- (node-x-pos n) HALF-SQUARE-SIZE)
           mx
           (+ (node-x-pos n) HALF-SQUARE-SIZE))
       (<= (- (node-y-pos n) HALF-SQUARE-SIZE)
           my
           (+ (node-y-pos n) HALF-SQUARE-SIZE))))

;; node-set-selected : Node Integer Integer -> Node
;; GIVEN: a node and a mouse position (x-pos and y-pos)
;; RETURNS: the given node set to be selected and its children updated
;; EXAMPLES: (node-set-selected root-node) => root-node-selected
;; STRATEGY: structural decomposition on n : Node 
(define (node-set-selected n mx my)
  (make-node (node-x-pos n)
             (node-y-pos n)
             true
             (trees-after-button-down (node-children n) mx my)))

;; trees-after-button-down-helper : Node Integer Integer -> Node
;; GIVEN: a node and a mouse position (x-pos and y-pos)
;; RETURNS: a node after button-down at the given position
;; EXAMPLES: (trees-after-button-down-helper root-node 200 200) => root-node
;; STRATEGY: structural decomposition on n : Node
(define (trees-after-button-down-helper n mx my)
  (make-node (node-x-pos n)
             (node-y-pos n)
             false
             (trees-after-button-down (node-children n) mx my)))

;; world-after-drag : World Integer Integer -> World
;; GIVEN: a world and a mouse position (x-pos and y-pos)
;; RETURNS: a world after drag event to the given position 
;; EXAMPLES: (world-after-drag world-root-node-selected 200 20) => 
;; world-root-node-selected
;; STRATEGY: structural decomposition on w : World
(define (world-after-drag w mx my)
  (make-world (trees-after-drag (world-lor w) mx my)))

;; trees-after-drag : ListOf<Node> Integer Integer -> ListOf<Node>
;; GIVEN: a list of nodes and a mouse position (x-pos and y-pos)
;; RETURNS: a list of nodes after drag event to the given position
;; EXAMPLES: (trees-after-drag (list root-node-selected) 200 20) =>
;; (list root-node-selected)
;; STRATEGY: structural decomposition on n : Node
(define (trees-after-drag lon mx my)
  (map 
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node after drag event
   (lambda (n)
     (if (node-selected? n)
         (node-moved-by-vector n mx my 
                               (- mx (node-x-pos n)) 
                               (- my (node-y-pos n)))
         (trees-after-drag-helper n mx my)))
   lon))

;; node-moved-by-vector : Node Integer Integer Real Real -> Node
;; GIVEN: a node a position of the mosue (x-pos and y-pos) and a vector 
;; (x-pos y-pos) directed from the original center of root node to its new 
;; position after the drag event
;; RETURNS: a node after the given mouse event
;; EXAMPLES: (node-moved-by-vector root-node-selected 200 20 0 0) => 
;; root-node-selected
;; STRATEGY: structural decomposition on n : Node
(define (node-moved-by-vector n mx my vx vy)
  (make-node  (+ (node-x-pos n) vx)
              (+ (node-y-pos n) vy)
              (node-selected? n)
              (children-moved-by-vector (node-children n) mx my vx vy)))

;; children-moved-by-vector : ListOf<Node> Integer Integer Real Real 
;;                            -> ListOf<Node>
;; GIVEN: a list of nodes the position of mouse (x-pos and y-pos) and 
;; a vector(x-pos y-pos) directed from the original center of the given
;; children's root node to its new position after the drag event
;; RETURNS: a list of nodes whose position updated by the given vector
;; EXAMPLES: (children-moved-by-vector (list root-node-selected) 200 20 0 0) 
;; => (list root-node-selected)
;; STRATEGY: structural decomposition on n : Node
(define (children-moved-by-vector lon mx my vx vy)
  (map
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node after the drag event
   (lambda (n)
     (if (node-selected? n)
         (node-moved-by-vector n mx my 
                               (- mx (node-x-pos n))
                               (- my (node-y-pos n)))
         (node-moved-by-vector n mx my vx vy)))
   lon))

;; trees-after-drag-helper : Node Integer Integer -> Node
;; GIVEN: a node and a mouse position (x-pos and y-pos)
;; RETURNS: a node after drag event to the given position
;; EXAMPLES: (trees-after-drag-helper root-node 200 20) => root-node
;; STRATEGY: structural decomposition on n : Node
(define (trees-after-drag-helper n mx my)
  (make-node (node-x-pos n)
             (node-y-pos n)
             (node-selected? n)
             (trees-after-drag (node-children n) mx my)))

;; world-after-button-up : World -> World
;; GIVEN: a world
;; RETURNS: a world after the button up mouse event
;; EXAMPLES: (world-after-button-up world-root-node-selected) => 
;; world-root-node
;; STRATEGY: structural decomposition on w : World
(define (world-after-button-up w)
  (make-world (trees-after-button-up (world-lor w))))

;; trees-after-button-up : ListOf<Node> -> ListOf<Node>
;; GIVEN: a list of nodes
;; RETURNS: a list of nodes after the button up mouse event
;; EXAMPLES: (trees-after-button-up (list root-node-selected)) =>
;; (list root-node)
;; STRATEGY: structural decomposition on n : Node
(define (trees-after-button-up lon)
  (map 
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node after the button up mouse event
   (lambda (n)
     (make-node  (node-x-pos n)
                 (node-y-pos n)
                 false
                 (trees-after-button-up (node-children n))))
   lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-t-key : World -> World
;; GIVEN: a world
;; RETURNS: a world after the key event T-KEY 
;; EXAMPLES: (world-after-n-key world-empty) => 
;; (make-world (list (make-node 200 10 false empty)))
;; STRATEGY: structural decomposition on w : World
(define (world-after-t-key w)
  (make-world (cons (make-node INI-RX INI-RY false empty) 
                    (world-lor w))))

;; world-after-n-key : World -> World
;; GIVEN: a world 
;; RETURNS: a world after the key event N-KEY
;; EXAMPLES: (world-after-n-key-helper world-root-node-selected) => 
;; world-root-with-children-selected
;; STRATEGY: structural decomposition on w : World
(define (world-after-n-key w)
  (make-world (trees-after-n-key (world-lor w))))

;; trees-after-n-key : ListOf<Node> -> ListOf<Node> 
;; GIVEN: a list of nodes
;; RETURNS: a list of nodes after the key event N-KEY
;; EXAMPLES: (trees-after-n-key (list root-node-selected) 190) => 
;; (list root-with-children-selected)
;; STRATEGY: structural decomposition on n : Node
(define (trees-after-n-key lon)
  (map
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node after the key event N-KEY
   (lambda (n)
     (if (new-node-should-add? n)
         (node-added-at-boundary 
          n 
          (boundary-x-find-from-children (node-children n) (node-x-pos n)))
         (trees-after-n-key-helper n)))
   lon))

;; new-node-should-add?: Node -> Boolean
;; GIVEN: a Node
;; RETURNS: true iff the boundary from the children of the given node is within
;; the canvas and the given node is selected
;; EXAMPLES: (new-node-should-add? root-node) => false
;; STRATEGY: structural decomposition on n : Node
(define (new-node-should-add? n)
  (and (node-selected? n) 
       (> (boundary-x-find-from-children 
           (node-children n) 
           (node-x-pos n)) 
          ZERO)))

;; node-added-at-boundary : Node Real -> Node
;; GIVEN: a node and a the x-pos of the boundary
;; RETURNS: a node which has the given son node created at the boundary
;; EXAMPLES: (node-added-at-boundary root-node-selected 190) => 
;; root-with-children-selected
;; STRATEGY: structural decomposition on n : Node
(define (node-added-at-boundary n boundary)
  (make-node  (node-x-pos n)
              (node-y-pos n)
              (node-selected? n)
              (node-added-at-boundary-helper n boundary)))

;; node-added-at-boundary-helper : Node Real -> ListOf<Node>
;; GIVEN: a node and the x-pos of the boundary
;; RETURNS: add child (left edge at boundary) to the given node 
;; EXAMPLES: (node-added-at-boundary-helper root-node-selected 190) =>
;; (list (make-node 200 80 false empty) empty)
;; STRATEGY: structural decomposition on n : Node
(define (node-added-at-boundary-helper n boundary)
  (cons (make-node (+ boundary HALF-SQUARE-SIZE)
                   (+ NODES-SPACE-HEIGHT (node-y-pos n))
                   false
                   empty) 
        (trees-after-n-key (node-children n))))

;; trees-after-n-key-helper : Node -> Node
;; GIVEN: a node
;; RETURNS: a node whose children have been updated if there is new 
;; descendent created
;; EXAMPLES: (trees-after-n-key-helper root-node ) => root-node
;; STRATEGY: structural decomposition on n : Node
(define (trees-after-n-key-helper n)
  (make-node (node-x-pos n)
             (node-y-pos n)
             (node-selected? n)
             (trees-after-n-key (node-children n))))

;; world-after-d-key : World -> World
;; GIVEN: a world
;; RETURNS: a world after key event D-KEY
;; EXAMPLES: (world-after-d-key world-root-node-selected) => world-empty
;; STRATEGY: structural decomposition on w : World
(define (world-after-d-key w)
  (make-world (trees-after-d-key (world-lor w))))

;; trees-after-d-key : ListOf<Node> -> ListOf<Node> 
;; GIVEN: a list of nodes
;; RETURNS: a list of nodes after the key event D-KEY
;; EXAMPLES: (trees-after-d-key (list root-node-selected)) => empty
;; STRATEGY: structural decomposition on n : Node
(define (trees-after-d-key lon)
  (foldr
   ;; Node ListOf<Node> -> ListOf<Node>
   ;; GIVEN: a node and a list of nodes
   ;; RETURNS: a list of nodes after the key event D-KEY
   (lambda (n lon-ans)
     (if (node-selected? n)
         lon-ans
         (cons (node-after-d-key n) lon-ans)))
   empty
   lon))

;; node-after-d-key : Node -> Node
;; GIVEN: a node
;; RETURNS: a node whose descendents are updated by the key-event D-KEY
;; EXAMPLES: (node-after-d-key root-with-selected-children) => root-node
;; STRATEGY: structural decomposition on n : Node
(define (node-after-d-key n)
  (make-node (node-x-pos n)
             (node-y-pos n)
             (node-selected? n)
             (trees-after-d-key (node-children n))))

;; world-after-u-key : World -> World
;; GIVEN: a world
;; RETURNS: a world w after the key event U-KEY
;; EXAMPLES: (world-after-u-key world-root-with-children) => world-empty
;; STRATEGY: structural decomposition on w : World
(define (world-after-u-key w)
  (make-world (trees-after-u-key (world-lor w))))

;; trees-after-u-key : ListOf<Node> -> ListOf<Node>
;; GIVEN: a list of nodes
;; RETURNS: a list of nodes after the key event U-KEY
;; EXAMPLES: (trees-after-u-key (list root-with-children)) => empty
;; STRATEGY: structural decomposition on n : Node
(define (trees-after-u-key lon)
  (foldr
   ;; Node ListOf<Node> -> ListOf<Node>
   ;; GIVEN: a node and a list of nodes
   ;; RETURNS: a list of nodes after the key event U-KEY
   (lambda (n lst-ans)
     (if (< (node-y-pos n) HALF-CANVAS-HEIGHT)
         lst-ans
         (cons (node-after-u-key n) lst-ans))) 
   empty
   lon))

;; node-after-u-key : Node -> Node
;; GIVEN: a node
;; RETURNS: a node after the key event U-KEY
;; EXAMPLES: (node-after-u-key root-with-upper-children) => lower-root
;; STRATEGY: structural decomposition on n : Node
(define (node-after-u-key n)
  (make-node  (node-x-pos n)
              (node-y-pos n)
              (node-selected? n)
              (trees-after-u-key (node-children n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-roots : World -> ListOf<Node>
;; GIVEN: a world
;; RETURNS: a list of all the roots in the given world
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on w : World
(define (world-to-roots w)
  (world-lor w))

;; node-to-center : Node -> Posn
;; GIVEN: a node
;; RETURNS: the center of the given node as it is to be displayed on the scene
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on n : Node
(define (node-to-center n)
  (make-posn (node-x-pos n) (node-y-pos n)))

;; node-to-sons : Node -> ListOf<Node>
;; GIVEN: a node
;; RETURNS: all the children of the given node
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on n : Node
(define (node-to-sons n)
  (node-children n))

;; node-to-selected? : Node -> Boolean
;; GIVEN: a node n
;; RETURNS: true iff the node is selected
;; EXAMPLES: see tests below
;; STRATEGY: strucutral decomposition on n : Node
(define (node-to-selected? n)
  (node-selected? n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tests for scene
(define world-original-scene 
  (world-to-scene-helper 
   (list
    (make-node
     279
     251
     false
     (list
      (make-node
       98
       86
       false
       (list (make-node 58 146 false empty) (make-node 98 146 false empty)))
      (make-node
       300
       310
       false
       (list
        (make-node 220 370 false empty)
        (make-node 260 370 false empty)
        (make-node 300 370 false empty))))))))

(define world-node-selected-boundary-outside-scene
  (world-to-scene-helper 
   (list
    (make-node
     279
     251
     false
     (list
      (make-node
       98
       86
       true
       (list
        (make-node 18 146 false empty)
        (make-node 58 146 false empty) 
        (make-node 98 146 false empty)))
      (make-node
       300
       310
       false
       (list
        (make-node 220 370 false empty)
        (make-node 260 370 false empty)
        (make-node 300 370 false empty))))))))

(define world-node-selected-scene 
  (world-to-scene-helper
   (list
    (make-node
     279
     251
     false
     (list
      (make-node
       98
       86
       true
       (list (make-node 58 146 false empty) (make-node 98 146 false empty)))
      (make-node
       300
       310
       false
       (list
        (make-node 220 370 false empty)
        (make-node 260 370 false empty)
        (make-node 300 370 false empty))))))))

(begin-for-test
  (check-equal? (initial-world 0) world-empty
                "there should be an empty world, but it does not")
  (check-equal? (world-after-mouse-event world-original 98 86 BUTTON-DOWN)
                world-node-selected
                "mouse button down in the square should select it, 
                but does NOT")
  (check-equal? (world-after-mouse-event world-node-selected 108 96 DRAG)
                world-node-selected-drag
                "drag should move the whole sub-tree to the given position, 
                but does NOT")
  (check-equal? (world-after-mouse-event world-node-selected 108 96 BUTTON-UP)
                world-original
                "button-up should unselect all the nodes, but does NOT")
  (check-equal? (world-after-mouse-event world-original 108 96 MOVE)
                world-original
                "any MouseEvent other than {BUTTON-UP, BUTTON-DOWN, DRAG} 
                should do nothing, but does NOT")
  (check-equal? (world-after-key-event world-original T-KEY)
                world-original-T-KEY
                "T-KEY should add a new root at the top center at the canvas, 
                but does NOT")
  (check-equal? (world-after-key-event world-empty T-KEY)
                world-empty-T-key
                "T-KEY should add a new root at the top center at the canvas, 
                but does NOT")
  (check-equal? (world-after-key-event world-original-N-KEY N-KEY)
                world-original-N-KEY
                "adding new nodes when boundary beyond the left edge is not 
                allowed, but it DOES")
  (check-equal? (world-after-key-event world-original U-KEY)
                world-original-U-KEY
                "U-KEY should delete all the nodes and thier children in the
                upper canvas, but does NOT")
  (check-equal? (world-after-key-event world-node-selected N-KEY)
                world-original-N-KEY
                "N-KEY should add a new child to the selected node, 
                but does NOT")
  (check-equal? (world-after-key-event world-node-selected D-KEY)
                world-original-D-KEY
                "D-KEY should delete the node and all its children, 
                but does NOT")
  (check-equal? (world-after-key-event world-original SPACE-KEY)
                world-original
                "Any KeyEvent other than {k,t,u,d} should do nothing, 
                but does NOT")
  (check-equal? (world-to-roots world-original-T-KEY)
                world-original-T-KEY-roots
                "should give the all the roots in the given world, 
                but does NOT")
  (check-equal? (node-to-selected? node-original)
                false
                "should return true iff the node is selected, but it's wrong")
  (check-equal? (node-to-sons node-original)
                node-original-sons
                "should return the given node's children, but does NOT")
  (check-equal? (node-to-center node-original)
                node-original-center
                "should return the center of the given node, but does NOT")
  (check-equal? (world-to-scene world-original) 
                world-original-scene
                "the scene should show trees without any selected node, 
                but it does NOT")
  (check-equal? (world-to-scene world-node-selected)
                world-node-selected-scene
                "the scene should show trees among which has one green solid 
                selected node, but it does NOT")
  (check-equal? (world-to-scene world-original-N-KEY)
                world-node-selected-boundary-outside-scene
                "the scene should show trees among which has one red solid 
                selected node, but it does NOT")
  (check-equal? (boundary-line-add-to-scene EMPTY-CANVAS -10) EMPTY-CANVAS
                "when the boundary is outside of the left edge of canvas, the 
                scene shoud not change, but it DOES")
  (check-equal? (boundary-x-find-from-children empty 200) 190
                "the boundary should be in the left edge of the root node, but
                it does NOT")
  (check-equal? (children-moved-by-vector (list root-node-selected) 200 20 0 0) 
                (list root-node-selected) "the list of node shoud not change, but
                it DOES"))

