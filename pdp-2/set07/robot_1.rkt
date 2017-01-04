;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Filename: robot.rkt
;; Algorithm:

;; The algorithm for this problem is BFS, every loop discovers new positions
;; reachable from the positions the robot can currently get to, and records all
;; the edges as the steps, once the target position is reached or the robot
;; can not discover new positions anymore, we can get the plan from the target 
;; position back to the start position. Or just return false, iff the robot can
;; not find the given target position.


(require "extras.rkt")
(require rackunit)
(require "sets.rkt")

(provide path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define NORTH "north")
(define SOUTH "south")
(define EAST "east")
(define WEST "west")

(define MV-E (list EAST 1))
(define MV-S (list SOUTH 1))
(define MV-W (list WEST 1))
(define MV-N (list NORTH 1))

(define RIGHT-FRAME 1)
(define BOTTOM-FRAME -1)
(define TOP-FRAME 2)
(define LEFT-FRAME -2)

(define DV-N (list 0 -1))
(define DV-S (list 0 1))
(define DV-W (list -1 0))
(define DV-E (list 1 0))


(define DIRECTION-LST (list NORTH SOUTH WEST EAST))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITION

;; A Position is a (list PosInt PosInt)
;; Interp: (x y) represents the position at position x, y.

;; TEMPLATE
;; p-fn: Position -> ??
;; (define (p-fn p)
;;   (... (first p)
;;        (second p)))


;; A ListOfPosition is one of
;; -- empty
;; -- (cons Position ListOfPosition)
;; WHERE: the PositionSet is a list of positions without duplication

;; TEMPLATE
;; ps-fn : PositionSet -> ListOfPosition
;; (define (ps-fn ps)
;;   (cond
;;     [(empty? ps) ...]
;;     [else (... (p-fn (first ps))
;;                (ps-fn (rest ps)))]))

;; A Block is a ListOfPosition
;; Interp: Block represents the list of positions which robot can Not 
;; pass through.

;; A Direction is one of
;; -- NORTH   interp: represents the current direction is north
;; -- EAST    interp: represents the current direction is east
;; -- SOUTH   interp: represents the current direction is south
;; -- WEST    interp: represents the current direction is west

;; TEMPLATE
;; d-fn : Direction -> ??
;; (define (d-fn d)
;;   (cond
;;     [(string=? d NORTH) ...]
;;     [(string=? d EAST) ...]
;;     [(string=? d SOUTH) ...]
;;     [(string=? d WEST) ...]))


;; A ListOfDirection (LOD) is one of
;; -- empty
;; -- (cons Direction LOD)

;; TEMPLATE
;; lod-fn : LOD ->??
;; (define (lod-fn lod)
;;   (cond
;;     [(empty? lod) ...]
;;     [else (... (d-fn (first lod))
;;                (lod-fn (rest lod)))]))

;; A Step is a (list Position Position)
;; Interp: 
;; a step is a list containing two positions, representing a step from the 
;; first position to second position, except for the start position, a step
;; can be (list start start), every step else must be (list p1 p2), where p2 is 
;; just 1 step away from p1, for example, p1: (list a b), p2: (list c d),
;; then either (abs (- a c)) = 1 and (abs (- b d)) = 0
;;          or (abs (- a c)) = 0 and (abs (- b d)) = 1

;; TEMPLATE
;; s-fn : Step -> ??
;; (define (s-fn s)
;;   (... (p-fn (first s))
;;        (p-fn (second s))))


;; A NEListOfStep (NELOS) is one of
;; -- (cons Step empty)
;; -- (cons Step ListOfStep)

;; TEMPLATE
;; nelos-fn : NELOS -> ??
;; (define (nelos-fn nelos)
;;   (cond
;;     [(empty? (rest nelos)) 
;;      (... (s-fn (first nelos)))]
;;     [else (... (s-fn (first nelos))
;;                (los-fn (rest nelos)))]))

;; A Path is a NEListOfStep
;; Interp:
;; a path is a non empty list of steps, where in each step, its first 
;; position equals the second position of the previous step (if exists) 
;; and its second position equals the first position of the next 
;; step (if exists).


;; A RightOrBottomFrame is one of
;; -- RIGHT-FRAME
;; -- BOTTOM-FRAME
;; Interp:
;; The two frames and two lines x=0 y=0 make a rectangle that the robot
;; should moved within to find the target position iff there exists a way 
;; from the start position to. Since the positions are only infinite to east 
;; and south, so we can just calculate the right-frame and the bottom-frame 
;; to make sure the robot will not go too far.

;; TEMPLATE
;; frame-fn : RightOrBottomFrame -> ??
;; (define (frame-fn f)
;;   (cond
;;     [(= f RIGHT-FRAME) ...]
;;     [(= f BOTTOM-FRAME) ...]))

;; A Move is a (list Direction PosInt)
;; Interp: a move of the specified number of steps in the indicated
;; direction. 

;; TEMPLATE
;; mov-fn : Move -> ??
;; (define (mov-fn mov)
;;   (... (d-fn (first mov))
;;        (second mov)))


;; A ListOfMove (LOM) is one of 
;; -- empty
;; -- (cons Move LOM)

;; TEMPLATE
;; lom-fn : LOM -> ??
;; (define (lom-fn lom)
;;   (cond
;;     [(empty? lom) ...]
;;     [else (... (mov-fn (first lom))
;;                (lom-fn (rest lom)))]))

;; A Plan is a ListOfMove
;; WHERE: the list does not contain two consecutive moves in the same
;; direction.

;; A NEListOfMove (NELOM) is a (cons Move LOM)

;; TEMPLATE
;; nelom-fn :  -> ??
;; (define (nelom-fn nelom)
;;   (... (mov-fn (first rp))
;;        (lom-fn (rest rp))))

;; A RealPlan is a NEListOfMove

;; A MaybePlan is one of:
;; -- false
;; -- RealPlan
;; Interp:
;; false iff there's not a way from the start position to the target position
;; RealPlan as interpreted before, representing there's a plan containing at
;; least one step

;; TEMPLATE
;; (define (mp-fn mp)
;;   (cond
;;     [(false? mp) ...]
;;     [(real-plan? mp) ...]))

;; A DirectionVector is one of:
;;  --(list 0 1)
;;  --(list 1 0)
;;  --(list 0 -1)
;;  --(list -1 0)
;; Interp:
;;   a direction-vector is a vector representing the directions, the four 
;;   vectors are for these directions respectively:
;;   --SOUTH
;;   --EAST
;;   --NORTH
;;   --WEST
;; TEMPLATE:
;; (define (dv-fn dv)
;;   (cond
;;     [(equals? dv DV-E) ...]
;;     [(equals? dv DV-N) ...]
;;     [(equals? dv DV-W) ...]
;;     [(equals? dv DV-S) ...]))


;;????????????????????????????????

;; real-plan?: List -> Boolean
;; GIVEN: a list
;; RETURNS: true iff the given LOM is a RealPlan
;; EXAMPLES: (real-plan? empty) -> false
;; STRATEGY: function composition
(define (real-plan? lom)
  (and (not (empty? lom)) 
       (plan? lom)))

;; plan?: Plan -> Boolean
;; GIVEN: a plan pl
;; WHERE: the Plan is not empty
;; RETURNS: true iff the given list of move is a plan
;; EXAMPLES: (plan? (list (list EAST 1))) -> true
;; STRATEGY: HOFC
(define (plan? pl)
  (andmap
   move?
   pl))

;; direction?: Any -> Boolean
;; GIVEN: Any
;; RETURNS: true iff the given arg is a direction
;; EXAMPLES: (direction? "yes") -> false
;; STRATEGY: function composition
(define (direction? d)
  (or
   (string=? d NORTH)
   (string=? d SOUTH)
   (string=? d EAST)
   (string=? d WEST)))

;; move?: List -> Boolean
;; GIVEN: a list lst
;; RETURNS: true iff the given list is a Move
;; EXAMPLES: (move? empty) -> false
;; STRATEGY: structure decomposition on lst : List
(define (move? lst)
  (cond
    [(empty? lst) false]
    [else (and 
           (= (length lst) 2)
           (direction? (first lst))
           (integer? (second lst))
           (> (second lst) 0))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples for testing

(define block-reachable 
  (list (list 4 1) (list 4 2) (list 4 3) (list 4 4)
        (list 1 2) (list 2 2) (list 3 5)))

(define block-non-reachable 
  (list (list 4 1) (list 4 2) (list 4 3) (list 4 4)
        (list 1 2) (list 2 2) (list 3 2) (list 3 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; path-from-start-position: Position Block Path -> Path 
;; GIVEN: a position, a block and a path
;; WHERE: 1)the position is the target position to stop
;;        2)the visited is  a path containing steps the robot has discovered
;; RETURNS: all the steps the robot can discover
;; TERMINATION ARGUMENT: since the robot can only move within a limited range,
;;    every recurrence the number of positions have NOT been visited is
;;    decreasing
;; HALTING MEASURE: the number of the positions the robot can get to and have
;;    not appeared as the second position in a step in the given path
;; EXAMPLES: (path-from-start-position 
;;              (list 2 1) empty 
;;              (list (list (list 1 1) (list 1 1))))
;;           -> (list (list (list 1 1) (list 1 1))
;;                    (list (list 1 1) (list 1 2)) 
;;                    (list (list 1 1) (list 2 1)))
;; STRATEGY: general recursion
;; (define (path-from-start-position ep bls visited nps)
;;   (cond
;;     [(exist-pos-in-visited? ep visited) visited]
;;     [else (if (subset? (new-position-add-to-path ep bls visited) visited)
;;               visited
;;               (path-from-start-position 
;;                 ep bls 
;;                 (new-position-add-to-path ep bls visited)))]))
(define (path-from-start-position ep bls visited nps fx fy)
  (cond
    [(exist-pos-in-visited? ep nps) visited]
    [else (if (empty? nps)
              visited
              (path-from-start-position
                ep (set-union bls nps)
                (if (empty? visited)
                    (list nps)
                    (cons nps visited))
                (new-positions ep (set-union bls nps) nps fx fy)
                fx
                fy))]))

(define (new-positions ep bls nps fx fy)
  (new-position-add-to-path ep bls nps fx fy))


(define (path-from-start-position-further ep bls visited nps)
  (path-from-start-position ep bls visited nps))



;; new-position-add-to-path: Position Block Path -> Path
;; GIVEN: a position, a block and a path
;; WHERE: 1)the position is the target position
;;        2)the visited is a path containing steps the robot has discovered 
;; RETURNS: add all new steps that can be get within one step from the
;; positons exist in given path
;; EXAMPLES: (new-position-add-to-path (list 2 1) empty 
;;                            (list (list (list 1 1) (list 1 1)))) -> 
;;           (list (list (list 1 1) (list 1 2)) 
;;                 (list (list 1 1) (list 2 1)))
;; STRATEGY: HOFC
;; (define (new-position-add-to-path ep bls visited)
;;   (foldr
;;     ;; Position Position block Path -> Path
;;     ;; GIVEN: 
;;     ;; RETURNS: 
;;    (lambda (p ans)
;;      (new-neighbor-add-for-pos p ep bls ans))
;;    visited
;;    visited))
(define (new-position-add-to-path ep bls nps fx fy)
  (foldr
    ;; Position Position block Path -> Path
    ;; GIVEN: 
    ;; RETURNS: 
   (lambda (p ans)
     (new-neighbor-add-for-pos p ep bls ans fx fy))
   empty
   nps))


;; new-neighbor-add-for-pos: Position Position Block Path -> Path
;; GIVEN: a position, another position, a block and a path
;; WHERE: 1)the first position is the start position
;;        2)the second position is the target position
;;        3)the visited is a path containing steps the robot has discovered
;; RETURNS: the given path add all new steps that is reachable within one step
;; EXAMPLES: (new-neighbor-add-for-pos 
;;             (list 1 1) (list 2 1) empty
;;             (list (list (list 1 1) (list 1 1)))) ->
;;           (list (list (list 1 1) (list 1 2)) 
;;                   (list (list 1 1) (list 2 1)))
;; STRATEGY: function composition
(define (new-neighbor-add-for-pos p ep bls nps fx fy)
  (set-union nps (new-neighbor-for-pos p ep bls)))

;; new-neighbor-for-pos: Position Position Block Path -> Path
;; WHERE: 1)the first position p is start position
;;        2)the second position is the target position
;;        3)the visited is a path containing steps the robot has discovered
;; RETURNS: a list of steps containing all the steps the robot can
;; go at the given start position
;; EXAMPLES: (new-neighbor-for-pos 
;;             (list 1 1) (list 2 1) empty
;;             (list (list (list 1 1) (list 1 1)))) ->
;;           (list (list (list 1 1) (list 1 1))
;;                 (list (list 1 1) (list 1 2))
;;                 (list (lsit 1 1) (list 2 1)))
;; STRATEGY: structural decomposition on Position
(define (new-neighbor-for-pos p ep bls)
  (foldr
   ;; Direction Path -> Path
   ;; GIVEN: a direction and a path
   ;; RETURNS: the given path add the step from the given start position
   ;; to the given direction iff the robot can go
   (lambda (d ans)
     (if (check-pos (pos-by-move p d) ep bls)
         (cons (pos-by-move p d) ans)
         ans))
   empty
   DIRECTION-LST))

;; pos-get-from-path: Position Path -> Path
;; GIVEN: a positon and a path
;; WHERE: 1)visited is the step the robot has already discovered
;;        2)the given position MUST exist as the second position of one step
;;        in given path
;; RETURNS: a step whose second position is the given position
;; EXAMPLES: (pos-get-from-path (list 1 1) (list (list (list 1 1) (list 1 1))))
;;           -> (list (list 1 1) (list 1 1))
;; STRATEGY: structural decomposition on Step
(define (pos-get-from-path pos visited)
  (foldr
   ;; Step Path -> Step
   ;; GIVEN: a position and a path
   ;; RETURNS: the given step iff its second position equals the 
   ;; given position
   (lambda (s ans)
     (if (equal? (second s) pos)
         s
         ans))
   empty
   visited))

;; plan-get-from-path-first: Position Position Path -> MaybePlan
;; GIVEN: two positions as start and target and a path
;; WHERE: 1)the first position is the start position
;;        2)the second position is the target position
;;        3)the path contains all the steps the robot can go, the path MUST
;;        contains a step as (list start start)
;; RETURNS: false iff there's no step in the path whose second position is 
;; the given target,  else a realplan from the given start position to
;; the given target position
;; EXAMPLES: (plan-get-from-path-first 
;;             (list 1 1) (list 3 3)
;;             (list (list (list 1 1) (list 1 1)))) -> false
;; STRATEGY: function composition
(define (plan-get-from-path-first sp ep path)
  (if (not (exist-pos-in-visited? ep path))
      false
      (plan-get-from-path 
       sp
       (first (pos-get-from-path ep path)) 
       path
       (list 
        (list (direction-from-step 
               (pos-get-from-path ep path)) 
              1)))))

;; plan-get-from-path-pos: Position Position Path RealPlan Step -> RealPlan
;; GIVEN: two positions, a path , a realplan and a step
;; WHERE: 1)sp is the start position
;;        2)ep is the current position
;;        3)the given path contains all the steps the robot can go
;;        4)the realplan contains the step from the current position to the 
;;        original target position
;;        5)the step is the previous step to the current position
;; RETURNS: a realplan containing moves from the first position of the given 
;;        step to the original position
;; TERMINATION ARGUMENT: the steps from the target position to the start
;;        position is always decreasing
;; HALTING MEASURE: the distance from the target position to the start position
;; EXAMPLES: (plan-get-from-path-pos
;;             (list 1 1) (list 2 1)
;;             (list (list (list 1 1) (list 1 1))
;;                   (list (list 1 1) (list 1 2))
;;                   (list (list 1 1) (list 2 1)))
;;             (list (list EAST 1))) -> (list (list EAST 1))
;; STRATEGY: structural decomposition on Step : Step
(define (plan-get-from-path-pos sp cp path plan pre-p)
  (if (equal? (first pre-p) sp)
      (plan-updated plan pre-p)
      (plan-get-from-path 
       sp 
       (first pre-p)
       path
       (plan-updated plan pre-p))))

;; plan-get-from-path: Position Position Path RealPlan -> RealPlan
;; GIVEN: two positions, a path and a realplan
;; WHERE: 1)sp is the start position
;;        2)ep is the current position
;;        3)the given path contains all the steps the robot can go
;;        4)the realplan contains the step from the current position to the 
;;        original target position
;; RETURNS: a realplan containing moves from the position one step 
;; previous to the current position to the original position
;; EXAMPLES: (plan-get-from-path
;;             (list 1 1) (list 2 1)
;;             (list (list (list 1 1) (list 1 1))
;;                   (list (list 1 1) (list 1 2))
;;                   (list (list 1 1) (list 2 1)))
;;             (list (list EAST 1))) -> (list (list EAST 1))
;; STRATEGY: function composition
(define (plan-get-from-path sp cp path plan)
  (plan-get-from-path-pos sp cp path plan (pos-get-from-path cp path)))

;; plan-updated: Plan Step -> RealPlan
;; GIVEN: a realplan and a step
;; RETURNS: a realplan containing moves from the first position of the 
;; given step to the original target
;; EXAMPLES: (plan-updated (list (list SOUTH 1))
;;                        (list (list (list 2 3) (list 2 4)))) ->
;;           (list (list SOUTH 2))
;; STRATEGY:structural decomposition on plan : RealPlan
(define (plan-updated plan s)
  (if (direction-same? (direction-from-step s) (first plan))
      (plan-updated-first-one plan)
      (new-move-add-to-plan plan (direction-from-step s))))

;; direction-same?: Direction Move -> Boolean
;; GIVEN: a direction and a move
;; RETURNS: true iff the given direction equals to the direction of the move
;; EXAMPLES: (direction-same? NORTH (list NORTH 1)) -> true
;; STRATEGY: structural decomposition on mv : Move
(define (direction-same? d mv)
  (string=? d (first mv)))

;; plan-updated-first-one: RealPlan -> RealPlan
;; GIVEN: a realplan
;; RETURNS: the first move in the given plan is updated, whose second 
;; member is added by 1
;; EXAMPLES: (plan-updated-first-one (list (list EAST 1)))
;;           -> (list (list EAST 2))
;; STRATEGY: structural decomposition on plan : RealPlan
(define (plan-updated-first-one plan)
  (cons (move-update-first-step (first plan))
        (rest plan)))

;; move-update-first-step: Move -> Move
;; GIVEN: a move
;; RETURNS: the given move updated for its second member(added by 1)
;; EXAMPLES: (move-update-first-step (list EAST 1)) -> (list EAST 2)
;; STRATEGY:structural decomposition on move : Move
(define (move-update-first-step mv)
  (list (first mv) (+ 1 (second mv))))

;; new-move-add-to-plan: RealPlan Direction -> RealPlan
;; GIVEN: a realplan and a direction
;; RETURNS: the given realplan added by a move towards the given direction with
;;         length 1 to the head
;; EXAMPLES: (new-move-add-to-plan (list (list EAST 1)) NORTH)
;;           -> (list (list NORTH 1))
;; STRATEGY: function composition
(define (new-move-add-to-plan plan d)
  (cons (list d 1) plan)) 

;; direction-from-step: Step -> Direction
;; GIVEN: a step
;; RETURNS: a direction
;; EXAMPLES: (direction-from-step (list (list 1 1) (list 1 2))) -> SOUTH 
;; STRATEGY: structural decomposition on step : Step
(define (direction-from-step s)
  (direction-by-pos (first s) (second s)))

;; direction-by-pos: Position Position -> Direction
;; GIVEN: two positions
;; WHERE: 1)the first position is a start position
;;        2)the second position is a target position 
;; RETURNS: the direction from the start position to the target position
;; EXAMPLES: (direction-by-pos (list 1 1) (list 1 2)) -> SOUTH
;; STRATEGY: structural decomposition on Position
(define (direction-by-pos p1 p2)
  (direction-by-vector 
   (list (- (first p2) (first p1)) 
         (- (second p2) (second p1)))))

;; direction-by-vector: DirectionVector -> Direction
;; GIVEN: two PosInt 
;; WHERE: 1)the first positive integer is the vector's x-value
;;        2)the secon position integer is the vector's y-value
;; RETURNS: the direction the given vector towards to
;; EXAMPLES: (direction-by-vector 1 0) -> EAST  
;; STRATEGY: sturcture decomposition on dv: DirectionVector
(define (direction-by-vector dv)
  (cond
    [(equal? dv DV-N) NORTH]
    [(equal? dv DV-S) SOUTH]
    [(equal? dv DV-E) EAST]
    [(equal? dv DV-W) WEST]))

;; pos-by-move: Position Direction -> Position
;; GIVEN: a position and a direction
;; RETURNS: the position which is a step away to the given position at the 
;; given direction 
;; EXAMPLES: (pos-by-move (list 1 1) EAST)-> (list 2 1)
;; STRATEGY:structural decomposition on d : Direction
(define (pos-by-move p d)
  (cond
    [(string=? d NORTH) (list (first p) (- (second p) 1))]
    [(string=? d SOUTH) (list (first p) (+ (second p) 1))]
    [(string=? d WEST) (list (- (first p) 1) (second p))]
    [(string=? d EAST) (list (+ (first p) 1) (second p))]))

;; exist-pos-in-visited?: Position Path -> Boolean 
;; GIVEN: a position and a path
;; RETURNS: true iff the given position is is the second position of one step
;; in the given path
;; EXAMPLES: (exist-pos-in-visited? 
;;             (list 1 1) 
;;             (list (list (list 1 1) (list 1 1)))) -> true
;; STRATEGY: structural decomposition on s : Step
(define (exist-pos-in-visited? p0 visited)
  (ormap
   ;; Step -> Boolean
   ;; GIVEN: a Step
   ;; RETURNS: true iff the given step's second position is equal to the 
   ;; the given position
   (lambda (s)
     (equal? p0 visited))
   (if (empty? visited)
       empty
       (first visited))))

;; check-pos: Position Position Block Path -> Boolean
;; GIVEN: a position, another position, a block and a path
;; WHERE: 1)the first position is the position needed to be checked
;;        2)the second position is target position
;;        3)path contains the current step the robot has discovered
;; RETURNS: true iff the position is not in the block and not visited, 
;; and also the given position should be in the reasonable frame, whose 
;; top and left edge is y=0 and x=0, right and bottom edge is calculated 
;; by function find-frame below
;; EXAMPLES: (checked (list 1 1) (list 2 1) empty
;;                    (list (list (list 1 1) (list 1 1)))) -> false
;; STRATEGY: structural decomposition on p : Position
(define (check-pos p ep bls)
  (and (> (first p) 0)
       (> (second p) 0)
       (<= (first p) (find-frame (set-cons ep bls) RIGHT-FRAME))
       (<= (second p) (find-frame (set-cons ep bls) BOTTOM-FRAME))
       ;; (<= (first p) 41)
       ;; (<= (second p) 41)
       ;; (not (exist-pos-in-visited? p visited))
       (not (my-member? p bls))))

;; find-frame: LOP RightOrBottomFrame-> PosInt
;; GIVEN: a list of positions and a frame
;; WHERE: 1)the given list positions contains a Block and the target position
;;        2)the given RightOrBottomFrame indicates which frame needed 
;;        calculating
;; RETURNS: the max y-pos in the given lop plus 1 iff calculating bottom-frame,
;; or the max x-pos in the given lop plus 1 iff the calculating right-frame
;; EXAMPLES: (find-frame (list (list 1 1) (list 2 1)) RIGHT-FRAME) -> 3
;; STRATEGY: structural decomposition on p : Position
(define (find-frame lop f)
  (+ 1 (foldr
        ;; Position PosInt -> PosInt
        ;; GIVEN: a position and the current frame
        ;; RETURNS: max one of x-pos of givne position and current frame iff for 
        ;; right-frame, or max one of y-pos of given position and 
        ;; current frame iff for bottom-frame
        (lambda (p ans)
          (find-frame-for-x-or-y f (first p) (second p) ans))
        0
        lop)))

;; find-frame-for-x-or-y: RightOrBottomFrame PosInt PosInt PosInt -> PosInt
;; GIVEN: a frame, a x-pos and y-pos and the current frame's position
;; RETURNS: max one of x-pos of given position and current frame iff for 
;; right-frame, or max one of y-pos of given position and current frame iff
;; for bottom-frame
;; EXAMPLES: (find-frame-for-x-or-y RIGHT-FRAME 15 9 10) -> 15
;; STRATEGY: structural decomposition on f: RIGHT-FRAME
(define (find-frame-for-x-or-y f x y cf)
  (cond
    [(= f RIGHT-FRAME) (max x cf)]
    [(= f BOTTOM-FRAME) (max y cf)]))

;; path: Position Position Block -> MaybePlan
;; GIVEN: two position and a block
;; WHERE: the first position is the start position
;; AND the second position is the target position
;; RETURNS: false iff there's no path from the start position to the target
;; position, or return a plan from the start position to the target position
;; EXAMPLES: (path (list 1 1) (list 2 1) empty) -> (list (list "east" 1))
;; STRATEGY: function composition
(define (path sp ep bls)
  (plan-get-from-path-first 
   sp 
   ep
   (path-from-start-position 
    ep bls
    (list (list sp sp)) 
    (list (list sp sp))
    (list (list sp sp)))))



;; set-diff : PositionSet PositionSet -> PositionSet 
;; GIVEN: two PositionSets set1 and set2
;; RETURNS: a new position set which contains the positions that only belongs 
;; to the first set 
;; EXAMPLES: (set-diff (list (list 1 2)) (list (list 1 2))) => empty
;; STRATEGY: HOFC
(define (set-diff ps1 ps2)
  (filter
   ;; Positoin -> Boolean
   ;; GIVEN: a position p
   ;; RETURNS: true iff the given position p is not in the ps2
   (lambda (p) (not (my-member? p ps2)))
   ps1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (new-position-add-to-path (list 4 4) empty (list (list (list 1 1) (list 1 1))) (list (list (list 1 1) (list 1 1))))

(define mysp (list 1 1))
;; (path-from-start-position (list 2 2) empty empty (list mysp))
;; (path (list 1 1) (list 40 40) empty)

;; (new-position-add-to-path (list 3 3) empty (list (list mysp mysp)) (list (list mysp mysp)))

;; (path (list 1 1) (list 4 4) empty)
;; (path (list 1 1) (list 8 6) block-non-reachable)

;; TESTS
;; (begin-for-test
;;   (check-equal? (real-plan? (list (list EAST 1)))
;;                 true
;;                 "should return true, but does NOT")
;;   (check-equal? (real-plan? empty)
;;                 false
;;                 "should return false, but dees NOT")
;;   (check-equal? (real-plan? (list (list "yes" 1)))
;;                 false
;;                 "should return false, but does NOT"))

;; (begin-for-test
;;   (check-equal? (path (list 1 1) (list 5 2) block-reachable)
;;                 '(("east" 1) ("south" 2) ("west" 1) 
;;                              ("south" 3) ("east" 3) ("north" 5))
;;                 "should return a path, but does NOT")
;;   (check-equal? (path (list 1 1) (list 8 6) block-non-reachable)
;;                 false
;;                 "should return a false, but does NOT"))



;; (new-positions (list 2 2) empty (list (list (list 1 1))) (list (list 1 2) (list 2 1)))

(new-positions (list 3 3) '((1 1) (1 2) (2 1)) '((1 2) (2 1)))

;; (new-positions (list 2 2) empty (list (list (list 1 1))) (list (list 1 2) (list 2 1)))

;; (path-from-start-position (list 3 3) empty empty (list (list 1 1)))






