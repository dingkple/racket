;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot_old) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))


;; The algorithm for this problem is DFS, the robot goes from one direction
;; until it can not go any further then it turn back to find the next way it
;; can go, until it's back to the start or hit the target position.

;; Then using the path we get, we can then find the plan from the start to the 
;; target position

(require "extras.rkt")
(require rackunit)
(require "sets.rkt")

(provide path)

(define NORTH "north")
(define SOUTH "south")
(define EAST "east")
(define WEST "west")

(define RIGHT-FRAME 1)
(define BOTTOM-FRAME -1)


(define MV-E (list EAST 1))
(define MV-S (list SOUTH 1))
(define MV-W (list WEST 1))
(define MV-N (list NORTH 1))

(define DV-N (list 0 -1))
(define DV-S (list 0 1))
(define DV-W (list -1 0))
(define DV-E (list 1 0))


(define DIRECTION-LST (list EAST SOUTH WEST NORTH))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITION

;; A Position is a (list PosInt PosInt)
;; (x y) represents the position x, y.
;; Note: this is not to be confused with the built-in data type Posn.
;; Template:
;; pos-fn : Position -> ??
#;(define (pos-fn p)
    (...
     (first p)
     (second p)))
;; Examples:
(define pos-1-1 '(1 1))
(define s-5-5 '(1 1))
(define t-5-5 '(4 2))
(define s-5-5-2 '(3 1))
(define t-5-5-2 '(1 5))
(define s-10-10 '(1 1))
(define t-10-10 '(10 10))
(define s-circle '(1 1))
(define t-circle '(6 4))

;; A ListOf<Position> is one of
;; -- empty
;; -- (cons Position ListOf<Position>)
;; Template:
;; lop-fn : ListOf<Position> -> ??


;; TEMPLATE
;; p-fn: Position -> ??
;; (define (p-fn p)
;;   (... (first p)
;;        (second p)))

;; A ListOfPosition is one of
;; -- empty
;; -- (cons Position ListOfPosition)
;; WHERE: the ListOfPosition is a list of positions without duplication

;; TEMPLATE
;; lop-fn : ListOfPosition -> ??
;; (define (lop-fn lop)
;;   (cond
;;     [(empty? lop) ...]
;;     [else (... (p-fn (first lop))
;;                (lop-fn (rest lop)))]))

;; A Block is a ListOfPosition
;; Interp: Block represents the list of positions which robot can not 
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

;; A MaybePlan is one of:
;; -- false
;; -- Plan
;; Interp:
;; false iff there's not a way from the start position to the target position
;; RealPlan as interpreted before, representing there's a plan containing at
;; least one step

;; TEMPLATE
;; mp-fn : MaybePlan -> ??
;; (define (mp-fn mp)
;;   (cond
;;     [(false? mp) ...]
;;     [(list? mp) (...
;;                   (lom-fn mp))])) 


;; A Square is a (list LOD Position)
;; Interp: the first member is a list of directions, representing the
;; directions the next move can towards to; the second member is 

;; TEMPLATE:
;; s-fn : Square -> ??
;; (define (s-fn s)
;;   (... (d-fn (first s))
;;        (p-fn (second s))))

;; A ListOfSquare is one of:
;; -- empty
;; -- (cons Square ListOfSquare)

;; TEMPLATE: 
;; los-fn : ListOfSquare -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los) empty]
;;     [else (...(s-fn (first los))
;;               (los-fn (rest los)))]))

;; A Path is a ListOfSquare


;; A AllInfo is a (list Position Plan Path)
;; Interp: the first member represents the position, 
;; the second member represents the plan
;; the third member represents the path

;; TEMPLATE
;; ai-fn : AllInfo -> ??
;; (define (ai-fn ai)
;;    (... (p-fn (first ai))
;;         (lom-fn (second ai))
;;         (los-fn (third ai))))

;; A DirectionVector is one of
;; -- (list 0 1)      interp: represents the south direction
;; -- (list 1 0)      interp: represents the east direction 
;; -- (list 0 -1)     interp: represents the north direction
;; -- (list -1 0)     interp: represents the west direction

;; TEMPLATE:
;; dv-fn : DirectionVector -> ??
;; (define (dv-fn dv)
;;   (cond
;;     [(equals? dv DV-E) ...]
;;     [(equals? dv DV-N) ...]
;;     [(equals? dv DV-W) ...]
;;     [(equals? dv DV-S) ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples for testing

(define block-reachable 
  (list (list 4 1) (list 4 2) (list 4 3) (list 4 4)
        (list 1 2) (list 2 2) (list 3 5)))

(define block-non-reachable 
  (list (list 4 1) (list 4 2) (list 4 3) (list 4 4)
        (list 1 2) (list 2 2) (list 3 2) (list 3 5)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path : Position Position ListOf<Position> -> Maybe<Plan>>
;; GIVEN:
;; 1. the starting position of the robot,
;; 2. the target position that robot is supposed to reach
;; 3. A list of the blocks on the board
;; RETURNS: a plan that, when executed, will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks, or false if no such sequence of moves exists.
;; EXAMPLES: (path (list 1 1) (list 2 1) empty) -> (list ("east" 1))
;; STRATEGY: function composition
(define (path sp ep bls)
  (move-get-from-path-prepare (prepare-to-find-path sp ep bls)))

;; prepare-to-find-path: Position Position LOP -> Path
;; GIVEN: a start position, an end position and a list of positions as
;;    blocks
;; RETURNS: a list of continuous squares from the start position to
;;    the end position iff there exists a way, or return an empty list
;; EXAMPLES: (prepare-to-find-path (list 1 1) (list 2 1) empty)
          ;; -> '((() (1 1)) (() (2 1)))
;; STRATEGY: function composition
(define (prepare-to-find-path sp ep bls)
  (findpath sp ep bls (list (gather-info sp ep bls empty))))

;; findpath: Position Position Path -> a Path
;; GIVEN: a start position, an end position and a list of positions as
;;    blocks
;; WHERE: the Path is the list of squares from the original start point
;;    to the current start point
;; RETURNS: a list of continuous squares from the start position to
;;    the end position iff there exists a way, or return an empty list
;; EXAMPLES: 
;; STRATEGY: 
(define (findpath sp ep bls path)
  (if (equal? sp ep)
      path
      (try-to-find ep bls (last path) path)))

;; try-to-find: Position LOP Square Path -> Path
;; GIVEN: a start position, an end position and a list of positions as
;;    blocks
;; WHERE: the Path is the list of squares from the original start point
;;    to the current start point
;; RETURNS: a list of continuous squares from the start position to
;;    the end position iff there exists a way, or return an empty list
;; EXAMPLES: 
;; STRATEGY:
(define (try-to-find ep bls lsq path)
  (if (nowhere-to-go? lsq)
      (go-back-to-find ep bls path)
      (go-on-current-way ep bls path)))

;; nowhere-to-go: Square -> Boolean
;; GIVEN: a square
;; RETURNS: true iff the first element of the given square is not empty
;; EXAMPLES: 
;; STRATEGY: 
(define (nowhere-to-go? lsq)
  (empty? (first lsq)))

;; go-back-to-find: Position LOP Path -> Path
;; GIVEN: an end position, a list of positions as blocks and a list of squares
;; WHERE: 1)the position is the end position to stop
;;        2)the given list of positions is the blocks the robot can not pass
;;     through
;;        3)the list of squares if the square from the original start point to
;;     the robot's position
;; RETURNS: a list of squares from the original position to the given end
;;     position iff there exists a way, else empty
;; EXAMPLES: 
;; STRATEGY: 
(define (go-back-to-find ep bls path)
  (go-on-current-way ep bls (find-last-way ep bls path)))
  

;; find-last-way: Position LOP Path -> Path
;; GIVEN: a target position, a list of position and a path
;; WHERE: 1)the position is the target position
;;        2)the list of position is a block
;;        3)the path is the path the robot is go now
;; RETURNS: the path from the start position to the target position iff exists
;;     else return empty
;; EXAMPLES: (find (list 2 1) empty (list (list "east south") (list 1 1)))
;;             -> (list (list "east" 1))
;; STRATEGY: structure decomposition on Path
(define (find-last-way ep bls path)
  (foldr
     (lambda (s ans)
       (if (and (should-turn? s) (equal? s (last ans)))
           (drop-last ans)
           ;; (stay-or-find-new-way p ep bls ans)))
           ans))
     path
     path))

;; should-turn? : Square -> Boolean
;; GIVEN: a square
;; RETURNS: true iff there's no way to go(the direction part is empty)
;; EXAMPLES: (should-turn? (list empty (list 1 1))) -> true
;; STRATEGY: structure decomposition on Square
(define (should-turn? s)
  (empty? (first s)))



;; go-this-direction: Position Square LOP Path -> Path
;; GIVEN: a positions, a square, a list of position and a path
;; WHERE: 1)the position is the target position
;;        2)the list of position is a block
;;        3)the path is the path the robot is go now
;; RETURNS: the path from the start position to the target position iff exists
;;     else return empty
;; EXAMPLES: (go-this-direction (list 2 1) empty (list (list "east south") (list 1 1)))
;;             -> (list (list "east" 1))
;; STRATEGY: structure decomposition on Square
;; (define (go-this-direction ep sq bls path)
;;   (findpath 
;;    (pos-by-move (second sq) (direction-first (first sq)))
;;    ep
;;    bls
;;    (path-one-step-further ep path sq bls)))


;; direction-first: LOD -> direction
;; GIVEN: a list of directions
;; RETURNS: the first element iff the list is not empty
;; EXAMPLES: see tests below
;; STRATEGY: structure decomposition on LOD
(define (direction-first ld)
  (cond
    [(empty? ld) empty]
    [else (first ld)]))


;; go-on-current-way: Position Block Path -> Path
;; GIVEN: a position, a list of positions and a path
;; WHERE: 1)the positions is the target positions
;;        2)the path is the path from the start position to current position
;; RETURNS:  the path updated by one step further
;; EXAMPLES: (find (list 2 1) empty (list (list "east south") (list 1 1)))
;;             -> (list (list "east" 1))
;; STRATEGY: function composition
(define (go-on-current-way ep bls path)
  (if (empty? path)
      path
      (findpath 
       (pos-by-move-from-square (last path))
       ep
       bls 
       (path-for-further ep bls path))))

;; path-for-further: Position Path Square LOP -> 
;; GIVEN: a position, a path, a square and a list of positions
;; WHERE: 1)the position is the target position
;;        2)the path is the path from the start position to current position
;;        3)the square is the current square the robot is on, and there's some
;;       directions the robot can go
;;        4)bls is the given block
;; RETURNS: the path updated by one step further
;; EXAMPLES: (path-for-further 
;;               (list 1 2) 
;;               '(((EAST SOUTH) (1 1))) 
;;               ((EAST SOUTH) (1 1))
;;               empty) -> '(((SOUTH) (1 1)) ((SOUTH EAST) (1 2)))
;; STRATEGY: structure decomposition on Square
(define (path-for-further ep bls path)
  (append 
   (update-path-direction (last path) (drop-last path)) 
   (list 
    (gather-info 
     (pos-by-move-from-square (last path)) 
     ep bls path))))

;; square-directions: Square -> LOD
;; GIVEN: a square
;; RETURNS: the direction part of the square
;; EXAMPLES: (list (list EAST) (list 1 1)) -> (list EAST)
;; STRATEGY: structure decomposition on Square
(define (square-directions sq)
  (first sq))


;; square-directions: Square -> LOD
;; GIVEN: a square
;; RETURNS: the direction part of the square
;; EXAMPLES: (list (list EAST) (list 1 1)) -> (list 1 1)
;; STRATEGY: structure decomposition on Square
(define (square-pos sq)
  (second sq))

;; update-path-direction: Square Path -> Path
;; GIVEN: a square and a path
;; WHERE: the given square's direction part is not empty
;; RETURNS: the udpated path(append the given updated square to the path)
;; EXAMPLES: (update-path-direction '((EAST SOUTH) (2 1)) '(((EAST) (1 1))))
;;               -> '(((EAST) (1 1)) '((SOUTH) (2 1)))
;; STRATEGY: structure decomposition on LOD
(define (update-path-direction sq path)
  (append path (list (list (rest (square-directions sq)) (second sq)))))

;; pos-by-move-from-square: Square -> Position
;; GIVEN: a square
;; WHERE: the direction part is not empty
;; RETURNS: the position goes one more step along the first direction of the 
;;    given square from the position of the given square
;; EXAMPLES: (pos-by-move-from-square '((EAST) (1 1))) -> (2 1)
;; STRATEGY: structure decomposition on LOD
(define (pos-by-move-from-square sq)
  (pos-by-move (second sq) (first (square-directions sq))))


;; last: List -> Any
;; GIVEN: a list of anything
;; RETURNS: the last element iff it's not empty or empty
;; EXAMPLES: (last (last 1 2)) -> 2
;; STRATEGY: structure decomposition on List
(define (last lst)
  (cond
    [(empty? lst) empty]
    [else (first (reverse lst))]))

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
(define (check-pos p ep bls path)
  (and (> (first p) 0)
       (> (second p) 0)
       (<= (first p) (find-frame (set-cons ep bls) RIGHT-FRAME))
       (<= (second p) (find-frame (set-cons ep bls) BOTTOM-FRAME))
       (not (exists-in-path? p path))
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

;; exist-in-path?: Position Path -> Boolean 
;; GIVEN: a position and a path
;; RETURNS: true iff the given position is is the second position of a square
;;   in the given path
;; EXAMPLES: (exist-in-path? 
;;             (list 1 1) 
;;             (list (list (list EAST) (list 1 1)))) -> true
;; STRATEGY: structural decomposition on s : Square
(define (exists-in-path? p0 path)
  (ormap
   (lambda (sq)
     (equal? p0 (second sq)))
   path))

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

;; gather-info: Position Position LOP Path -> Square
;; GIVEN: two position, a list of positions and a path
;; WHERE: 1)the first position is the start position
;;        2)the second position is the target position
;;        3)the lop is the block
;;        4)the path is the path from the start position to current position
;; RETURNS: a square of the given position, direction part is all the directions
;;   the robot can go
;; EXAMPLES: (list (list 1 2) (list 3 3) empty '(((EAST) (1 1)))) ->
;;               '((EAST SOUTH) (1 1))
;; STRATEGY: structural decomposition on Square
(define (gather-info sp ep bls path)
  (foldr
   (lambda (d ans)
     (if (check-pos (pos-by-move sp d) ep bls path)
         (list (cons d (first ans)) (second ans))
         ans))
   (list empty sp)
   DIRECTION-LST))

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

(define (move-get-from-path-prepare path)
  (if (empty? path)
      false
      (move-get-from-path path)))

;; first-plan-from-path: Path -> Plan
;; GIVEN: a path
;; WHERE: the path is not empty
;; RETURNS: a plan containing the first move
;; EXAMPLES: (list (list (list EAST) (list 1 1))) -> (list (list EAST 1))
;; STRATEGY: structure decomposition on Square
(define (first-plan-from-path sq1 sq2)
  (list (direction-by-pos (second sq1) 
                                (second sq2))))

;; move-get-from-path: Path -> MaybePlan
;; GIVEN: a Path from the start to the target
;; RETURNS: false iff there's no way else the plan from the start to the target
;; EXAMPLES: (move-get-from-path empty) -> false
;; STRATEGY: structure decomposition on Path
(define (move-get-from-path path)
  (if (empty? path)
      false
      (second
        (all-info-get-from-path
          (second (second path))
          (list (first-plan-from-path (first path) (second path)))
          (rest (rest path))))))


;; all-info-get-from-path: Position Plan Path -> Plan
;; GIVEN: a positions, a plan and a path
;; WHERE: 1)the positions is the current start position
;;        2)the plan contains the plan from the original start position to the 
;;   current start position
;;        3)path contains the path from the original start position to the 
;;   original target position
;; RETURNS: the plan from the start to the target
;; EXAMPLES: (list (list 1 2) (list (list EAST 1)) (list ))
;; STRATEGY: structure decomposition on AllInfo
(define (all-info-get-from-path sp plan path)
  (foldl
    (lambda (sq ans)
      (if (going-old-direction? (first ans) (second ans) (third ans))
          (move-along-ori-direction sq (second ans) (third ans))
          (move-along-new-direction (first ans) sq (second ans) (third ans))))
    (list sp plan path)
    path))


;; going-old-direction?: Position Plan Path ->  
;; GIVEN: a position ,a plan and a path
;; WHERE: 1)the position the current start position
;; ;;     2)the plan contains the plan from the original start position to the 
;;        3)the path is a path from the start to the end
;; RETURNS: true iff the next move is same direction as the robot is going now
;; EXAMPLES: (going-old-direction? 
;;               (list 1 2) 
;;               (list (list EAST 1)) 
;;               (list (list empty (1 1)) (list empty (1 2)) (list empty 1 3)))
;;           -> true
;; STRATEGY: structure decomposition on Path
(define (going-old-direction? sp plan path)
  (string=? 
    (direction-by-pos sp 
      (square-pos (first path))) 
      (move-dirction (last plan))))


;; move-dirction: Move -> Direction
;; GIVEN: a move
;; RETURNS: the direction of the move
;; EXAMPLES: (move-dirction (list EAST 1)) -> EAST
;; STRATEGY: structure decomposition on Move
(define (move-dirction mv)
  (first mv))

;; move-length: Move -> Direction
;; GIVEN: a move
;; RETURNS: the direction of the move
;; EXAMPLES: (move-length (list EAST 1)) -> 1
;; STRATEGY: structure decomposition on Move
(define (move-length mv)
  (second mv))

;; move-along-ori-direction: RealPlan Path -> Realplan 
;; WHERE:  1)the position is the current position, not the original one
;;         2)the realplan is from the original start point to current start point
;;         3)the path is from the next square to the end position
;; RETURNS: a realplan from the original position to the original end position
;; EXAMPLES: 
;; STRATEGY: structure decomposition on path
(define (move-along-ori-direction sq rp path)
  (list (second sq) (one-step-to-move rp) (rest path)))

;; move-along-new-direction: Position Path -> RealPlan
;; GIVEN: a start position, a realplan and a path
;; WHERE:  1)the position is the current position, not the original one
;;         2)the realplan is from the original start point to current start point
;;         3)the path is from the next square to the end position
;; RETURNS: a realplan from the original position to the original end position
;; EXAMPLES: 
;; STRATEGY: 
(define (move-along-new-direction last sq plan path)
  (list 
    (second sq) 
    (add-new-move plan (direction-by-pos last (second sq)))
    (rest path)))

;; one-step-to-move: Plan -> Plan
;; GIVEN: a plan from the original start position to the previous position
;; RETURNS: the given plan whose last move is udpated by adding 1 to the second
;;     element of the move
;; EXAMPLES: (one-step-to-move (list ("east 1"))) -> (list ("east" 2))
;; STRATEGY: structure decomposition on Move
(define (one-step-to-move mvs)
  (append 
   (drop-last mvs)
   (list (list (first (last mvs)) (+ 1 (second (last mvs)))))))

;; drop-last: List -> List 
;; GIVEN: a list of anything
;; RETURNS: empty iff the given list is empty, else the given list except the 
;;     last element of the list is deleted
;; EXAMPLES: (drop-last (list a 1)) -> (list a)
;; STRATEGY: structure decomposition on lst: List
(define (drop-last lst)
  (cond
    [(empty? lst) empty]
    [else (reverse (rest (reverse lst)))]))
  
;; add-new-move: Plan Move -> Plan
;; GIVEN: a plan and  a move
;; RETURNS: a new plan
;; EXAMPLES: (list empty (list EAST 1)) -> (list (list EAST 1))
;; STRATEGY: function composition
(define (add-new-move mvs mv)
  (append mvs (list (list mv 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS:
;; (begin-for-test
;;   (check-equal? (path s-5-5 t-5-5 block-5-5) plan-5-5
;;                 "Find a path from s-5-5 to t-5-5 is plan 5-5.")
;;   (check-equal? (path s-5-5-2 t-5-5-2 block-5-5) plan-false
;;                 "There's no path from s-5-5-2 to t-5-5-2.")
;;   (check-equal? (path s-10-10 t-10-10 block-10-10) plan-10-10
;;                 "Find a path from s-10-10 to t-10-10 is plan-10-10.")
;;   (check-equal? (path s-circle t-circle block-circle) plan-false
;;                 "There's no path from (1 1) to (6 4)."))

(begin-for-test
  (check-equal? (path (list 1 1) (list 5 2) block-reachable)
                '(("east" 2) ("south" 3) ("west" 1) ("south" 2) ("east" 4) 
                  ("north" 1) ("west" 1) ("north" 1) ("east" 1) ("north" 1)
                  ("west" 1) ("north" 1))
                "should return a path, but does NOT")

  (check-equal? (path (list 1 1) (list 8 6) block-non-reachable)
                false
                "should return a false, but does NOT"))



(path (list 1 1) (list 1 2) empty)





















