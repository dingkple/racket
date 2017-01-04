;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname obstacles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; FILE NAME: obstacles.rkt

;; How this program works:
;; In order to realize function position-set-equal?, if two position sets are 
;; subset to each other, then the two sets are equal to each other.
;; In order to realize function obstacle?, we have to convert this problem 
;; into a graph problem. We will use positon to replace node and use adjacent
;; relationship to replace edges. Then, what we have to do is to use graph alg
;; to help us to determine whether a graph is connected graph. If it is, return
;; true; else, return false.
;; In order to realize function blocks-to-obstacles, we also have to convert 
;; this problem into graph problem. The problem is how to use an alg to
;; traversal a graph to get every connected sub graph in the original one.
;; Here, we will starting searching from a given set and then get all its 
;; successers. After that, we will compare the result set with the original
;; set. If they are the same, we are done. If not, we will continue handling 
;; the successor of the rest set which equals to the original set minus the set
;; which we have already handled.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "sets.rkt")
(require "extras.rkt")
(require rackunit)


(provide 
 position-set-equal?
 obstacle?
 blocks-to-obstacles)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Position is a (list PosInt PosInt)
;; Interp: (x y) represents the position x, y

;; TEMPLATE
;; p-fn: Position -> ??
;; (define (p-fn p)
;;   (... (first p)
;;        (second p)))

;; EXAMPLES
;; (list 1 2)

;; A ListOfPosition is either
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

;; A PositionSet is a ListOfPosition

;; EXAMPLES
;; (list (list 1 2) (list 1 3))

;; A ListOfPositionSet is either
;; -- empty
;; -- (cons PositionSet ListOfPositionSet)
;; WHERE: the ListOfPositionSet is a list of PositionSets without duplication


;; TEMPLATE
;; lops-fn : ListOfPositionSet -> ??
;; (define (lops-fn lops)
;;   [(empty? pss) ...]
;;   [else (... (lop-fn (first lops))
;;              (lops-fn (rest lops)))])

;; A PositionSetSet is a ListOfPositionSet

;; EXAMPLES
;; (list (list (list 1 2) (list 1 3))
;;       (list (list 2 3) (list 3 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples for testing

(define blocks (list (list 1 2) (list 1 3) (list 2 3) (list 3 2) 
                     (list 3 4) (list 4 1) (list 4 4)))

(define obstacles (list
                   (list (list 4 1) (list 3 4) (list 3 2) 
                         (list 2 3) (list 1 2))
                   (list (list 1 3))
                   (list (list 4 4))))

(define obstacle (list (list 4 1) (list 3 4) (list 3 2) (list 2 3) (list 1 2)))

(define set (list (list 1 2) (list 1 3) (list 2 3) (list 3 2) 
                        (list 3 4) (list 4 1) (list 4 3)))

(define result (list (list 4 1) (list 4 3) (list 3 2) (list 3 4) 
                     (list 2 3) (list 1 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; position-set-equal? : PositionSet PositionSet -> Boolean
;; GIVEN: two PositionSets
;; RETURNS: true iff they denote the same set of positions
;; EXAMPLES: see tests below
;; STRATEGY: function composition
(define (position-set-equal? ps1 ps2)
  (set-equal? ps1 ps2))


;; obstacle? : PositionSet -> Boolean
;; GIVEN: a PositionSet
;; WHERE: every position in the given position set is all occupied
;; RETURNS: true iff the set of positions would be an obstacle 
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on posset : PositoinSet
(define (obstacle? posset)
  (cond
    [(empty? posset) false]
    [else (position-set-equal? 
           (reachalbes (list (first posset)) posset)
           posset)]))

;; adjacent? : Position Position -> Boolean
;; GIVEN: two Positions p1 and p2
;; RETURNS: true iff two positions are adjacent to each other
;; EXAMPLES: (position-adjacent? (list 1 2) (list 2 3)) => true
;; STRATEGY: structural decomposition on Position 
(define (adjacent? p1 p2)
  (and (= (abs (- (first p1) (first p2))) 1)
       (= (abs (- (second p1) (second p2))) 1)))

;; adjacent-positions : Position PositionSet -> PositionSet
;; GIVEN: a Position pos and a PositionSet posset
;; RETURNS: a PositionSet which contains the positions that are adjacent to 
;; the given position
;; EXAMPLES: (adjacent-positions (list 1 2) set) => (list (list 2 3))
;; STRATEGY: HOFC
(define (adjacent-positions pos posset)
  (filter
   ;; Position -> Boolean
   ;; GIVEN: a Position p
   ;; RETURNS: true iff the position is adjacent to the given position
   (lambda (p)
     (adjacent? p pos))
   posset))

;; all-adjacent-positions : PositionSet PositionSet -> PositionSet
;; GIVEN: a PositionSet ps and another PositionSet posset
;; RETURNS: a PositionSet which contains a list of positions that are 
;; adjacent positions of the given list of positions
;; EXAMPLES: (all-adjacent-positions (list (list 1 2)) set)
;; => (list (list 2 3))
;; STRATEGY: HOFC
(define (all-adjacent-positions ps posset)
  (foldr
   ;; Position PositionSet -> PositionSet
   ;; GIVEN: a Position p and a PositionSet set-til-now
   ;; RETURNS: a PositionSet which contains a list of positions that are 
   ;; adjacent positions of the given list of positions
   (lambda (p set-til-now)
     (set-union (adjacent-positions p posset) 
                set-til-now))
   empty
   ps))

;; reachalbes : PositionSet PositionSet -> PositionSet 
;; GIVEN: a PositionSet ps and a PositionSet posset
;; RETURNS: a set of positions which are reachalbe from the given position set
;; EXAMPLES: (reachalbes (list (list 1 2)) set) => result
;; STRATEGY: general recursion
;; HALTING MEASURE: the set of positions NOT in 'ps'
;; TERMINATION ARGUMENT: At the recursive call, 'candidates' contains at
;; least one element that is not in 'PS' (otherwise the subset? test
;; would have returned true).  Hence the result of the set-union is at
;; least one element bigger than 'ps'.  So the halting measure decreases.
(define (reachalbes ps posset)
  (local
    ((define candidates (all-adjacent-positions ps posset)))
    (cond
      [(subset? candidates ps) ps]
      [else (reachalbes (set-union candidates ps)
                        posset)])))

;; blocks-to-obstacles : PositionSet -> PositionSetSet
;; GIVEN: the set of occupied positions on a chessboard
;; RETURNS: the set of obstacles on that chessboard
;; EXAMPLES: see tests below
;; STRATEGY: function composition
(define (blocks-to-obstacles posset)
  (if (empty? (set-diff posset (blocks-to-one-obstacle posset)))
      (list posset)
      (cons (blocks-to-one-obstacle posset) 
            (blocks-to-obstacles 
             (set-diff posset 
                       (blocks-to-one-obstacle posset))))))
  
;; blocks-to-one-obstacle : PositionSet -> PositionSet
;; GIVEN: a PositionSet posset 
;; RETURNS: a PositionSet which is an obstacle
;; EXAMPLES: (blocks-to-one-obstacle blocks) => obstacle
;; STRATEGY: function composition
(define (blocks-to-one-obstacle posset)
  (blocks-to-one-obstacle-inner posset empty))

;; blocks-to-one-obstacle-inner : PositionSet PositionSet -> PositionSet
;; GIVEN: a subset posset of original PositionSet and another PositionSet
;; newset
;; WHERE: newset is an obstacle converted from a subset of original position
;; set which is above the given posset
;; RETURNS: a PositionSet which is an obstacle
;; EXAMPELS: (block-to-one-obstacle-inner blocks) => obstacle
;; STRATEGY: structural decomposition on posset : PositionSet
(define (blocks-to-one-obstacle-inner posset newset)
  (cond
    [(empty? posset) newset]
    [else (if (obstacle? (set-cons (first posset) newset))
              (blocks-to-one-obstacle-inner 
                (rest posset) 
                (set-cons (first posset) newset))
              (blocks-to-one-obstacle-inner
                (rest posset)
                newset))]))


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

;; TESTS

(begin-for-test
  (check-equal? (position-set-equal? (list (list 1 2) (list 1 3)) 
                                     (list (list 1 3))) false               
                "the two position sets should be different, but they are NOT")
  (check-equal? (position-set-equal? (list (list 1 2) (list 1 3))
                                     (list (list 1 3) (list 1 2))) true
                "the two position sets should be same, but they are NOT")
  (check-equal? (obstacle? empty) false
                "the emtpy set is not an obstacle, but it DOES")
  (check-equal? (obstacle? (list (list 4 1) (list 3 2))) true
                "this position set is an obstacle, but it does NOT")
  (check-equal? (obstacle? blocks) false
                 "evety position in this position set should be adjacent to 
                 each other, but it does NOT")
  (check-equal? (blocks-to-obstacles blocks) obstacles
                "the blocks should be changed to obstacles, but it does NOT"))