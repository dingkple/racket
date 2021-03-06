;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname river) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;;INTRO:
;;the algorithm for this problem is BFS, and before starting to get the plan
;;we add a procedure of choosing the pitchers that needed, proved by
;;Extended Euclidean algorithm, let gcd(a, b) be the greatest common divisor
;;iff c = k*gcd(a,b) (k is a positive integer), then we can use pitchers with 
;;capacities:a and b to get amount of c. In case the goal is greater than
;;(max a b), we can add a bigger pitcher to store the water. If no such pair
;;(a, b) can be found, or no pitcher can store the given amount of water, 
;;just return false.

;;Once we choose the pitchers we need, we use bfs to get the plan, we get all
;;the possible new status that can be reach within one operation from the 
;;current set of status, and iff the goal have NOT appeared, we continue, 
;;until one pitcher contains the given amount of water.


(require "extras.rkt") 
(require rackunit)
(require "sets.rkt")

(provide 
 list-to-pitchers
 pitchers-to-list
 pitchers-after-moves
 make-move
 move-src
 move-tgt
 make-fill
 make-dump
 fill-pitcher
 dump-pitcher
 solution
 move?
 fill?
 dump?)

;;DATA DEFINITIONS

;; PosInt means a set of Positive Integers
;; NonNegInt means is set of Non negative Integer

;; A ListOf<NonNegInt> is
;;-- empty ; an empty list
;;-- (cons NonNegInt ListOf<NonNegInt>) 
; NonNegInt added to the ListOf<NonNegInt>

;;Template:
;; loni-fn: ListOf<NonNegInt> -> ??
;;(define (loni-fn lstni)
;;  (cond
;;    [(empty? lstni) ...]
;;    [else (...
;;           (first lstni)
;;           (loni-fn (rest lstni)))]))

;; a NELONI(non-empty list of NonNegInt) is:
;;  --(cons NonNegInt ListOf<NonNegInt>) 
; a NonNegInt added to list of NonNegInt

;; Template:
;; neloni-fn: NELONI -> ??
;;(define (neloni-fn nelstni)
;;  (... (first nelstni)
;;       (neloni-fn (rest nelstni))))


;; A ListOf<PosInt> is
;;-- empty ; an empty list
;;-- (cons PosInt ListOf<PosInt>) ; PosInt added to the ListOf<PosInt>

;;Template:
;; lopi-fn: ListOf<PosInt> -> ??
;;(define (lopi-fn lstpi)
;;  (cond
;;    [(empty? lstpi) ...]
;;    [else (...
;;           (first lstpi)
;;           (lopi-fn (rest lstpi)))]))

;; a NELOPI(non-empty list of PosInt) is:
;;  --(cons PosInt ListOf<PosInt>) 

;; Template:
;; nelopi-fn: NELOPI -> ??
;;(define (nelopi-fn nelstpi)
;;  (... (first nelstpi)
;;       (nelopi-fn (rest nelstpi))))

;; SinglePitcherExternalRep is a (list PosInt NonNegInt)
;; INTERP:
;; the first PosInt represents the capacity of the pitcher
;; the second NonNegInt represents the current amount of contents 
;; in the pitcher
;; WHERE: 0 <= contents <= capacity
;; Template:
;; sp-fn: SinglePitcherExternalRep -> ??
;;(define (sp-fn sp)
;;  (... (first sp)
;;       (second sp)))

;; A ListOf<SinglePitcherExternalRep> (LOSPER) is
;;-- empty ; an empty list
;;-- (cons SinglePitcherExternalRep LOSPER) 
; SinglePitcherExternalRep added to the LOSPER

;;Template:
;; lstsinglepxr-fn: LOSPER -> ??
;;(define (lstsinglepxr-fn sepr)
;;  (cond
;;    [(empty? sepr) ...]
;;    [else (...
;;           (sp-fn (first sepr))
;;           (lstsinglepxr-fn (rest sepr)))]))

;; PitchersExternalRep is one of:
;;  --(cons SinglePitcherExternalRep LOSPER)

;; INTERPRETATION: the list of pitchers, from 1 to n, with their contents
;; and capacity and for each i, 0 <= contents_i <= capacity_i
;; EXAMPLE: ((10 5) (8 7)) is a list of two pitchers. The first has
;; capacity 10 and currently holds 5; the second has capacity 8 and
;; currently holds 7.
;; Template:
;; per-fn: PitchersExternalRep -> ??
;;(define (per-fn per)
;;  (... (sp-fn (first per))
;;       (lstsinglepxr-fn (rest per))))


(define-struct move (src tgt))
;; A Move is a (make-move PosInt PosInt)
;; WHERE: src and tgt are different
;; INTERP: (make-move i j) means pour from pitcher i to pitcher j.
;; 'pitcher i' refers to the i-th pitcher in the PitchersExternalRep.
;; 'pitcher j' refers to the j-th pitcher in the PitchersExternalRep.
;; Template:
;; move-fn: Move -> ??
;;(define (move-fn m)
;;  (... (move-src m)
;;       (move-tgt m)))

(define-struct dump (pitcher))
;; A Dump is a (make-dump PosInt)
;; INTERP:
;; (make-dump i) means dump all the water from pitcher i.
;;  'pitcher i' refers to the i-th pitcher in the PitchersExternalRep.
;; Template:
;; dump-fn: Dump -> ??
;;(define (dump-fn p)
;;  (... (dump-pitcher p)))

(define-struct fill (pitcher))
;; A Fill is a (make-fill PosInt)
;; INTERP:
;; (make-fill i) means fill all the water to pitcher i.
;;  'pitcher i' refers to the i-th pitcher in the PitchersExternalRep.
;; Template:
;; fill-fn: Fill -> ??
;;(define (fill-fn p)
;;  (... (fill-pitcher p)))

;; An Operation is one of:
;;  --(make-move PosInt PosInt)
;;  --(make-dump PosInt)
;;  --(make-fill PosInt)
;; INTERP:
;; an Operation is one of the operations on pitchers: move, dump and fill
;; Template:
;; op-fn: Operation -> ??
;;(define (op-fn o)
;;  (cond
;;    [(move? o) (move-fn o)]
;;    [(dump? o) (dump-fn o)]
;;    [(fill? o) (fill-fn o)]))

;; a Change is one of:
;; --(list NELONI '() NELONI)
;; --(list NELONI Operation NELONI)
;; INTERP:
;; the first NELONI is the original status of a list of pitchers
;; the Operation represents the operation to be executed on the pitchers
;; the last NELONI is the status of the pitchers after executing the operation
;; iff the second part is empty, means this is initial change, nothing is
;; changed, the first NELONI is the same as the second NELONI
;; Template:
;; ch-fn: Change -> ??
;; (define (ch-fn c)
;;   (... (first c)
;;        (second c)
;;        (third c)))

;; a LOC is list of Changes, which is one of:
;;  --empty ; an empty list
;;  --(cons Change LOC) ; a change added to LOC
;; Template:
;; loc-fn: LOC -> ??
;;(define (loc-fn loc)
;;  (cond
;;    [(empty? loc) ...]
;;    [else (... (first loc)
;;               (loc-fn (rest loc)))]))

(define-struct pitchersInternalRep (pitchers status))
;; a PitchersInternalRep is a (make-pitchersInternalRep NELOPI NELONI)
;; INTERP:
;; pitchers is the list of is the capacities of the pitchers, 
;; and status  is the list of current amount of contents in each of
;; the pitchers.
;; A pitchersInternalRep is generated from the PitchersExternalRep, 
;; and the members in the list keep the same order from it.
;; Template:
;; pir-fn: PitchersInternalRep -> ??
;; (define (pir-fn pir)
;;   (... (nelopi-fn (pitchersInternalRep-pitchers pir)
;;        (neloni-fn (pitchersInternalRep-status pir))))


;; a Plan is list of Operations, it's one of
;;  --empty ; an empty list
;;  --(cons Operation Plan) ; an operation added to Plan
;; Template:
;; pl-fn: Plan -> ??
;; (define (pl-fn p)
;;   (cond
;;     [(empty? p) ...]
;;     [else (...(op-fn (first p))
;;               (pl-fn (rest p)))]))

;; a MaybePlan is one of:
;;  --false
;;  --Plan
;; INTERP:
;;  false representing it's impossible to make one of the pitchers having
;;  goal amount of water
;;  plan representing the steps to make at least one of the pitcher contain
;;  goal amount of water
;; Template:
;; mp-fn: MaybePlan -> ??
;; (define (mp-fn mp)
;;   (cond
;;     [(false? mp) ...]
;;     [(list? mp) ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EXAMPLES FOR TESTS:

(define pitchers-external-example1
  '((10 0) (7 0) (3 0)))

(define pitchersInternalRep-example1
  (make-pitchersInternalRep '(10 7 3) '(0 0 0)))

(define pitchersInternalRep-example2
  (make-pitchersInternalRep '(10 7 3) '(10 5 0)))

(define plan-example1
  (list
   (make-fill 2)
   (make-move 2 1)
   (make-fill 2)
   (make-move 2 1)
   (make-dump 1)
   (make-move 2 1)
   (make-fill 2)
   (make-move 2 1)
   (make-dump 1)
   (make-move 2 1)
   (make-fill 2)
   (make-move 2 1)
   (make-fill 2)
   (make-move 2 1)))

(define pitchers-example1 '(10 7 3))
(define pitchers-example1-empty '(0 0 0))

(define loc-example1 
  '(((10 0 0) () (10 0 0)) 
    ((10 0 0) (make-move 1 2) (3 7 0))
    ((10 0 0) (make-move 1 3) (7 0 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-to-pitchers: PitchersExternalRep -> PitchersInternalRep
;; GIVEN: a PitchersExternalRep
;; RETURNS: a pitchersInternalRep generated from the given PitchersExternalRep
;; EXAMPLES: (list-to-pitchers '((10 8) (7 3))) -> 
;;                 (make-pitchersInternalRep '(10 7) '(8 3))
;; STRATEGY: function composition
(define (list-to-pitchers pit-ext-rep)
  (make-pitchersInternalRep 
   (get-pitchers-from-ext pit-ext-rep)
   (get-status-from-ext pit-ext-rep)))

;; pitchers-to-list: PitchersInternalRep -> PitchersExternalRep
;; GIVEN: An internal representation of a set of pitchers
;; RETURNS: a PitchersExternalRep generated from the pitchersInternalRep, 
;;      the members are kept in same order
;; EXAMPLES: (pitchers-to-list (make-pitchersInternalRep '(10 9) '(0 0))) 
;;                  ->'((10 0) (9 0))
;; STRATEGY: structure decomposition on pitcher : PitchersInternalRep
(define (pitchers-to-list pitcher)
  (reverse
   (pitcher-to-external 
    (pitchersInternalRep-pitchers pitcher) 
    (pitchersInternalRep-status pitcher))))

;; pitcher-to-external: NELOPI NELONI -> PitchersExternalRep
;; GIVEN: pitcher is the list of capacities of some given pitchers
;; and status is a list of current amount of water in them
;; RETURNS: a PitchersExternalRep generated from the given pitchers and status
;;       the members of PitchersExternalRep keep the same order
;; EXAMPLES: (pitcher-to-external '(10 9) '(0 0)) -> '((10 0) (9 0))
;; STRATEGY: HOFC
(define (pitcher-to-external pitcher status)
  (foldl
   ;; PosInt PitchersExternalRep -> PitchersExternalRep
   ;; GIVEN: a pitcher's capacity and a PitchersExternalRep 
   ;; RETURNS: a SinglePitcherExternalRep of the given pitcher added to
   ;;     the head of the given PitchersExternalRep
   (lambda (p ans)
     (cons (single-pitcher-generate pitcher status (length ans)) ans))
   empty
   pitcher))

;; single-pitcher-generate: NELOPI NELONI PosInt -> SinglePitcherExternalRep
;; GIVEN: 1)the first list of PosInt is the pitchers's capacities
;;        2)the second list of NonNegInt is the pitcher's current
;;     amount of water
;;        3)n is no less than 0 and less than the length of given pitchers
;; RETURNS: a single pitcher external reperesentation with capacity and
;; content taken from the nth index in the list of pitcher and status 
;; respectively
;; EXAMPLES: 
;; (single-pitcher-generate '(10 9) '(0 0) 0) -> '(10 0)
;; (single-pitcher-generate '(10 9) '(0 0) 1) -> '(9 0)
;; STRATEGY: function composition
(define (single-pitcher-generate pitcher status n)
  (list (list-ref pitcher n) (list-ref status n)))

;; get-status-from-ext: PitchersExternalRep -> NELONI
;; GIVEN: a PitchersExternalRep 
;; RETURNS: a list of NonNegInt representing the amount of current  
;;     water in the pitchers extracted from the given PitchersExternalRep
;; EXAMPLES: (get-status-from-ext '((10 9) (9 0))) -> '(9 0)
;; STRATEGY: structure decomposition on p: SinglePitcherExternalRep
(define (get-status-from-ext pit-ext-rep)
  (map
   ;; SinglePitcherExternalRep -> NonNegInt
   ;; GIVEN: a SinglePitcherExternalRep, representing a pitcher
   ;; RETURNS: the amount of water in the given pitcher
   (lambda (p)
     (second p))
   pit-ext-rep))

;; get-pitchers-from-ext: PitchersExternalRep -> NELOPI
;; GIVEN: a PitchersExternalRep
;; RETURNS: a list of PosInt representing capacities of pitchers 
;; extracted from the given PitchersExternalRep
;; EXAMPLES: (get-pitchers-from-ext '((10 9) (8 0))) -> '(10 8)
;; STRATEGY: structure decomposition on p: SinglePitcherExternalRep
(define (get-pitchers-from-ext pit-ext-rep)
  (map
   ;; SinglePitcherExternalRep -> PosInt
   ;; GIVEN: a SinglePitcherExternalRep, representing a pitcher
   ;; RETURNS: the capacity of the given pitcher
   (lambda (p)
     (first p))
   pit-ext-rep))


;; pitchers-after-moves : PitchersInternalRep ListOf<Move> 
;;                            -> PitchersInternalRep
;; GIVEN: An internal representation of a set of pitchers, and a sequence
;; of moves
;; WHERE: every move refers only to pitchers that are in the set of pitchers.
;; RETURNS: the internal representation of the set of pitchers that should
;; result after executing the given list of moves, in order, on the given
;; set of pitchers.
;; EXAMPLES: (pitchers-after-moves 
;;             (make-pitchersInternalRep '(10 9) '(10 0)) '(make-move 1 2))
;;           -> (make-pitchersInternalRep '(10 9) '(1 9))
;; STRATEGY: structure decomposition on pits: PitchersInternalRep
(define (pitchers-after-moves pits oplst)
  (make-pitchersInternalRep 
   (pitchersInternalRep-pitchers pits)
   (status-after-move 
    (pitchersInternalRep-pitchers pits) 
    (pitchersInternalRep-status pits)
    oplst)))

;; status-after-move: NELOPI NELONI Plan -> NELONI
;; GIVEN: 
;; 1)the first list of PosInt is the capacities of some give pitchers
;; 2)the second list of NonNegInt represents current amount of water
;;   in given pitchers
;; 3) a plan(list of Operation)
;; RETURNS: the status of the given pitchers after executing the given plan
;; EXAMPLES: 
;; (status-after-move '(10 7) '(10 0) (list (make-move 1 2))) -> '(3 7)
;; (status-after-move '(10 7) '(10 0) (list (make-move 1 2) (make-dump 1)))
;;                                    -> '(0 7)
;; STRATEGY: HOFC
(define (status-after-move pits sta oplst)
  (foldl
   ;; Operation NELONI -> NELONI
   ;; GIVEN: a operation and a status of the pitchers
   ;; RETURNS: the status of the pitchers after the given operation
   (lambda (op ans)
     (status-after-single-operation pits ans op))
   sta
   oplst))

;; status-after-single-operation: NELOPI NELONI Operation -> NELONI
;; GIVEN:
;; 1)the first list of PosInt is the capacities of some give pitchers
;; 2)the second list of NonNegInt represents current amount of water
;;   in given pitchers
;; 3)op is a operation
;; RETURNS: the status of the pitchers after the given operation
;; EXAMPLES: (status-after-single-operation '(10 7) '(0 0) (make-fill 1))
;;               -> '(10 0)
;; STRATEGY: structure decomposition on op : Operation
(define (status-after-single-operation pitchers s-now op)
  (cond
    [(move? op) 
     (water-pour-get-info pitchers s-now (move-src op) (move-tgt op))]
    [(dump? op)
     (src-pitcher-dump s-now (dump-pitcher op))]
    [(fill? op)
     (src-pitcher-fill pitchers s-now (fill-pitcher op))]))

;; water-pour-get-info: NELOPI NELONI PosInt PosInt -> NELONI
;; GIVEN: 
;;1)the first list of PosInt are capacities of some given pitchers
;;2)the second list of NonNegInt represents current amount of water
;;   in given pitchers
;;3)the src is the pitcher to pour water out
;;4)the tgt is the pitcher to pour water in
;; RETURNS: the status of the pitchers after pour water from pitcher src to 
;;       pitcher tgt
;; EXAMPLES: (water-pour-get-info '(10 9) '(10 0) 1 2) -> '(3 7)
;; STRATEGY: function composition
(define (water-pour-get-info pitcaps pitstas src tgt)
  (if (and (<= src (length pitcaps)) (<= tgt (length pitcaps)))
      (water-pour-to-pits
       (list-ref pitcaps (- tgt 1))
       (list-ref pitstas (- src 1))
       (list-ref pitstas (- tgt 1))
       pitstas
       src tgt)
      empty))

;; water-pour-to-pits: PosInt PosInt PosInt NELONI PosInt PosInt -> NELONI
;; GIVEN: 1)tc is the target pitcher's capacity
;;        2)ss is the source pitcher's current status(amount of water)
;;        3)ts is the target pitcher's current status(amount of water)
;;        4)pitstas is a status (list of current amount of 
;;          water in some given pitchers)
;;        5)src refers to the source pitcher in some list of pitchers
;;        6)tgt refers to the target pitcher in some list of pitchers
;; RETURNS: the status of the list of pitchers after pouring the water in
;;     the given pitcher to the target pitcher
;; EXAMPLES: (water-pour-get-info 7 10 0 '(10 0) 1 2) -> '(3 7)
;; STRATEGY: HOFC
(define (water-pour-to-pits tc ss ts pitstas src tgt)
  (reverse
   (foldl
    ;; PosInt NELONI -> NELONI
    ;; GIVEN: a pitcher's current amount of water, and a list of pitchers'
    ;;     status already processed
    ;; RETURNS: iff the given pitcher is the src then pour its water out
    ;;     according to the target pitcher's capacity and its current amount
    ;;     of water else iff the given pitcher is the target pitcher, 
    ;;     receive the water from the src pitcher, else just put the 
    ;;     pitcher's status to the head of the given list.
    (lambda (ps ans)
      (cond
        [(= (length ans) (- src 1)) (cons (src-pour ss tc ts) ans)]
        [(= (length ans) (- tgt 1)) (cons (tgt-recieve ss tc ts) ans)]
        [else (cons ps ans)]))
    empty
    pitstas)))

;; src-pour: PosInt PosInt PosInt -> PosInt
;; WHERE: 1)ss the source pitcher's status(amount of water)
;;        2)tc is the target pitcher's capacity
;;        3)ts is the target pitcher status
;; RETURNS: the source pitcher's status after pouring water to the target
;;     position
;; EXAMPLES: (src-pour 10 7 0) -> 3
;; STRATEGY: function composition
(define (src-pour ss tc ts)
  (if (< (+ ss ts) tc)
      0
      (- (+ ts ss) tc)))


;; tgt-recieve: PosInt PosInt PosInt -> PosInt
;; WHERE: 1)ss is the source pitcher's status(current amount of water)
;;        2)tc is the target pitcher's capacity
;;        3)ts is the target pitcher's status(current amount of water)
;; RETURNS: the target's status after pouring water in
;; EXAMPLES: (tgt-recieve 10 7 3) -> 7
;; STRATEGY: function composition
(define (tgt-recieve ss tc ts)
  (if (> (+ ss ts) tc)
      tc
      (+ ss ts)))

;; findmove: NELOPI PosInt LOC LOC -> LOC
;; WHERE: 1)pitchers is the capacities of some given pitchers
;;        2)goal is the amount of water needed in one of the pitchers
;;        3)clst is the changes of the previous operations
;;        4)new-clst is the new changes generated from operating on the 
;;     new-clst in last findmove
;;        5)for the given pitchers, it's sure that after certain
;;     steps, the goal can be found
;; RETURNS: a list of changes from the original status to the final status:
;;     any pitcher having amount of given goal water or empty if it's 
;;     impossible to get one pitcher having that amount of water
;; HALTING MEASURE: the number of possible different kinds of pitchers's  
;;      status have NOT appeared in the new-clst
;; TERMINATION ARGUMENT: the number of different status of pitchers is 
;;     limited every time we will discover new status and add them in clst
;; EXAMPLES: 
;; (findmove '(10 7) 3 '(((0 0) () (0 0))) '(((0 0) () (0 0)))) ->
;;   (list
;;    (list (list 0 0) empty (list 0 0))
;;    (list (list 0 0) (make-fill 1) (list 10 0))
;;    (list (list 0 0) (make-fill 2) (list 0 7))
;;    (list (list 10 0) (make-move 1 2) (list 3 7))
;;    (list (list 0 7) (make-fill 1) (list 10 7))
;;    (list (list 0 7) (make-move 2 1) (list 7 0)))
;; STRATEGY: general recursion
(define (findmove pitchers goal clst new-clst)
  (local
    ((define all-clst (my-set-union clst new-clst)))
    (cond
      [(exists-in-status? goal clst) clst]
      [else (findmove
             pitchers goal all-clst
             (water-pour pitchers all-clst new-clst))])))

;; water-pour: NELOPI LOC LOC -> LOC
;; WHERE: 1)pitcher is the capacities of some given pitchers
;;        2)clst is the changes of the previous operations
;;        3)new-clst is the new changes generated from operating on the 
;;     new-clst in last findmove
;; RETURNS: new status of the pitchers after excuting operations on the 
;;      status appears as the third part of one of the changes in the given
;;      new-clst
;; EXAMPLES: (water-pour '(10 7) '(((0 0) () (0 0))) '(((0 0) () (0 0)))) ->
;;             (list 
;;              (list (list 0 0) (make-fill 1) (list 10 0)) 
;;              (list (list 0 0) (make-fill 2) (list 0 7)))        
;; STRATEGY: structure decomposition on c : Change
(define (water-pour pitcher clst new-clst)
  (foldr
   ;; Change LOC -> LOC
   ;; GIVEN: a change and a list of changes
   ;; RETURNS: all the new changes can be generated from the result status 
   ;;     of the given changes
   (lambda (c ans)
     (operations-union pitcher (my-set-union ans clst) ans (third c)))
   empty
   new-clst))

;; operations-union: NELOPI LOC LOC NELONI -> LOC
;; WHERE: 1)pitcher is the capacities of the pitchers
;;        2)clst is the changes of the previous operations
;;        3)new-clst is the new changes generated from operating on the 
;;     new-clst in last findmove
;;        4)s-now is the current status of the pitchers(amount of water)
;; RETURNS: all the new possible status generated by the operations: 
;;          move, fill, dump
;; EXAMPLES: 
;; (operations-union '(10 7) '(((0 0) () (0 0))) '(((0 0) () (0 0))) '(0 0))
;; ->
;;  (list 
;;   (list (list 0 0) (make-fill 1) (list 10 0)) 
;;   (list (list 0 0) (make-fill 2) (list 0 7)) 
;;   (list (list 0 0) empty (list 0 0)))
;; STRATEGY: function composition
(define (operations-union pitcher clst new-clst s-now)
  (my-set-union
   (fill-every-possible pitcher clst s-now 1)
   (my-set-union
    (dump-every-possible pitcher clst s-now 1)
    (my-set-union
     (pour-every-possible pitcher clst s-now 1 1)
     new-clst))))

;; dump-every-possible: NELOPI LOC NELONI PosInt -> LOC
;; WHERE: 1)pitcher is the capacities of the pitchers
;;        2)clst is the changes of the previous operations
;;        3)s-now is the current status of the pitchers(amount of water)
;;        4)src refers to the pitcher in the given list to empty
;; RETURNS: all the possible new changes generated by empty one of the pitcher
;;       in the given list
;; HALTING MESURE: (length pitchers) - (src - 1), src refers to the position 
;;        of the given pitcher in the original list of pitchers
;; TERMINATION ARGUMENT: the number of different status that can be get from 
;;        current status is limited, and since we only keep the status not 
;;        not appeared before, so it will halt eventually
;; EXAMPLES: 
;; (dump-every-possible '(10 7) '(((0 0) () (0 0))) '(0 0) 1) -> empty
;; (dump-every-possible '(10 7) '(((0 0) () (10 7))) '(10 7) 1) ->
;;      (list (list (list 10 7) (make-dump 1) (list 0 7)) 
;;            (list (list 10 7) (make-dump 2) (list 10 0)))
;; STRATEGY: general recursion
(define (dump-every-possible pitcher clst s-now src)
  (local 
    ((define pour-src (src-pitcher-dump s-now src))
     (define new-mv-after-dump 
       (new-operation-to-list (make-dump src) s-now pour-src)))
    (cond
      [(> src (length pitcher)) empty]
      [(exists-before? pour-src clst)
       (dump-every-possible pitcher clst s-now (+ src 1))]
      [else 
       (cons
        new-mv-after-dump
        (dump-every-possible pitcher clst s-now (+ src 1)))])))

;; new-operation-to-list: Operation NELONI NELONI -> Change
;; GIVEN: 
;; 1) An operation 
;; 2) s-before is the status of the pitchers before excuting the 
;;    given operation
;; 3) s-now is the status of the pitchers after executing the given
;;    operation
;; RETURNS: a change using the given information
;; EXAMPLES: 
;; (new-operation-to-list (make-move 1 2) '(10 0) '(3 7)) ->
;;               (list (list 10 0) (make-move 1 2) (list 3 7))
;; (new-operation-to-list (make-dump 1) '(10 7) '(0 7))  ->
;;               (list (list 10 7) (make-dump 1) (list 0 7))
;; STRATEGY: function composition
(define (new-operation-to-list op s-before s-now)
  (list s-before op s-now))

;; fill-every-possible: NELOPI LOC NELONI PosInt -> LOC
;; WHERE: 1)pitcher is the capacities of the pitchers
;;        2)clst is the changes of the previous operations
;;        3)s-now is the status of the pitchers after executing the given
;;     operation
;;        4)src refers to the source pitcher in the list of pitchers
;; RETURNS: a list of new changes after executing (make-fill i) in which:
;;          src<=i<=(length pitcher)
;; HALTING MESURE: (length pitcher) - (src - 1), src refers 
;;  to the position of the given pitcher in the original list of pitchers
;; TERMINATION ARGUMENT: the number of new changes from the given status is 
;;       limited, so it will halt eventually when src > (length pitcher)
;; EXAMPLES: (fill-every-possible 
;;             '(10 7) '(((0 0) () (0 0))) 
;;             '(0 0) 1) ->
;;             (list (list (list 0 0) (make-fill 1) (list 10 0)) 
;;                   (list (list 0 0) (make-fill 2) (list 0 7)))
;; STRATEGY: general recursion
(define (fill-every-possible pitcher clst s-now src)
  (local 
    ((define fill-src (src-pitcher-fill pitcher s-now src))
     (define new-mv-after-fill 
       (new-operation-to-list (make-fill src) s-now fill-src)))
    (cond
      [(> src (length pitcher)) empty]
      [(exists-before? fill-src clst)
       (fill-every-possible pitcher clst s-now (+ src 1))]
      [else 
       (cons
        new-mv-after-fill
        (fill-every-possible pitcher clst s-now (+ src 1)))])))

;; src-pitcher-dump: NELONI PosInt -> NELONI
;; WHERE: 1)the current status of the pitchers
;;        2)src refers to the pitcher's index in the list of given pitchers
;;          (i.e. given pitchers as '(10 7 3) then src = 1 mean refering
;;           to pitcher 1 in the list , similarly src= 3 means refering 
;;           to pitcher 3 in the list)
;; RETURNS: the status of the pitchers after dumping all the water  
;;     from the given pitcher = src
;; EXAMPLES: 
;; (src-pitcher-dump '(10 7) 1) -> '(0 7)
;; (src-pitcher-dump '(10 7) 2) -> '(10 0)
;; STRATEGY: HOFC
(define (src-pitcher-dump s-now src)
  (reverse
   (foldl
    ;; PosInt NELONI -> NELONI
    ;; GIVEN: a pitcher's status and a list of pitchers's status pocessed
    ;; RETURNS: iff the given pitcher is the src pitcher, dump all the water
    ;;     in it, else add it to the head of the given list 
    (lambda (s ans)
      (if (= (length ans) (- src 1))
          (cons 0 ans)
          (cons s ans)))
    empty
    s-now)))

;; src-pitcher-fill: NELOPI NELONI PosInt -> NELONI
;; WHERE: 1)pitchers is he capacities of the pitchers
;;        2)s-now is the current status of the pitchers
;;        3)src refers to the pitcher's index in the list of given pitchers
;;          (i.e. given pitchers as '(10 7 3) then src = 1 mean refering
;;           to pitcher 1 in the list , similarly src= 3 means refering 
;;           to pitcher 3 in the list)
;; RETURNS: the status of the pitchers after filling full water to the given 
;;     pitcher = src
;; EXAMPLES: 
;; (src-pitcher-fill '(10 7) '(0 0) 1) -> '(10 0)
;; (src-pitcher-fill '(10 7) '(10 0) 2) -> '(10 7)
;; STRATEGY: HOFC
(define (src-pitcher-fill pitcher s-now src)
  (reverse
   (foldl 
    ;; PosInt NELONI -> NELONI
    ;; GIVEN: a pitcher's status and a list of pitchers's status pocessed
    ;; RETURNS: iff the given pitcher is the src pitcher, fill all the water
    ;;     in it, else add it to the head of the given list 
    (lambda (s ans)
      (if (= (length ans) (- src 1))
          (cons (list-ref pitcher (- src 1)) ans)
          (cons s ans)))
    empty
    s-now)))

;; pour-every-possible: NELOPI LOC NELONI PosInt PosInt -> LOC
;; WHERE: 1)pitchers is the capacities of the pitchers
;;        2)clst is the changes of the previous operations
;;        3)s-now is the status of the pitchers after executing the given
;;     operation
;;        4)src refers to the source pitcher in the list of pitchers
;;        5)tgt refers to the target pitcher in the list of pitchers
;; RETURNS: a list of new changes after excuting (make-move i j), in which
;;     (tgt<=j<=(length pitchers) and i=src) or (1<=j<=(length pitchers and
;;     i>src), and of cource i != j
;; HALTING MESURE: (length pitchers) - (src - 1), src refers to the position 
;;        of the given pitcher in the original list of pitchers
;; TERMINATION ARGUMENT: different kinds of pour operations between pitchers
;;     are limited, every time the number of undiscovered status decreases
;;     and function will halt when src > (length pitchers)
;; EXAMPLES:
;; (pour-every-possible  '(10 7) '(((0 0) () (0 0))) '(0 0) 1 1) -> empty
;; (pour-every-possible  '(10 7) '(((0 0) (make-fill 1) (10 0))) '(10 0) 1 1)
;;   -> (list (list (list 10 0) (make-move 1 2) (list 3 7))) 
;; (pour-every-possible  '(10 7) '(((0 0) (make-fill 2) (0 7))) '(0 7) 1 3)
;;   -> (list (list (list 0 7) (make-move 2 1) (list 7 0)))
;; STRATEGY: general recursion
(define (pour-every-possible pitchers clst s-now src tgt)
  (local 
    ((define next-status (water-pour-get-info pitchers s-now src tgt))
     (define new-mv-after-pour-from-src-to-tgt
       (new-operation-to-list (make-move src tgt) s-now next-status)))
    (cond
      [(> src (length pitchers)) empty]
      [(> tgt (length pitchers))
       (pour-every-possible pitchers clst s-now (+ src 1) 1)]
      [(or 
        (= src tgt) 
        (exists-before? next-status clst))
       (pour-every-possible pitchers clst s-now src (+ tgt 1))]
      [else 
       (cons
        new-mv-after-pour-from-src-to-tgt 
        (pour-every-possible pitchers clst s-now src (+ tgt 1)))])))

;; exists-in-status?: PosInt LOC -> Boolean
;; WHERE: 1)goal is the amount of water needed in one of the pitchers
;;        2)clst is the list of changes generated in the operations
;; RETURNS: true iff the goal has appeared in one of the result of the changes
;; EXAMPLES: (exists-in-status? 10 '(((10 0) () (10 0)))) -> true
;; STRATEGY: structure decomposition on c : Change
(define (exists-in-status? goal clst)
  (ormap
   ;; Change -> Boolean
   ;; GIVEN: a change
   ;; RETURNS: true if the there's a pitcher in the change's result status 
   ;;    that contains the given amount of water
   (lambda (c)
     (check-status? goal (third c)))
   clst))

;; check-status?: PosInt NELONI -> Boolean
;; WHERE: 1)goal is the amount of water needed in one of the pitchers
;;        2)status is the status of the pitchers now(amount of water in them)
;; RETURNS: true iff one of the pitchers having given amount of water
;; EXAMPLES: (check-status? 5 '(10 5)) -> true
;; STRATEGY: HOFC
(define (check-status? goal status)
  (ormap
   ;; PosInt -> Boolean
   ;; GIVEN: a pitcher's status(amount of water in it)
   ;; RETURNS: true iff the pitcher contains given amount of water
   (lambda (s)
     (equal? goal s))
   status))

;; exists-before: NELONI LOC -> Boolean
;; WHERE: 1)ps the status of the pitchers
;;        2)clst is the changes generated by previous operations
;; RETURNS: true iff there's one change's result is the given status
;; EXAMPLES: (exists-before? '(10 0) '(((10 0) () (10 0)))) -> #t
;; STRATEGY: structure decomposition on c : Change
(define (exists-before? ps clst)
  (ormap
   ;; Change -> Boolean
   ;; GIVEN: a change
   ;; RETURNS: true iff the given change's result status equals the given
   ;;     status
   (lambda (c)
     (equal? (third c) ps))
   clst))

;; get-goal-pitcher-from-clst: PosInt LOC -> Change
;; WHERE: 1)goal is the amount of the water needed in one of the pitchers
;;        2)clst is the changes generated by previous operations
;; RETURNS: one of the changes 
;; EXAMPLES: (get-goal-pitcher-from-clst 0 '(((10 0) () (10 0))) ->
;;               '((10 0) () (10 0))
;; STRATEGY: structure decomposition on change-having-goal : Change
(define (get-goal-pitcher-from-clst goal clst)
  (local
    ((define change-having-goal
       (filter
        ;; Change -> Boolean
        ;; GIVEN: a change
        ;; RETURNS: true iff the given change's result status shows that one
        ;;      of the pitchers contains the given amount of water
        ;; STRATEGY: structure decomposition on c : Change
        (lambda (c)
          (check-status? goal (third c)))
        clst)))
    (cond
      [(empty? change-having-goal) empty]
      [else (first change-having-goal)])))

;; get-result-from-change: Change -> NELONI
;; GIVEN: a change
;; RETURNS: the result of the move: the third part of the move
;; EXAMPLES: (get-result-from-change '(((10 0) () (10 0))) -> '(10 0)
;; STRATEGY: structure decomposition on c : Change
(define (get-result-from-change c)
  (third c))

;; move-from-list: NELONI LOC -> Change
;; WHERE: 1)status is the status of the pitchers
;;        2)clst is the changes generated by previous operations
;; RETURNS: a change in the given list whose result is given status
;; EXAMPLES: (move-from-list '(10 0) '(((10 0) () (10 0)))) -> 
;;             '((10 0) () (10 0))
;; STRATEGY: structure decomposition on filter-status : LOC
(define (move-from-list status clst)
  (local
    ((define filter-status
       (filter
        ;; Change -> Boolean
        ;; GIVEN: a change
        ;; RETURNS: true iff the given change's result status equals the given
        ;;     status
        ;; STATEGY: structure decomposition on c : Change
        (lambda (c)
          (equal? (third c) status))
        clst)))
    (cond
      [(empty? filter-status) empty]
      [else (first filter-status)])))

;; plan-from-list: NELONI NELONI LOC Plan -> Plan
;; GIVEN: 
;; 1)the ini-status is the start status of the pitchers
;; 2)the now-s is the current status of the pitchers
;; 3)clst is the changes generated by previous operations
;; 4)plan is a list of moves that can change the current status to the
;;   target status(one of the pitchers containing the needed amount of water)
;; WHERE: ini-status and now-s are present in main 
;;        clst(list returned from findmove) 
;; RETURNS: a plan that can change the ini-status to the target status
;; HALTING MEASURE: the halting measure is the number of the operations
;;     needed from the ini-status to the target status(which contains goal)
;; TERMINATION ARGUMENT: when now-s = ini-status return the plan found
;;                       (since now-s and ini-status are both present in
;;                        main clst, condition will satisfy for sure and
;;                        function will return)
;; EXAMPLES: (plan-from-list 
;;               '(0 0) 
;;               '(3 7) 
;;               '(((0 0) empty (0 0))
;;                 ((0 0) (make-fill 1) (10 0))
;;                 ((0 0) (make-fill 2) (0 7))
;;                 ((10 0)(make-move 1 2) (3 7))
;;                 ((0 7) (make-fill 1) (10 7))
;;                 ((0 7) (make-move 2 1) (7 0))) 
;;                empty) -> (list (list 'make-fill 1) (list 'make-move 1 2))
;; STRATEGY: general recursion
(define (plan-from-list ini-status now-s clst plan)
  (if (equal? now-s ini-status)
      plan
      (plan-from-list 
       ini-status 
       (ini-status-from-change (move-from-list now-s clst))
       clst
       (cons (second (move-from-list now-s clst)) plan))))

;; ini-status-from-change: Change -> NELONI
;; GIVEN: a Change
;; RETURNS: the initial status of the given change
;; EXAMPLES: (ini-status-from-change '((10 0) () (10 0))) -> '(10 0) 
;; STRATEGY: structure decomposition on c : Change
(define (ini-status-from-change c)
  (first c))

;; ini-status-generate: NELOPI -> NELONI
;; WHERE: 1)pitchers is the capacities of the pitchers
;; RETURNS: a status of the pitchers which just fill the first pitcher and
;;     leave all the other pitchers empty
;; EXAMPLES: (ini-status-generate '(10 7)) -> '(10 0)
;; STRATEGY: structure decomposition on pitchers : NELOPI
(define (ini-status-generate pitchers)
  (make-list (length pitchers) 0))

;; initial-list: NELOPI -> Change
;; WHERE: 1)pitchers is the capacities of the pitchers
;; RETURNS: a change that having an empty operation having no effect on the
;;     initial status of the pitchers
;; EXAMPLES: (initial-list '(10 9)) -> '(((10 0) () (10 0)))
;; STRATEGY: function composition
(define (initial-list pitchers)
  (list
   (list 
    (ini-status-generate pitchers) 
    '() 
    (ini-status-generate pitchers))))

;; find-temp-solution: NELOPI PosInt -> Plan
;; WHERE: 1)pitchers is the capacities of the pitchers
;;        2)goal is the amount needed in one of the pitchers
;;        3)and it's sure that after certain steps, goal can be found in 
;;      one of the pitchers
;; RETURNS: a plan that make one of the pitchers containing the given amount
;;     of water if possible or return false iff impossible
;; EXAMPLES: (find-temp-solution '(10 7) 7) -> '((make-fill 2))
;; STRATEGY: function composition
(define (find-temp-solution pitchers goal)
  (local
    ((define movelist 
       (findmove pitchers 
                 goal (initial-list pitchers) (initial-list pitchers))))
    (plan-from-list 
     (ini-status-generate pitchers)
     (get-result-from-change
      (get-goal-pitcher-from-clst goal movelist)) 
     movelist
     empty)))

;; my-set-union: LOC LOC -> LOC
;; GIVEN: two lists of changes
;; RETURNS: s1 unions s2 by their member's result status
;; EXAMPLES: (my-set-union 
;;             '(((10 0 0) () (10 0 0)) ((10 0 0) (make-move 1 2) (3 7 0)))
;;             '(((10 0 0) () (10 0 0))))
;;          -> '(((10 0 0) () (10 0 0)) ((10 0 0) (make-move 1 2) (3 7 0)))
;; STRATEGY: structure decomposition on c : Change
(define (my-set-union s1 s2)
  (foldr
   ;; Change LOC -> LOC
   ;; GIVEN: a change and a list of changes
   ;; RETURNS: iff the result status in the given change have NOT appeared as 
   ;;     a result status in the given list of changes, add it to the given
   ;;     list, or just return the given list
   (lambda (c ans)
     (if (exists-before? (third c) s2)
         ans
         (cons c ans)))
   s2
   s1))

;; operation-pitchers: NELOPI PosInt PosInt PosInt -> NELONI
;; WHERE: 1)pitchers is the capacities of the pitchers
;;        2)goal is the amount needed in one of the pitchers
;;        3)p1 refers to i-th pitcher in the given list of pitchers
;;        4)p2 refers to j-th pitcher in the given list of pitchers where j>=i
;; RETURNS: 1)iff the length of pitchers is greater than 1, return a pair of 
;;        pitchers that can generate the goal amount of water after certain 
;;        steps, iff no such pair, return empty
;;          2)iff the given list of pitchers is empty, return empty
;; HALTING MESURE: the number of pairs that have NOT been checked in
;;                 the given list of pitchers
;; TERMINATION ARGUMENT: the pair of pitchers is limited, every time we will 
;;        check one and count also decreases by one.
;; EXAMPLES: 
;; (operation-pitchers '(10) 4 1 1) -> empty
;; (operation-pitchers '(8 5) 4 1 1) -> (list 1 2)
;; STRATEGY: general recursion
(define (operation-pitchers pitchers goal p1 p2)
  (local
    ((define common-mod 
       (if (and (<= p1 (length pitchers)) (<= p2 (length pitchers)))
           (gcd
            (list-ref pitchers (- p1 1)) 
            (list-ref pitchers (- p2 1)))
           0)))
    (cond
      [(> p1 (length pitchers)) empty]
      [(> p2 (length pitchers)) 
       (operation-pitchers pitchers goal (+ p1 1) (+ p1 1))]
      [(and (not (= p1 p2)) (= (modulo goal common-mod) 0)) (list p1 p2)]
      [else (operation-pitchers pitchers goal p1 (+ p2 1))])))

;; add-bigger-one: NELOPI NELONI PosInt -> NELONI
;; WHERE: 1)pitchers is the capacities of the pitchers
;;        2)extracted is either: a)empty b)a pair of pitchers c)a list having 
;;       one pitcher whose capacity is the goal
;;        3)goal is the amount needed in one of the pitchers 
;; RETURNS: 1)iff either pitcher in extracted has a greater capacity than 
;;      goal, return it
;;          2)else iff there'a pitcher in the list whose capacity is greater 
;;      than goal, add it to the extracted
;;          3)else return empty
;; EXAMPLES: (add-bigger-one '(10 5) '(10 5) 15) -> empty
;; STRATEGY: function composition
(define (add-bigger-one pitchers extracted goal)
  (if (<= goal 
          (max 
           (list-ref pitchers (- (first extracted) 1)) 
           (list-ref pitchers (- (second extracted) 1))))
      extracted
      (find-bigger-one pitchers extracted goal 1)))

;; find-bigger-one: NELOPI LONI PosInt PosInt -> LONI
;; WHERE: 1)pitchers is the capacities of the pitchers
;;        2)extracted is either: a)empty b)a pair of pitchers
;;        3)goal is the amount needed in one of the pitchers
;;        4)p0 refers to the i-th pitcher in the given list
;; RETURNS: empty iff from p0, there's no pitchers having the capacity bigger 
;;      than goal, else append the first eligible pitcher to extracted
;; HALTING MESURE: (length - (p0 - 1))
;; TERMINATION ARGUMENT: the number of pitchers is limited, every time we 
;;      check one
;; EXAMPLES: (find-bigger-one '(10 7 15) '(10 7) 12 1) -> '(10 7 15)
;; STRATEGY: general recursion
(define (find-bigger-one pitchers extracted goal p0)
  (cond
    [(> p0 (length pitchers)) empty]
    [(> (list-ref pitchers (- p0 1)) goal) (append extracted (list p0))]
    [else (find-bigger-one pitchers extracted goal (+ p0 1))]))

;; extract-pitchers: NELOPI PosInt -> LONI
;; WHERE: 1)pitchers is the capacities of the pitchers
;;        2)goal is the amount needed in one of the pitchers
;; RETURNS: a list of pitchers containing 2 or 3 pitchers that can get the 
;;       amount of water
;;       after certain steps. Or empty 
;; EXAMPLES: (extract-pitchers '(10 7) 15) -> empty
;; STRATEGY: function composition
(define (extract-pitchers pitchers goal)
  (local
    ((define extracted (operation-pitchers pitchers goal 1 1)))
    (if (empty? extracted)
        empty
        (add-bigger-one pitchers extracted goal))))

;; caps-of-extracted: NELOPI NELOPI -> NELOPI
;; WHERE: 1)pitchers is the capacities of the pitchers
;;        2)extracted is either: a)a pair of pitchers b)a list having one
;;       pitcher whose capacity is the goal
;; RETURNS: a list of capacities of the pitchers in extracted(in order)
;; EXAMPLES: (caps-of-extracted '(10 7) '(1 2)) -> '(10 7)
;; STRATEGY: HOFC
(define (caps-of-extracted pitchers extracted)
  (foldr
   ;;PosInt NELOPI -> NELOPI
   ;;GIVEN : a PosInt and LOPI constructed
   ;;RETURNS: a list of PosInts(i.e capacities of the pitchers in extracted)
   (lambda (p ans)
     (cons (list-ref pitchers (- p 1)) ans))
   empty
   extracted))

;; transform-operations: Plan NELOPI -> Plan
;; WHERE: 1)a plan that make one of the pitchers containing the given amount
;;     of water if possible, the i-th pitcher refers to the position of 
;;     pitcher in extracted
;;        2)extracted is a list of pitchers containing 2 or 3 pitchers that
;;     can get the amount of water after certain steps.
;; RETURNS: a plan that make one of the pitchers containing the given amount
;;     of water if possible, the i-th pitcher refers to the position of
;;     pitcher in original list of pitchers
;; EXAMPLES: (transform-operations empty '(1 2)) -> empty
;; STRATEGY: HOFC
(define (transform-operations plan extracted)
  (map
   ;;Operation -> Operation
   ;;GIVEN : an Operation
   ;;RETURNS: an Operation modified such that the i-th pitcher refers 
   ;;     to the position of pitcher in original list of pitchers
   (lambda (m)
     (transform-single-operation m extracted))
   plan))

;; transform-single-operation: Operation LOPI -> Operation
;; WHERE: 1)pitchers in operation refers to the position in extracted
;;         2)extracted is a list of pitchers contaitransform-single-operation
;;       after certain steps.
;; RETURNS: an operation that transformed the position of the pitchers in 
;;      original list of pitchers
;; EXAMPLES: (transform-single-operation (make-move 1 2) '(2 4))
;;           -> (make-move 2 4)
;; STRATEGY: structure decomposition on m : Operation
(define (transform-single-operation m extracted)
  (cond
    [(move? m) (transform-pour m extracted)]
    [(fill? m) (transform-fill m extracted)]
    [(dump? m) (transform-dump m extracted)]))


;; transform-pour: Operation LOPI -> Move
;; WHERE: 1)pitchers in operation refers to the position in extracted
;;         2)extracted is a list of pitchers containing 2 or 3 pitchers that 
;;      can get the amount of water after certain steps.
;; RETURNS: a move that transformed the position of the pitchers in original
;;      list of pitchers
;; EXAMPLES: (transform-pour (make-move 1 2) '(2 4)) -> (make-move 2 4)
;; STRATEGY: structure decomposition on m: Move
(define (transform-pour m extracted)
  (make-move 
   (list-ref extracted (- (move-src m) 1)) 
   (list-ref extracted (- (move-tgt m) 1))))


;; transform-fill: Operation LOPI -> Fill
;; WHERE: 1)pitchers in operation refers to the position in extracted
;;         2)extracted is a list of pitchers containing 2 or 3 pitchers that 
;;       can get the amount of water after certain steps.
;; RETURNS: a fill that transformed the position of the pitchers in original 
;;       list of pitchers
;; EXAMPLES: (transform-fill (make-fill 1) '(2 4)) -> (make-fill 2)
;; STRATEGY: structure decomposition on m : Fill
(define (transform-fill m extracted)
  (make-fill (list-ref extracted (- (fill-pitcher m) 1))))


;; transform-dump: Operation LOPI -> Dump
;; WHERE: 1)pitchers in operation refers to the position in extracted
;;         2)extracted is a list of pitchers containing 2 or 3 pitchers that
;;       can get the amount of water
;;       after certain steps.
;; RETURNS: a dump that transformed the position of the pitchers in original
;;       list of pitchers
;; EXAMPLES: (transform-dump (make-dump 1) '(2 4)) -> (make-dump 2)
;; STRATEGY: structure decomposition on m : Dump
(define (transform-dump m extracted)
  (make-dump (list-ref extracted (- (dump-pitcher m) 1))))

;; solution: NELOPI PosInt -> MaybePlan
;; WHERE: 1)pitchers is the capacities of the pitchers
;;        2)goal is the amount needed in one of the pitchers
;; RETURNS: a plan that make one of the pitchers containing the given amount
;;     of water if possible or return false iff impossible
;; EXAMPLES: (solution '(10 7) 7) -> '((make-fill 2))
;; STRATEGY: function composition
(define (solution pitchers goal)
  (local
    ((define real-pitchers 
       (cond
         [(empty? pitchers) empty]
         [(and (= (length pitchers) 1) (= (first pitchers) goal)) pitchers]
         [(= (length pitchers) 1) empty]
         [else (extract-pitchers pitchers goal)])))
    ;;find the solution
    (if (empty? real-pitchers) 
        false
        (if (= (length real-pitchers) 1) 
            (list (make-fill 1))
            (find-real-solution 
             (caps-of-extracted pitchers real-pitchers) 
             real-pitchers goal)))))

;; find-real-solution: NELOPI NELONI PosInt  -> MaybePlan
;; WHERE: 1)pitchers is the capacities of the pitchers
;;        2)goal is the amount needed in one of the pitchers
;; RETURNS: a plan that make one of the pitchers containing the given amount
;;     of water if possible or return false iff impossible
;; EXAMPLES: (find-real-solution '(10 7) 7) -> '((make-fill 2))
;; STRATEGY: function composition
(define (find-real-solution pitchers extracted goal)
  (transform-operations (find-temp-solution pitchers goal) extracted))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS

(begin-for-test
  (check-equal? (list-to-pitchers pitchers-external-example1)
                pitchersInternalRep-example1
                "should transform to pitchersInternalRep")
  (check-equal? (pitchers-to-list pitchersInternalRep-example1)
                pitchers-external-example1
                "should transform to external-rep")
  (check-equal? (pitchers-after-moves 
                 pitchersInternalRep-example1 
                 plan-example1)
                pitchersInternalRep-example2
                "should change from the start status to target")
  (check-equal? (solution pitchers-example1 5)
                plan-example1
                "should give a plan but does NOT")
  (check-equal? (solution pitchers-example1 50)
                false
                "should give a false")
  (check-equal? (get-goal-pitcher-from-clst 50 loc-example1)
                empty
                "should give an empty")
  (check-equal? (solution '(3 6 9) 2)
                false
                "should give a false")
  (check-equal? (solution '(10) 10)
                (list (make-fill 1))
                "should give a plan")
  (check-equal? (solution '(10) 5)
                false
                "should give a false")
  (check-equal? (move-from-list '(10 0 0 0) loc-example1)
                empty
                "should give an empty")
  (check-equal? (solution empty 5)
                false
                "should give a false")
  (check-equal? (solution '(10 7 15) 12)
                (list (make-fill 3)
                      (make-move 3 1)
                      (make-fill 2) 
                      (make-move 2 3))
                "should give a a plan") 
  (check-equal? (solution '(7 10) 9)
                (list (make-fill 2) 
                      (make-move 2 1) 
                      (make-dump 1) 
                      (make-move 2 1) 
                      (make-fill 2) 
                      (make-move 2 1) 
                      (make-dump 1) 
                      (make-move 2 1) 
                      (make-fill 2) 
                      (make-move 2 1))
                "should give a a plan") 
  (check-equal? (solution '(7 10) 9)
                (list (make-fill 2) 
                      (make-move 2 1) 
                      (make-dump 1)
                      (make-move 2 1) 
                      (make-fill 2)
                      (make-move 2 1)
                      (make-dump 1) 
                      (make-move 2 1) 
                      (make-fill 2) 
                      (make-move 2 1))
                "should give a a plan") 
  (check-equal? (solution '(3 5 16 65 32 128 8) 50)
                (list (make-fill 4) 
                      (make-move 4 2)
                      (make-dump 2) 
                      (make-move 4 2) 
                      (make-dump 2) 
                      (make-move 4 2))
                "should give a a plan")
  (check-equal? (pitchers-to-list
                 (pitchers-after-moves
                  (list-to-pitchers 
                   '((3 0) (5 0) (16 0) (65 0) (32 0) (128 0) (8 0)))
                  (solution '(3 5 16 65 32 128 8) 50)))
                (list (list 3 0) 
                      (list 5 5)
                      (list 16 0) 
                      (list 65 50) 
                      (list 32 0) 
                      (list 128 0) 
                      (list 8 0))
                "getting output for solution in external represtation"))


