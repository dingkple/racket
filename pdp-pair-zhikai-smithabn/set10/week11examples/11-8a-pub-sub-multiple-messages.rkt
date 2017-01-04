;; A Direction is one of:
;; -- "left"
;; -- "right"

;; change-edge : Direction NonNegInteger -> Void
;; EFFECT: sets the edge in the given direction to the given value
;; STRATEGY: Struct Decomp on Direction
(define/public (change-edge dir val)
  (cond
   [(string=? dir "left") (set! left-edge val)]
   [(string=? dir "right") (set! right-edge val)]))

;; ====== OR ===========

(define-struct change-left-message (val))
(define-struct change-right-message (val))
;; A ChangeEdgeMessage is one of
;; -- (make-change-left-message  NonNegInteger)
;; -- (make-change-right-message NonNegInteger)

;; change-edge : ChangeEdgeMessage -> Void
;; EFFECT: sets the edge in the given direction to the given value
;; STRATEGY: Struct Decomp on Direction
(define/public (change-edge msg)
  (cond
   [(change-left-message? msg) 
    (set! left-edge (change-left-message-val msg))]
   [(change-right-message? msg) 
    (set! right-edge (change-right-message-val msg))]
