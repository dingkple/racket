;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 400)
(define HALF-CANVAS-WIDTH 200)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define RATE 0.25)
(define INI-RX (/ CANVAS-WIDTH 2))
(define INI-RY (/ CANVAS-HEIGHT 3))
(define RECT-SIZE 20)
(define HALF-RECT-SIZE 10)
(define OL-NODE (rectangle RECT-SIZE RECT-SIZE "outline" "green"))
(define SL-NODE (rectangle RECT-SIZE RECT-SIZE "solid" "green"))
(define NODES-SPACE-HEIGHT 60)
(define NODES-SPACE-WIDTH 40)
(define LEVEL-1-HEIGHT INI-RY)
(define LEVEL-2-HEIGHT (+ LEVEL-1-HEIGHT NODES-SPACE-HEIGHT))
(define LEVEL-3-HEIGHT (+ LEVEL-2-HEIGHT NODES-SPACE-HEIGHT))
(define LEVEL-1-WIDTH INI-RX)
(define LEVEL-2-WIDTH (+ LEVEL-1-WIDTH NODES-SPACE-WIDTH)) 
(define LEVEL-3-WIDTH (+ LEVEL-2-WIDTH NODES-SPACE-WIDTH))

(define ZERO 0)
(define NONE -1)

(define-struct node (name x-pos y-pos len-to-next children))


(define-struct world (root selected boundary nodeNum))

(define (initial-world x)
	(make-world empty -1 0 0))

(define (run x)
	(big-bang (initial-world ZERO)
			  (on-tick world-after-tick RATE)
			  (on-draw world-to-scene)
			  (on-key world-after-key-event)
			  (on-mouse world-after-mouse-event)))

(define (world-after-key-event w kev)
	(cond
		[(key=? kev "t") (world-after-t-key w)]
		; [(key=? kev "n") (world-after-n-key w)]
		; [(key=? kev "d") (world-after-d-key w)]
		; [(key=? kev "u") (world-after-u-key w)]
		[else w]))


; (1 (2 3 4))

; (2 (5 6 7 8))

; (define n1 (make-node 1  empty))
; (define n2 (make-node 2  empty))
; (define n3 (make-node 3  empty))

; (define n4 (make-node 4  (list n1 n2 n3)))
; (define n5 (make-node 5  (list n4)))
; (define n6 (make-node 6 empty))
; (define n5-t (make-node 5 LEVEL-1-WIDTH LEVEL-1-HEIGHT (list n6 n4)))

(define (initial-node name x-pos y-pos)
	(make-node name x-pos y-pos 0 empty))

; (define w1 (make-world n5 5 0 5))
; (define w1-t (make-world n5-t 5 0 6))

(define (check-node aNode aNodeName newName boundary)
 (if (= (node-name aNode) aNodeName)
		(make-node (node-name aNode)
				   (node-x-pos aNode)
				   (node-y-pos aNode)
				   (+ (node-len-to-next aNode) NODES-SPACE-WIDTH)
				   (cons (initial-node newName
				   					   boundary
				   					   (+ (node-y-pos aNode) NODES-SPACE-HEIGHT))
				   		 (node-children aNode)))
		(make-node (node-name aNode) 
				   (node-x-pos aNode)
				   (node-y-pos aNode)
				   (node-len-to-next aNode)
				   (add-node-to-selected (node-children aNode) 
				   						 aNodeName 
				   						 newName 
				   						 boundary))))


(define (add-node-to-selected lon aNodeName newName boundary)
	(cond
		[(empty? lon) empty]
		[else (cons (check-node (first lon) aNodeName newName boundary)
 					(add-node-to-selected (rest lon) aNodeName newName boundary))]))


(define (world-after-mouse-event w mx my mev)
	(cond
		[(mouse=? mev "button-down") (world-after-button-down w mx my)]
		[(mouse=? mev "button-up") (world-after-button-up w)]
		[(mouse=? mev "drag") (world-after-drag w mx my)]
		[else w]))


(define (add-first-node w)
	(make-world (initial-node (+ 1 (world-nodeNum w))
							  INI-RX
							  INI-RY)
				NONE
				HALF-CANVAS-WIDTH
				1))


(define (world-after-t-key w)
	; (if (= (world-selected w) NONE)
	; 	w
	; 	(world-after-t-key-helper w)))
	(cond
		[(= 0 (world-nodeNum w)) (add-first-node w)]
		[(= (world-selected w) NONE) w]
		[else (world-after-t-key-helper w)]))

(define (world-after-t-key-helper w)
	(make-world (check-node (world-root w) 
							(world-selected w) 
							(+ (world-nodeNum w) 1) 
							(world-boundary w))
				(world-selected w)
				(- (world-boundary w) NODES-SPACE-WIDTH)
				(+ (world-nodeNum w) 1)))

(define (draw-node aNode selected scene)
	(if (= selected (node-name aNode))
		(place-image SL-NODE
					 (node-x-pos aNode)
					 (node-y-pos aNode)
					 (draw-children (node-children aNode)
					 				 selected
					 				 scene))
		(place-image OL-NODE
					 (node-x-pos aNode)
					 (node-y-pos aNode)
 					 (draw-children (node-children aNode)
 									selected
					 				scene))))

(define (draw-children lon selected scene)
	(cond
		[(empty? lon) scene]
		[else (draw-node (first lon)
						 selected
						 (draw-children (rest lon)
						 				selected
						 				scene))]))


(define (world-to-scene w)
	(if (> (world-nodeNum w) 0)
		(draw-node (world-root w)
 				   (world-selected w)
				   EMPTY-CANVAS)
		EMPTY-CANVAS))


(define (world-after-tick w)
	w)

(define (selecting-node x y nodeX nodeY)
	(and (<= (- nodeX HALF-RECT-SIZE)
			 x
			 (+ nodeX HALF-RECT-SIZE))
		 (<= (- nodeY HALF-RECT-SIZE)
		 	 y
		 	 (+ nodeY HALF-RECT-SIZE))))

(define (find-selected-node aNode mx my)
	; (if (selecting-node (node-x-pos aNode) (node-y-pos aNode) mx my)
	; 	(node-name aNode)
	; 	(find-selected-node-children (node-children aNode))))
	(cond
		[(empty? aNode) -1]
		[(selecting-node (node-x-pos aNode) (node-y-pos aNode) mx my)
		 	(node-name aNode)]
		[else (find-selected-node-children (node-children aNode) mx my)]))

(define (find-selected-node-children lon mx my)
	(cond
		[(empty? lon) -1]
		[else (if (> (find-selected-node (first lon) mx my) -1)
				  (find-selected-node (first lon) mx my)
				  (find-selected-node-children (rest lon) mx my))]))

(define (set-world-boundary aNode selected)
	(cond
		[(= selected NONE) NONE]
		[(= (node-name aNode) selected) (- (node-x-pos aNode)
										   (node-len-to-next aNode))]
		[else (set-world-boundary-children (node-children aNode) selected)]))

(define (set-world-boundary-children lon selected)
	(cond
		[(empty? lon) NONE]
		[else (if (> (set-world-boundary (first lon) selected) NONE)
			      (set-world-boundary (first lon) selected)
			      (set-world-boundary-children (rest lon) selected))]))

(define (world-after-drag w mx my)
	(make-world (move-selected-node (world-root w) (world-selected w) mx my)
				(world-selected mx)
				(world-boundary w)
				(world-nodeNum w)))

(define (move-selected-node aNode aNodeName mx my)
	(cond
		[(= )]

(define (world-after-button-down-helper w mx my selected)
	(make-world
		(world-root w)
		selected
		(set-world-boundary (world-root w) selected)
		(world-nodeNum w)))

(define (world-after-button-down w mx my)
	(world-after-button-down-helper w mx my (find-selected-node (world-root w) mx my)))


(define (world-after-button-up w)
	(make-world (world-root w)
				-1
				(world-boundary w)
				(world-nodeNum w)))



; (begin-for-test
; 	(check-equal? (world-after-t-key w1)
; 	              w1-t
; 	              ""))






