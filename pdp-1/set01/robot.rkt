;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(provide
  initial-robot
  robot-left 
  robot-right
  robot-forward
  robot-north? 
  robot-south? 
  robot-east? 
  robot-west?) 


; DATA DEFINITION:
; a robot is a (make-robot real real Face)
; Interp: 
; --x is robot's x-coordinate
; --y is robot's y-coordinate
; --Face is where the robot is facing, it's from {0 1 2 3}, meaning north east south west respectively
; Template:
;(define (robot-fn r)
;  (...
;   (robot-x r)
;   (robot-y r)
;   (robot-face r)
;   ))
(define-struct robot (x y face))
(define ROBOT_R 15)


(define ROOM_X_START 0)
(define ROOM_X_END 200) 
(define ROOM_Y_START 0)
(define ROOM_Y_END 400)

; initial-robot: Real Real -> robot
; GIVEN: the robot' original x-coordinate and y-coordinate
; RETURN: a robot at given coordinate point
; EXAMPLE:
; (initial-robot -1 -100) -> (make-robot -1 -100 0)
; (initial-robot 10 10) -> (make-robot 10 10 0)
; STRATEGY: Function composition
(define (initial-robot x y)
  (make-robot x y 0)
  )

(begin-for-test 
  (check-equal? (initial-robot -1 -100) (make-robot -1 -100 0))
  (check-equal? (initial-robot 10 10) (make-robot 10 10 0))
  )

; robot-left : robot -> robot
; GIVEN: a robot
; RETURN: a robot with its face turned left
; EXAMPLE:
; (robot-left (make-robot 0 10 1)) -> (make-robot 0 10 0)
; (robot-left (make-robot 10 0 2)) -> (make-robot 10 0 1)
; (robot-left (make-robot 10 0 0)) -> (make-robot 10 0 3)
; STRATEGY: Structural Decomposition [robot]
(define (robot-left rt)
  (make-robot (robot-x rt)
              (robot-y rt)
              (modulo (- (robot-face rt) 1) 4)))
(begin-for-test 
  (check-equal? (robot-left(make-robot 0 10 1)) (make-robot 0 10 0))
  (check-equal? (robot-left(make-robot 10 0 2)) (make-robot 10 0 1))
  (check-equal? (robot-left(make-robot 10 0 0)) (make-robot 10 0 3)))

; robot-left : robot -> robot
; GIVEN: a robot
; RETURN: a robot with its face turned right
; EXAMPLE:
; (robot-left (make-robot 0 10 1)) -> (make-robot 0 10 2)
; (robot-left (make-robot 10 0 2)) -> (make-robot 10 0 3)
; (robot-left (make-robot 10 0 3)) -> (make-robot 10 0 0)
; STRATEGY: Structural Decomposition [robot]
(define (robot-right rt)
  (make-robot (robot-x rt)
              (robot-y rt)
              (modulo (+ (robot-face rt) 1) 4)))

(begin-for-test 
  (check-equal? (robot-right(make-robot 0 10 1)) (make-robot 0 10 2))
  (check-equal? (robot-right(make-robot 10 0 2)) (make-robot 10 0 3))
  (check-equal? (robot-right(make-robot 10 0 3)) (make-robot 10 0 0)))



; get-y-north: (get-y-north robot PosReal)
; GIVEN: a robot facing north, the distance to move
; RETURN: the y-coordinate after the movement 
; STRATEGY: Structural Decomposition [robot]
; (similar to fucntions like get-y-south, get-x-west, get-x-east)
(define (get-y-north r distance)
  (if (< (robot-y r) (+ ROBOT_R ROOM_Y_START))
      (- (robot-y r) distance)
      (if (< (- (robot-y r) distance) (+ ROBOT_R ROOM_Y_START))
          (+ ROBOT_R ROOM_Y_START)
          (- (robot-y r) distance))))

(define (get-y-south r distance)
  (if (> (robot-y r) (- ROOM_Y_END ROBOT_R))
      (+ (robot-y r) distance)
      (if (> (+ (robot-y r) distance) (- ROOM_Y_END ROBOT_R))
          (- ROOM_Y_END ROBOT_R)
          (+ (robot-y r) distance))))

(define (get-x-west r distance)
  (if (< (robot-x r) (+ ROBOT_R ROOM_X_START))
      (- (robot-x r) distance)
      (if (< (- (robot-x r) distance) (+ ROBOT_R ROOM_X_START))
          (+ ROBOT_R ROOM_X_START)
          (- (robot-x r) distance))))

(define (get-x-east r distance)
  (if (> (robot-x r) (- ROOM_X_END ROBOT_R))
      (+ (robot-x r) distance)
      (if (> (+ (robot-x r) distance) (- ROOM_X_END ROBOT_R))
          (- ROOM_X_END ROBOT_R)
          (+ (robot-x r) distance))))


; x-outside?: robot -> Boolean
; GIVEN: a robot
; RETURN: Boolean, #f iff the the x-coordinate is in the room's range, else true
; STRATEGY: Structural Decomposition [robot]
(define (x-outside? r)
  (or (< (robot-x r) (+ ROOM_X_START ROBOT_R)) 
            (> ROBOT_R (- ROOM_X_END ROBOT_R))))
       
(define (y-outside? r)
  (or (< (robot-y r) (+ ROOM_Y_START ROBOT_R))
            (> (robot-y r) (- ROOM_Y_END ROBOT_R))))



; get-x: real PosReal  -> robot
; GIVEN: the robot's x-coordinate, face(1 or 3), distance to move
; RETURN: the robot's x-coordinate after the move
; EXAMPLE: 
; (get-x 0 3 100) -> 0
; (get-x 100 1 40) -> 140
; (get-x 20 1 400) -> 200
; STRATEGY: Structural Decomposition [robot]
(define (get-x r distance)
  (if (y-outside? r)
      (if (= (robot-face r) 1)
          (+ (robot-x r) distance)
          (- (robot-x r) distance))
      (if (= (robot-face r) 3) 
          (get-x-west r distance )
          (get-x-east r distance))))

; get-y: real PosReal  -> robot
; GIVEN: the robot's y-coordinate, face(0 or 2), distance to move
; RETURN: the robot's y-coordinate after the move
; EXAMPLE: 
; (get-y 0 0 100) -> 0
; (get-y 100 2 40) -> 140
; (get-y 20 2 500) -> 400
; STRATEGY: Structural Decomposition [robot]
(define (get-y r distance)
  (if (x-outside? r)
      (if (= (robot-face r) 0)
          (- (robot-y r) distance)
          (+ (robot-y r) distance))
      (if (= (robot-face r) 2) 
          (get-y-south r distance)
          (get-y-north r distance))))
   
; robot-forward : robot PosReal -> robot 
; GIVEN: the robot and the distance to move
; RETURN: the robot at the coordinate point after move
; EXAMPLE: 
; (robot-forward (make-robot 20 220 0) 100) -> (make-robot 20 120 0))
; (robot-forward (make-robot 10 50 0) 100) -> (make-robot 10 -50 0))
; (robot-forward (make-robot 100 0 0) 100) -> (make-robot 100 -100 0))
; (robot-forward (make-robot 1000 0) 300) -> (make-robot 100 0))
; (robot-forward (make-robot 0 50 0) 500) -> (make-robot 0 -450 0))
; (robot-forward (make-robot 0 10 1) 100) -> (make-robot 100 10 1))
; (robot-forward (make-robot 0 10 1) 400) -> (make-robot 400 10 1))
; (robot-forward (make-robot 200 50 1) 500) -> (make-robot 700 50 1))
; (robot-forward (make-robot 10 50 1) 100) -> (make-robot 110 50 1))
; (robot-forward (make-robot 20 50 1) 500) -> (make-robot 185 50 1))
; (robot-forward (make-robot 20 2) 200) -> (make-robot 220 2))
; (robot-forward (make-robot 100 500 2) 100) -> (make-robot 100 600 2))
; (robot-forward (make-robot 20 220 2) 1000) -> (make-robot 20 385 2))
; (robot-forward (make-robot 0 50 2) 500) -> (make-robot 0 550 2))
; (robot-forward (make-robot 1220 20 3) -> 2000) (make-robot 20 3))
; (robot-forward (make-robot 1220 20 3) -> 1200) (make-robot 20 20 3))
; (robot-forward (make-robot -10 50 3) -> 100) (make-robot -110 50 3))
; (robot-forward (make-robot 20 0 3) -> 500) (make-robot -480 0 3))
; STRATEGY: Structural Decomposition [robot]
(define (robot-forward r distance)
  (if (or (= (modulo (robot-face r) 4) 3) (= (modulo (robot-face r) 4) 1))
      (make-robot (get-x r distance) 
                  (robot-y r) 
                  (robot-face r))
      (make-robot (robot-x r) 
                  (get-y r distance) 
                  (robot-face r))
      ))

(begin-for-test
  (check-equal? (robot-forward (make-robot 20 220 0) 100) (make-robot 20 120 0))
  (check-equal? (robot-forward (make-robot 10 50 0) 100) (make-robot 10 -50 0))
  (check-equal? (robot-forward (make-robot 100 0 0) 100) (make-robot 100 -100 0))
  (check-equal? (robot-forward (make-robot 100 150 0) 300) (make-robot 100 15 0))
  (check-equal? (robot-forward (make-robot 0 50 0) 500) (make-robot 0 -450 0))
  (check-equal? (robot-forward (make-robot 0 10 1) 100) (make-robot 100 10 1))
  (check-equal? (robot-forward (make-robot 0 10 1) 400) (make-robot 400 10 1))
  (check-equal? (robot-forward (make-robot 200 50 1) 500) (make-robot 700 50 1))
  (check-equal? (robot-forward (make-robot 10 50 1) 100) (make-robot 110 50 1))
  (check-equal? (robot-forward (make-robot 20 50 1) 500) (make-robot 185 50 1))
  (check-equal? (robot-forward (make-robot 20 150 2 ) 200) (make-robot 20 350 2))
  (check-equal? (robot-forward (make-robot 100 500 2) 100) (make-robot 100 600 2))
  (check-equal? (robot-forward (make-robot 0 30 2) 500) (make-robot 0 530 2))
  (check-equal? (robot-forward (make-robot 15 20 2) 1000) (make-robot 15 385 2))
  (check-equal? (robot-forward (make-robot 150 50 2) 500) (make-robot 150 385 2))
  (check-equal? (robot-forward (make-robot 1220 20 3) 2000) (make-robot 15 20 3))
  (check-equal? (robot-forward (make-robot 1220 20 3) 1200) (make-robot 20 20 3))
  (check-equal? (robot-forward (make-robot -10 50 3) 100) (make-robot -110 50 3))
  (check-equal? (robot-forward (make-robot 20 0 3) 500) (make-robot -480 0 3))
  
  )


; robot-north? : robot -> Boolean
; GIVEN: a robot
; RETURN: true iff the robot is facing north(east, soutch, west)
; EXAMPLE:
;   (robot-north? (make-robot 0 10 1)) -> #f
;   (robot-north? (make-robot 0 10 0)) -> #t
;   (robot-east? (make-robot 0 10 2)) -> #f
;   (robot-east? (make-robot 0 10 1)) -> #t
;   (robot-south? (make-robot 0 10 2)) -> #t
;   (robot-south? (make-robot 0 10 0)) -> #f
;   (robot-west? (make-robot 0 10 3)) -> #t
;   (robot-west? (make-robot 0 10 0)) -> #f
; STRATEGY: Structural Decomposition [robot]
(define (robot-north? r)
  (if (= (robot-face r) 0)
      true
      false))
(define (robot-east? r)
  (if (= (robot-face r) 1)
      true
      false))
(define (robot-south? r)
  (if (= (robot-face r) 2)
      true
      false))
(define (robot-west? r)
  (if (= (robot-face r) 3)
      true
      false))

(begin-for-test 
  (check-equal? (robot-north? (make-robot 0 10 0)) #t)
  (check-equal? (robot-north? (make-robot 0 10 1)) #f)
  (check-equal? (robot-east? (make-robot 0 10 1)) #t)
  (check-equal? (robot-east? (make-robot 0 10 2)) #f)
  (check-equal? (robot-south? (make-robot 0 10 2)) #t)
  (check-equal? (robot-south? (make-robot 0 10 0)) #f)
  (check-equal? (robot-west? (make-robot 0 10 3)) #t)
  (check-equal? (robot-west? (make-robot 0 10 1)) #f)
  )



