;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")

(provide
  initial-robot
  robot-left 
  robot-right
  robot-forward
  robot-north? 
  robot-south? 
  robot-east? 
  robot-west?) 

(define-struct robot(x y r face))
(define-struct canvas(x_start x_stop y_start y_stop))

(define my_canvas (make-canvas 0 200 0 400))

(define (initial-robot x y)
  (make-robot x y 15 0)
  )

(define (robot-left rt)
  (make-robot (rt-x rt)
              (rt-y rt)
              (rt-r rt)
              (rt-face (modulo (- face 1) 4))))

(define (robot-right rt)
  (make-robot (rt-x rt)
              (rt-y rt)
              (rt-r rt)
              (rt-face (modulo (+ face 1) 4))))

(define (get-x x_now face distance c)
  (if (= face 0) 
      (if (< (- x_now distance) (canvas-x_start c)) 
          (- x_now distance)
          (canvas-start c)
          )
      (if (< (+ x_now distance) (canvas-x_stop c)) 
          (+ x_now distance)
          (canvas-y_stop c)
          )))

(define (get-y y_now face distance c)
  (if (= face 3) 
      (if (< (- y_now distance) (canvas-y_start c)) 
          (- y_now distance)
          (canvas-start c)
          )
      (if (< (+ y_now distance) (canvas-y_stop c)) 
          (+ y_now distance)
          (canvas-y_stop c)
          )))
      

(define (robot-forward r distance)
  (if (or (= (modulo (robot-face r) 4) 3) (= (modulo (robot-face r) 4) 1))
      (make-robot (get-x (robot-x x) (robot-face r) distance mycanvas) (robot-y r) (robot-r r) (robot-face r))
      (make-robot (robot-y r) (get-x (robot-y r) (robot-face r) distance mycanvas) (robot-r r) (robot-face r))
      ))

(define (robot-north? r)
  if (= (robot-face r) 0))

(define (robot-east? r)
  if (= (robot-face r) 1))

(define (robot-south? r)
  if (= (robot-face r) 2))

(define (robot-west? r)
  if (= (robot-face r) 3))


      
  
              