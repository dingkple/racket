;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |25|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;check_all_true: ListofBoolean -> boolean
;GIVEN: a ListofBoolean
;RETURN: a boolean, true if all the item in the list are true, false if not
;EXAMPLES:
;  (check_all_true (list true true true)) -> true
;  (check_all_true (list true true true false true)) -> false
(define (check_all_true lst)
  (cond
    [(empty? lst) true]
    [else (and (first lst) 
               (check_all_true (rest lst)))]
    ))

(check-expect (check_all_true (list true true true)) true)
(check-expect  (check_all_true (list true true true false true)) false)