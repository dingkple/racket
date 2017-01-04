;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;FILE NAME: pretty.rkt

(require "extras.rkt")
(require rackunit)

(provide
 make-sum-exp
 sum-exp-exprs
 make-mult-exp
 mult-exp-exprs
 expr-to-strings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITION

(define-struct sum-exp (exprs))
;; A SumExp is a (make-sum-exp NELOExpr)
;; Interpretation:
;; exprs is a NELOExpr, the struct means all the members in the list 
;; adding together

(define-struct mult-exp (exprs))
;; A MultExp is a (make-mult-exp NELOExpr)
;; Interpretation:
;; exprs is a NELOExpr, the struct means all the members in the list 
;; multiplying together


;; An Expr is one of
;; -- Integer
;; -- (make-sum-exp NELOExpr)
;; -- (make-mult-exp NELOExpr)
;; Interpretation: 
;; a sum-exp represents a sum expression
;; a mult-exp represents a multiplication expression

;; TEMPLATE
;; e-fn : Expr -> ??
;; (define (e-fn expr)
;;   (cond
;;     [(integer? expr) ...]
;;     [(sum-exp? expr) (...
;;                         (sum-exp-exprs expr))]
;;     [(mult-exp? expr) (...
;;                        (mult-exp-exprs expr))]))

;; A LOExpr is one of
;; -- empty
;; -- (cons Expr LOExpr)

;; TEMPLATE
;; loe-fn : LOExpr -> ??
;; (define (loe-fn loe)
;;   (cond
;;     [(empty? loe) ...]
;;     [else (...
;;            (e-fn (first loe))
;;            (loe-fn (rest loe)))]))

;; NELOExpr = (cons Expr LOExpr)
;; Interpretation:
;; the first element is a Expr
;; the rest of NELOExpr is a LOExpr

;; TEMPLATE
;; neloe-fn : NELOExpr -> ??
;; (define (neloe-fn neloe)
;;   (... 
;;    (e-fn (first neloe))
;;    (loe-fn (rest neloe))))

;; A ListOfString (LOS) is one of
;; -- empty
;; -- (cons String ListOfString)

;; TEMPLATE
;; los-fn : LOS -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los) ...]
;;     [else (...
;;             (first los)
;;             (los-fn (rest los))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; Error message
(define ERROR-MSG "not enough room")

;; Spaces
(define ONE-SPACE " ")
(define THREE-SPACES "   ")
(define NULL "")

;; parentheses
(define LP-ADD "(+")
(define LP-MUL "(*")
(define RP ")")

;; Numbers
(define ZERO 0)
(define ONE 1)
(define THREE 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; examples for testing


(define hw-example-0 100)

(define hw-example-1 
  (make-sum-exp (list 22 333 44)))

(define hw-example-2
  (make-sum-exp
   (list
    (make-mult-exp (list 22 3333 44))
    (make-mult-exp
     (list
      (make-sum-exp (list 66 67 68))
      (make-mult-exp (list 42 43))))
    (make-mult-exp (list 77 88)))))

(define hw-example-3
  (make-mult-exp
   (list
    (make-sum-exp (list 22 3333 44))
    (make-sum-exp
     (list
      (make-mult-exp (list 42 43))))
    (make-mult-exp (list 77 88)))))

(define hw-example-4
  (make-sum-exp (list 22 333 (make-mult-exp (list 11 556)))))

(define hw-example-5
  (make-mult-exp
   (list
    (make-sum-exp (list 22 33 44))
    (make-sum-exp
     (list
      (make-mult-exp (list 66 67 68))
      (make-sum-exp (list 42 43))))
    (make-sum-exp (list 77 88)))))



(define hw-1-ans "(+ 22 333 44)")
(define hw-2-ans "(+ (* 22 3333 44) (* (+ 66 67 68) (* 42 43)) (* 77 88))")
(define hw-3-ans "(+ (* 22 3333 44) (* (* 42 43)) (* 77 88))")
(define hw-0-10-ans '("100"))
(define hw-1-15-ans '("(+ 22 333 44)"))
(define hw-1-10-ans '("(+ 22" "   333" "   44)"))

(define hw-example-2-100-ans 
  '("(+ (* 22 3333 44) (* (+ 66 67 68) (* 42 43)) (* 77 88))"))

(define hw-example-2-50-ans 
  '("(+ (* 22 3333 44)"
    "   (* (+ 66 67 68) (* 42 43))"
    "   (* 77 88))"))

(define hw-example-2-15-ans
  '("(+ (* 22"
    "      3333"
    "      44)"
    "   (* (+ 66"
    "         67"
    "         68)"
    "      (* 42"
    "         43))"
    "   (* 77 88))"))

(define hw-example-3-50-ans
  '("(* (+ 22 3333 44) (+ (* 42 43)) (* 77 88))"))

;; (display-strings! (expr-to-strings hw-example-1 15))
;; (display-strings! (expr-to-strings hw-example-1 10))
;; (display-strings! (expr-to-strings hw-example-2 100))
;; (display-strings! (expr-to-strings hw-example-2 50))
;; (display-strings! (expr-to-strings hw-example-2 15))
;; (display-strings! (expr-to-strings hw-example-2 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expr-length: Expr -> NonNegInt
;; GIVEN: an expr
;; RETURNS: the length of the expr written in one line
;; EXAMPLES: (expr-length 15) -> 2
;; STRATEGY: structural decomposition on expr : Expr
(define (expr-length expr)
  (cond 
    [(integer? expr) (string-length (number->string expr))]
    [(sum-exp? expr) (sm-expr-length (sum-exp-exprs expr))]
    [(mult-exp? expr) (sm-expr-length (mult-exp-exprs expr))]))

;; sm-expr-length: NELOExpr -> NonNegInt
;; GIVEN: a list of exprs
;; WHERE: the given list of exprs belongs to a SumExp or a MultExp
;; RETURNS: the length of the given list exprs written in one line
;; EXAMPLES: (sm-expr-length (list 1 2)) -> 7
;; STRATEGY: HOFC
(define (sm-expr-length loe)
  (foldr
   ;; Expr NonNegInt -> NonNegInt
   ;; GIVEN: an expr and a non negative integer
   ;; RETURNS: the length of the given expr plus the the given integer
   (lambda (e ans-til-now)
     (+ ans-til-now (expr-length e) 1))
   3
   loe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expr-to-strings: Expr NonNegInt -> LOS
;; GIVEN: an Expr expr and a non negative integer length representing the 
;;        max-length allowed for strings in the list
;; RETURNS: the given expr converted to a list of string within the given 
;;          length
;; EXAMPLES: see tests below
;; STRATEGY: function composition
(define (expr-to-strings expr length)
  (expr-to-lines expr length 0))

;; expr-to-lines: Expr NonNegInt NonNegInt -> LOS
;; GIVEN: an Expr expr, an non negative integer length and a non negative 
;;        integer pn
;; WHERE: length means the the max-length allowed for strings in the list
;;        pn means the number of the right brackets at the end of the 
;;        last line
;; RETURNS: the given expr converted to a list of strings within the given 
;;          length
;; EXAMPLES: (expr-to-lines (make-sum-exp (list 1 2)) 10 0) => '("(+ 1 2)")
;; STRATEGY: function composition
(define (expr-to-lines expr length pn)
  (if (<= (expr-length expr) (- length pn))
      (list (whole-expr-to-string expr))
      (ex-to-stack expr length pn)))

;; ex-to-stack: Expr NonNegInt NonNegInt -> LOS
;; GIVEN: an Expr expr, an integer and a non-neg-integer
;; WHERE: the given expr should be a mult-exp or a sum-exp, or it will 
;;        raise an error since the length of the expr is already too long.
;;        And the length of the given expr written in one line is longer
;;        than the given length
;;        length means the the max-length allowed for strings the given 
;;        exprs can convert to
;;        pn means the number of the right brackets at the end of last line
;; RETURNS: the given expr converted to a list of string within the given 
;;          length
;; EXAMPLES: (ex-to-stack (make-sum (list 1 2)) 10 0) 
;;                  -> '("(+ 1" "   2")
;; STRATEGY: structural decomposition on expr: Expr
(define (ex-to-stack expr length pn)
  (cond
    [(number? expr) (error ERROR-MSG)]
    [(sum-exp? expr)  
     (sm-body-to-list LP-ADD (sum-exp-exprs expr) (- length 3) pn)]
    [(mult-exp? expr)
     (sm-body-to-list LP-MUL (mult-exp-exprs expr) (- length 3) pn)]))

;; sm-body-to-list: String NELOExpr NonNegInt NonNegInt -> LOS
;; GIVEN: a string and a non-empty list of exprs, a non-neg-integer and 
;;        another non-neg-integer
;; WHERE: the string is the prefix needed to add to the los which the given
;;        list of exprs converted to
;;        the list of exprs is from a SumExp or a MultExp
;;        length means the the max-length allowed for strings the given 
;;        exprs converted to
;;        pn means the number of the right brackets at the end of last line 
;; RETURNS: the given list of exprs converted to a list of string within 
;;          the given length
;; EXAMPLES: (sum-body-to-list LP-ADD (list 1 3) 10 0) -> ("(+ 1" "   2")
;; STRATEGY: function composition    
(define (sm-body-to-list prefix loe length pn)
  (sm-head-updated prefix 
                  (body-to-list loe length pn)))

;; body-to-list: NELOExpr NonNegInt NonNegInt -> LOS
;; GIVEN: a non-empty list of exprs, an integer and a non-neg-integer
;; WHERE: length means the the max-length allowed for string the 
;;        given exprs converted to
;;        pn means the number of the right brackets at the end of last line
;; RETURNS: the given NELOExpr converted to a list of string within the 
;;          given length
;; EXAMPLES: (body-to-list (list 1 2) 10 0) => '("   1" "   )")
;; STRATEGY: HOFC
(define (body-to-list exprs length pn)
  (foldr
   ;; Expr LOS -> LOS
   ;; GIVEN: an Expr and a LOS
   ;; RETURNS: the given Expr converted to a list of strings and then add to
   ;;          the head of given LOS
   (lambda (expr ans)
     (if (empty? ans)
         (expr-add-to-list (expr-to-lines expr length (+ pn 1)) ans)
         (expr-add-to-list (expr-to-lines expr length 0) ans)))
   empty
   exprs))

;; sm-head-updated: String LOS -> LOS
;; GIVEN: a String and a list of strings
;; WHERE: the string is the prefix needed to add to the los which the given
;;        list of exprs converted to
;;        and the given list of strings is not empty
;; RETURNS: the first string in the given list is updated by the given
;;          given prefix
;; EXAMPLES: (sm-head-updated LP-ADD '("   1" "   2)"))
;;               ->'("(+ 1" "   2)")
;; STRATEGY: structural decomposition on los: LOS
(define (sm-head-updated prefix los)
  (cons (string-append
         prefix ONE-SPACE (substring (first los) 3))
        (rest los)))

;; expr-add-to-list: LOS LOS -> LOS
;; GIVEN: a list of strings and another list of strings
;; WHERE: the first list of strings is not empty
;; RETURNS: the second list appended to the first list, and the last string 
;;          of the first list is updated(append a right bracket ")"
;; EXAMPLES: (expr-add-to-list '("   1" "   2") empty) -> '("   1" "   2)")
;; STRATEGY: HOFC
(define (expr-add-to-list lst ans)
  (foldr
   ;; String LOS -> LOS
   ;; GIVEN: a string and a los
   ;; RETURNS: the given string add to the head of the given los, iff the 
   ;;          string is the last line of the expr, add a right bracket
   ;;          to its tail
   (lambda (str ln)
     (if (empty? ln)
         (cons (string-append THREE-SPACES str RP) ln)
         (cons (string-append THREE-SPACES str) ln)))
   ans
   lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; whole-expr-to-string: Expr -> String
;; GIVEN: an Expr expr
;; RETURNS: the string of given expression written in one line
;; EXAMPLES: (whole-expr-to-string (make-sum-exp (list 22 333 44))) 
;;               => "(+ 22 333 44)"
;; STRATEGY: structural decomposition on expr: Expr
(define (whole-expr-to-string expr)
  (cond
    [(number? expr) (number->string expr)]
    [(sum-exp? expr) (sm-exp-to-whole-string LP-ADD (sum-exp-exprs expr))]
    [(mult-exp? expr) (sm-exp-to-whole-string LP-MUL (mult-exp-exprs expr))]))

;; sm-exp-to-whole-string: String Expr -> String
;; GIVEN: a string and an Expr expr
;; WHERE: the expr is a SumExp or a MultExp
;;        the string is the prefix needed add to the string the given expr
;;        is converted to
;; RETURNS: the given expr converted to a whole string in one line
;; EXAMPLES: (sm-exp-to-whole-string LP-ADD (make-sum-exp (list 1 22)))
;;               -> "(+ 1 22)"
;; STRATEGY: structural decomposition on expr: SumExp
(define (sm-exp-to-whole-string prefix loe)
  (string-append 
   prefix
   (exprlst-to-whole-string loe)
   RP))

;; exprlst-to-whole-string: NELOExpr -> String
;; GIVEN: a non-empty list of expr
;; RETURN: the string of the given list of exprs written in one line
;; EXAMPLES: (exprlst-to-whole-string (list 22 333 44)) 
;;           => " 22 333 44"
;; STRATEGY: HOFC
(define (exprlst-to-whole-string exprlst)
  (foldr
   ;; Expr String -> String
   ;; GIVEN: an expr and a string
   ;; RETURNS: the given string appended to the string that the given expr
   ;;          converted to
   (lambda (e ans)
     (string-append ONE-SPACE (whole-expr-to-string e) ans))
   NULL
   exprlst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (check-equal? (expr-to-strings hw-example-0 10)
                hw-0-10-ans
                "Just write the number")
  (check-equal? (expr-to-strings hw-example-1 15)
                hw-1-15-ans
                "Should write the whole Expr in single line, but does NOT")
  (check-equal? (expr-to-strings hw-example-1 10)
                hw-1-10-ans
                "Should write the Expr in lines within 10, but does NOT")
  (check-equal? (expr-to-strings hw-example-2 100)
                hw-example-2-100-ans
                "Should write the Expr in lines within 100, but does NOT")
  (check-equal? (expr-to-strings hw-example-2 50)
                hw-example-2-50-ans
                "Should write the Expr in lines within 50, but does NOT")
  (check-equal? (expr-to-strings hw-example-2 14)
                hw-example-2-15-ans
                "Should write the Expr in lines within 10, but does NOT")
  (check-equal? (expr-to-strings hw-example-3 50)
                hw-example-3-50-ans
                "Should write the Expr in lines within 50, but does NOT")
  (check-error (expr-to-strings hw-example-2 10)
               "not enough room"))





