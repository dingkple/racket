;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)


(provide 
 make-editor
 editor-pre
 editor-post
 edit
 )


; DATA DEFINITION: an editor is a (make-editor String String)
; Interp:
;  --pre is the String before the cursor
;  --post is the String after the cursor
; TEMPLATE:
; (define (editor-fn e)
;  (...
;   (editor-pre e)
;   (editor-post e)))
(define-struct editor (pre post))


; cursor_left: editor -> editor
; GIVEN: an editor
; RETURN: an editor with its cursor move one charactor to the left
; STRATEGY: Structural Decomposition [editor]
(define (cursor_left ed)
  (if (> (string-length (editor-pre ed)) 0)
      
      (make-editor (substring (editor-pre ed)
                              0
                              (- (string-length (editor-pre ed)) 1))
                   (substring (string-append (editor-pre ed)
                                             (editor-post ed))
                              (- (string-length (editor-pre ed)) 1)
                              (string-length (string-append 
                                              (editor-pre ed)
                                              (editor-post ed)))
                              ))
      ed))


; cursor_left: editor -> editor
; GIVEN: an editor
; RETURN: an editor with its cursor move one charactor to the right
; STRATEGY: Structural Decomposition [editor]
(define (cursor_right ed)
  (if (> (string-length (editor-post ed)) 0)
      
      (make-editor (substring (string-append (editor-pre ed)
                                             (editor-post ed))
                              0
                              (+ (string-length (editor-pre ed)) 1)
                              )
                   (substring (editor-post ed)
                              1
                              (string-length (editor-post ed)))
                   )
      ed))


; cursor_last: editor -> editor
; GIVEN: an editor
; RETURN: an editor with the charactor before its cursor deleted iff exists
; STRATEGY: Structural Decomposition [editor]
(define (remove_last ed)
  (if (> (string-length (editor-pre ed)) 0)
      
      (make-editor (substring (editor-pre ed)
                              0
                              (- (string-length (editor-pre ed)) 1))
                   (editor-post ed)
                   )
      ed))


; add_input: editor String -> editor
; GIVEN: a editor
; RETURN: a editor with the String added after the cursor, ignore all the "\t" and "\u007F"
; EXAMPLES: see tests below
; STRATEGY: Structural Decomposition [editor]
(define (add_input ed ke)
  (if (and (not (key=? "\t" ke)) (not (key=? "\u007F" ke)))
      (make-editor (string-append (editor-pre ed) ke)
                   (editor-post ed)
                   )
      ed))


; edit: editor String -> editor
; GIVEN: an editor
; RETURN: an editor after responsing to the input ke
; EXAMPLE:
; (edit (make-editor "Hou" "Jing") "left") -> (make-editor "Ho" "uJing")
; (edit (make-editor "" "HouJing") "left") -> (make-editor "" "HouJing")
; (edit (make-editor "Hou" "Jing") "right") -> (make-editor "HouJ" "ing")
; (edit (make-editor "Hou" "") "left") -> (make-editor "Hou" "")
; (edit (make-editor "Hou" "Jing") "\b") -> (make-editor "Ho" "Jing")
; (edit (make-editor "" "Jing") "\b") -> (make-editor "" "Jing")
; (edit (make-editor "Ho" "Jing") "u") -> (make-editor "Hou" "Jing")
; (edit (make-editor "Hou" "Jing") "\t") -> (make-editor "Hou" "Jing")
; (edit (make-editor "Hou" "Jing") "nice") -> (make-editor "Hou" "Jing")
; STRATEGY: Function Composition                 
(define (edit ed ke)
  (if (key-event? ke)
    (cond
      [(key=? "left" ke) (cursor_left ed)]
      [(key=? "right" ke) (cursor_right ed)]
      [(key=? "\b" ke) (remove_last ed)]
      [else (if (< (string-length ke) 2) 
                (add_input ed ke)
                ed)])
    ed))

(begin-for-test
  (check-equal? (edit (make-editor "Hou" "Jing") "left") (make-editor "Ho" "uJing"))
  (check-equal? (edit (make-editor "" "HouJing") "left") (make-editor "" "HouJing"))
  (check-equal? (edit (make-editor "Hou" "Jing") "right") (make-editor "HouJ" "ing"))
  (check-equal? (edit (make-editor "Hou" "") "right") (make-editor "Hou" ""))
  (check-equal? (edit (make-editor "Hou" "Jing") "\b") (make-editor "Ho" "Jing"))
  (check-equal? (edit (make-editor "" "Jing") "\b") (make-editor "" "Jing"))
  (check-equal? (edit (make-editor "Ho" "Jing") "u") (make-editor "Hou" "Jing"))
  (check-equal? (edit (make-editor "Hou" "Jing") "\t") (make-editor "Hou" "Jing"))
  (check-equal? (edit (make-editor "Hou" "Jing") "nice") (make-editor "Hou" "Jing"))
  (check-equal? (edit (make-editor "Hou" "Jing") "shift") (make-editor "Hou" "Jing"))
  )




