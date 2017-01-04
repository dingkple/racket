#lang racket

(require racket)
(require racket/file)
(require racket/match)
(require racket/pretty)
(require (only-in lang/htdp-beginner string-contains?))

(provide define-pdp-test-suite test-equal? test-or test-and test-group
         test-file test-group-with-points run-pdp-tests test-within
         test-error test-pred test-nontermination)

(pretty-print-columns 70)
(define-for-syntax TERMINATION-TIMEOUT 2) ;; in Seconds

(define-struct pdp-test-suite (problem-set preamble test-files))
;; A PdpTestSuite is a (make-pdp-test-suite String String ListOf<PdpTestFile>)

(define-struct pdp-test-file (file preamble definitions test-groups))
;; A PdpTestFile is a (make-pdp-test-file String String "Language" "Allow-Requires" ListOf<Definition> ListOf<PdpTestGroup>)

(define-struct pdp-test-group (title preamble points definitions tests))
;; A PdpTestGroup is a (make-pdp-test-group String String Number ListOf<Definition> ListOf<PdpTest>)

(define-struct pdp-test-eq (expr1 expr2 thunk1 thunk2 points explanation))
(define-struct pdp-test-within (expr1 expr2 plusminus thunk1 thunk2 thunk3 points explanation))
(define-struct pdp-test-error (expr pred thunk1 thunk2 points explanation))
(define-struct pdp-test-pred (expr pred thunk1 thunk2 points explanation))
(define-struct pdp-test-nonterm (expr thunk waittime points explanation))
(define-struct pdp-test-or (points explanation tests))
(define-struct pdp-test-and (points explanation tests))
;; A PdpTest is one of
;;   - (make-pdp-test-eq Expr Expr Number String)
;;   - (make-pdp-test-or Number String ListOf<PdpTest>)
;;   - (make-pdp-test-and Number String ListOf<PdpTest>)

(define-struct pdp-test-result (lines type success-count fail-count error-count error-messages achieved-points total-points))
(define empty-tr (make-pdp-test-result empty 'neutral 0 0 0 empty 0 0))

(define-syntax (test-equal? stx)
  (syntax-case stx ()
    [(_ file-defs group-defs expr1 expr2 points explanation)
     #`(make-pdp-test-eq (quote expr1) (quote expr2) 
                         (make-thunk (letrec file-defs 
                                       (letrec group-defs expr1)))
                         (make-thunk (letrec file-defs 
                                       (letrec group-defs expr2)))
                         points explanation)]))

(define-syntax (test-within stx)
  (syntax-case stx ()
    [(_ file-defs group-defs expr1 expr2 plusminus points explanation)
     #`(make-pdp-test-within (quote expr1) (quote expr2)
                             (quote plusminus)
                             (make-thunk (letrec file-defs 
                                           (letrec group-defs expr1)))
                             (make-thunk (letrec file-defs 
                                           (letrec group-defs expr2)))
                             (make-thunk (letrec file-defs
                                           (letrec group-defs plusminus)))
                             points explanation)]))

(define-syntax (test-error stx)
  (syntax-case stx ()
    [(_ file-defs group-defs expr pred points explanation)
     #`(make-pdp-test-error (quote expr) (quote pred)
                            (make-thunk (letrec file-defs 
                                          (letrec group-defs expr)))
                            (make-thunk (letrec file-defs 
                                          (letrec group-defs pred)))
                            points explanation)]))
(define-syntax (test-pred stx)
  (syntax-case stx ()
    [(_ file-defs group-defs expr pred points explanation)
     #`(make-pdp-test-pred (quote expr) (quote pred)
                           (make-thunk (letrec file-defs 
                                         (letrec group-defs expr)))
                           (make-thunk (letrec file-defs 
                                         (letrec group-defs pred)))
                           points explanation)]))

(define-syntax (test-nontermination stx)
  (syntax-case stx ()
    [(_ file-defs group-defs expr waittime points explanation)
     #`(make-pdp-test-nonterm 
        (quote expr)
        (lambda () (let* ([channel (make-channel)]
                          [thrd (thread (lambda () (channel-put channel 
                                                                (with-handlers ([(lambda (x) true) (lambda (x) (list false x))])
                                                                  (list true expr)))))]
                          [result (let ([res (sync/timeout #,TERMINATION-TIMEOUT (wrap-evt channel list))])
                                    (kill-thread thrd)
                                    res)])
                     (if (false? result)
                         #t
                         (if (false? (first (first result)))
                             (error (second (first result)))
                             (second (first result))))))
        #;(lambda ()
            (let* ([thrd (thread (lambda () expr))]
                   [result (let ([res (sync/timeout waittime (wrap-evt thrd list))])
                             (kill-thread thrd)
                             res)])
              (if (false? result)
                  #t
                  result)))
        waittime points explanation)]))


(define-syntax (make-thunk stx)
  (syntax-case stx ()
    [(_ code) (if (> TERMINATION-TIMEOUT 0)
                  #`(lambda () (let* ([channel (make-channel)]
                                      [thrd (thread (lambda () (channel-put channel 
                                                                            (with-handlers ([(lambda (x) true) (lambda (x) (list false x))])
                                                                              (list true code)))))]
                                      [result (let ([res (sync/timeout #,TERMINATION-TIMEOUT (wrap-evt channel list))])
                                                (kill-thread thrd)
                                                res)])
                                 (if (false? result)
                                     (error (format "Calculation did not terminate within ~a seconds" #,TERMINATION-TIMEOUT))
                                     (if (false? (first (first result)))
                                         (error (second (first result)))
                                         (second (first result))))))
                  #`(lambda () code))]))

(define-syntax (test-or stx)
  (syntax-case stx ()
    [(_ file-defs group-defs points explanation (tc tr ...) ...)
     #`(make-pdp-test-or points explanation
                         #,(local-expand #`(list (tc file-defs group-defs tr ...) ...)
                                         'expression
                                         #f))]))

(define-syntax (test-and stx)
  (syntax-case stx ()
    [(_ file-defs group-defs points explanation (tc tr ...) ...)
     #`(make-pdp-test-and points explanation
                          #,(local-expand #`(list (tc file-defs group-defs tr ...) ...)
                                          'expression
                                          #f))]))

(define-syntax (test-group stx)
  (syntax-case stx ()
    [(_ file-definitions title preamble group-definitions (tc tr ...) ...)
     #`(make-pdp-test-group title preamble false (quote group-definitions) 
                            #,(local-expand #`(list (tc file-definitions group-definitions tr ...) ...)
                                            'expression
                                            #f))]))

(define-syntax (test-group-with-points stx)
  (syntax-case stx ()
    [(_  file-definitions title preamble points group-definitions (tc tr ...) ...)
     #`(make-pdp-test-group title preamble points (quote group-definitions)
                            #,(local-expand #`(list (tc file-definitions group-definitions tr ...) ...)
                                            'expression
                                            '()))]))

(define-syntax define-pdp-test-suite
  (syntax-rules ()
    [(_ name problem-set preamble 
        test-file ...)
     (define name (make-pdp-test-suite problem-set preamble (list test-file ...)))]))

(define-syntax (test-file stx)
  (syntax-case stx ()
    [(_ filename preamble definitions (tgd tgr ...) ...)
     #`(make-pdp-test-file 
        filename preamble
        (quote definitions)
        #,(local-expand #`(list (tgd definitions tgr ...) ...)
                        'expression
                        '()))]))

(define (run-pdp-tests tests [student-name "unknown"] [out-name (string-append "test-results-" student-name)] [col-path (current-directory)])
  (let ([result (pdp-test-suite-files->results (pdp-test-suite-test-files tests) col-path)]
        [header (list 
                 "#lang scribble/base"
                 "@(require scribble/manual)"
                 (string-append
                  "@title{CS5010 - Problem Set " 
                  (pdp-test-suite-problem-set tests) " - Test Results}")
                 (string-append "@author{" student-name "}")
                 (pdp-test-suite-preamble tests))])
    (if (and (= (pdp-test-result-success-count result) 0)
             (= (pdp-test-result-fail-count result) 0)
             (> (pdp-test-result-error-count result) 0))
        (scribble-lines 
         (append
          header
          (list 
           "@para{@bold{All test cases failed due to errors!}}"
           #;(string-append "@itemlist{"
                            "@item{You did not provide the functions we need to test your code}"
                            "@item{There is a serious error in one or more of those basic functions}"
                            "@item{Your code requires libraries that are not allowed}"
                            "@item{Your code has other deficiencies that prohibit our tests to run}}")
           ;"@para{Do not panic immediately: Your TA will have a look at your code and try to fix the problem if it is a minor one. You should try to fulfill the requirements in the future, though}"
           "Error messages:"
           (string-join (map (lambda (err) (format "@para{~a}" err)) (pdp-test-result-error-messages result)) "\n")))
         out-name true)
        (scribble-lines 
         (result->lines (prepend-test-result-lines result header)) 
         out-name))))

(define (scribble-lines lines out-name [use-postfix false] [postfix "-errors"])
  (let ([final-name (if use-postfix (string-append out-name postfix) out-name)])
    (if (file-exists? (format "~a.scribble" out-name))
        (delete-file (format "~a.scribble" out-name))
        '())
    (if (file-exists? (format "~a~a.scribble" out-name postfix))
        (delete-file (format "~a~a.scribble" out-name postfix))
        '())
    (display-lines-to-file
     lines
     (string-append final-name ".scribble")
     #:exists 'truncate/replace)
    (if (file-exists? (format "~a.pdf" out-name))
        (delete-file (format "~a.pdf" out-name))
        '())
    (if (file-exists? (format "~a~a.pdf" out-name postfix))
        (delete-file (format "~a~a.pdf" out-name postfix))
        '())
    (system (string-append (format "scribble --pdf ~a.scribble" final-name)))))

(define (pdp-test-suite-files->results files col-path)
  (combine-test-results* (map (pdp-test-suite-file->results col-path) files)))

(define ((pdp-test-suite-file->results col-path) file)
  (let* ([header
          (append
           (list 
            (string-append "@section{File: " (pdp-test-file-file file) "}")
            (pdp-test-file-preamble file))
           (definitions->lines (pdp-test-file-definitions file)))]
         [test-results
          (combine-test-results*
           (map 
            (pdp-test-group->results 
             (pdp-test-file-definitions file))
            (pdp-test-file-test-groups file)))])
    (prepend-test-result-lines test-results header)))

(define (definitions->lines defs)
  (cond
    [(empty? defs) empty]
    [else 
     (list "@para{Common Definitions}"
           ""
           (string-join (map definition->line defs) "")
           "")]))

(define (definition->line def)
  (match def
    ((list name exp) #;(format "@racketblock{@(define ~s ~s)}" name exp)
                     (format-code `(define ,name ,exp)))))

(define ((pdp-test-group->results defs) group)
  (add-margin-note
   (let* ([sub-results 
           (map (test->results (append defs (pdp-test-group-definitions group))
                               (not (false? (pdp-test-group-points group))))
                (pdp-test-group-tests group))]
          [test-results 
           (if (false? (pdp-test-group-points group))
               (combine-test-results* sub-results)
               (combine-test-results-and* (pdp-test-group-points group) sub-results))]
          [header 
           (append
            (list 
             (string-append 
              "@subsection{Test-Group: " (pdp-test-group-title group) 
              (if (false? (pdp-test-group-points group)) 
                  "" 
                  (format " (~a Points)" (pdp-test-group-points group)))
              "}")
             (if (false? (pdp-test-group-points group))
                 ""
                 'MARGIN-NOTE)
             (pdp-test-group-preamble group))
            (definitions->lines (pdp-test-group-definitions group)))])
     (prepend-test-result-lines test-results header))))

(define ((test->results defs partial?) test)
  (add-margin-note
   (cond
     [(pdp-test-eq? test) (pdp-test-eq->results defs partial? test)]
     [(pdp-test-within? test) (pdp-test-within->results defs partial? test)]
     [(pdp-test-error? test) (pdp-test-error->results defs partial? test)]
     [(pdp-test-pred? test) (pdp-test-pred->results defs partial? test)]
     [(pdp-test-nonterm? test) (pdp-test-nonterm->results defs partial? test)]
     [(pdp-test-or? test) (pdp-test-or->results defs partial? test)]
     [(pdp-test-and? test) (pdp-test-and->results defs partial? test)])))

(define (pdp-test-eq->results defs partial? test)
  (let* ([points (pdp-test-eq-points test)]
         [header 
          (append
           (test-caption "equality" partial? points)
           (list
            (string-append "@para{" (pdp-test-eq-explanation test) "}")
            "Input:"
            (format-code (pdp-test-eq-expr1 test))
            "Expected Output:"
            (format-code (pdp-test-eq-expr2 test))))])
    (test-with-handler 
     header 
     (list "Error occured when calculating expected output value")
     points
     (let* [(expr2-value ((pdp-test-eq-thunk2 test)))
            (header-2 (append header (list "Expected Output Value:"
                                           (format-code expr2-value))))]
       (test-with-handler 
        header-2
        (list "Error occured when calculating result")
        points
        (let [(expr1-value ((pdp-test-eq-thunk1 test)))]
          (test-with-handler 
           header-2
           (list "Error occured when testing equality")
           points
           (if (equal? expr1-value expr2-value)
               (make-pdp-test-result (append header-2 (list "Correct")) 'success 1 0 0 empty points points)
               (make-pdp-test-result 
                (append header-2 
                        (list "Wrong Output:"
                              (format-code expr1-value)))
                'fail 0 1 0 empty 0 points)))))))))

(define (pdp-test-within->results defs partial? test)
  (let* ([points (pdp-test-within-points test)]
         [plusminus (pdp-test-within-plusminus test)]
         [header 
          (append
           (test-caption (format "within-range (+/- ~a)" plusminus) partial? points)
           (list
            (string-append "@para{" (pdp-test-within-explanation test) "}")
            "Input:"
            (format-code (pdp-test-within-expr1 test))
            (format "Expected Output (+/- ~a):" plusminus)
            (format-code (pdp-test-within-expr2 test))))])
    (test-with-handler 
     header 
     (list "Error occured when calculating expected output value/range")
     points
     (let* [(expr2-value ((pdp-test-within-thunk2 test)))
            (plusminus-value ((pdp-test-within-thunk3 test)))
            (header-2 (append header (list  (format "Expected Output Value (+/- ~a):" plusminus)
                                            (format "@para{Between ~a and ~a}" 
                                                    (- expr2-value plusminus-value)
                                                    (+ expr2-value plusminus-value)))))]
       (test-with-handler 
        header-2
        (list "Error occured when calculating result")
        points
        (let [(expr1-value ((pdp-test-within-thunk1 test)))]
          (test-with-handler 
           header-2
           (list "Error occured when testing whether the value is in range")
           points
           (if (and (>= expr1-value (- expr2-value plusminus-value)) (<= expr1-value (+ expr2-value plusminus-value)))
               (make-pdp-test-result (append header-2 (list (format "Correct Output: ~a" expr1-value))) 'success 1 0 0 empty points points)
               (make-pdp-test-result 
                (append header-2 
                        (list "Wrong Output:"
                              (format-code expr1-value)))
                'fail 0 1 0 empty 0 points)))))))))

(define (pdp-test-error->results defs partial? test)
  (let* ([points (pdp-test-error-points test)]
         [header 
          (append
           (test-caption "error" partial? points)
           (list
            (string-append "@para{" (pdp-test-error-explanation test) "}")
            "Input:"
            (format-code (pdp-test-error-expr test))
            "Expected Error should match:"
            (format-code (pdp-test-error-pred test))))])
    (test-with-handler 
     header 
     (list "Error occured when calculating error predicate")
     points
     (let* [(expr2-value ((pdp-test-error-thunk2 test)))
            (header-2 header)]
       (test-with-handler
        header-2
        (list "Error occured when calculating result")
        points
        (with-handlers
            ([expr2-value 
              (lambda (x) 
                (make-pdp-test-result (append header-2 (list "Correct")) 'success 1 0 0 empty points points))])
          (let [(expr1-value ((pdp-test-error-thunk1 test)))]
            (make-pdp-test-result 
             (append header-2 
                     (list "Wrong Output:"
                           (format-code expr1-value)))
             'fail 0 1 0 empty 0 points))))))))

(define (pdp-test-pred->results defs partial? test)
  (let* ([points (pdp-test-pred-points test)]
         [header 
          (append
           (test-caption "predicate" partial? points)
           (list
            (string-append "@para{" (pdp-test-pred-explanation test) "}")
            "Input:"
            (format-code (pdp-test-pred-expr test))
            "Output should match:"
            (format-code (pdp-test-pred-pred test))))])
    (test-with-handler 
     header 
     (list "Error occured when calculating predicate")
     points
     (let* [(expr2-value ((pdp-test-pred-thunk2 test)))
            (header-2 header)]
       (test-with-handler
        header-2
        (list "Error occured when calculating result")
        points
        (let [(expr1-value ((pdp-test-pred-thunk1 test)))]
          (test-with-handler
           header-2
           (list "Error when checking result with predicate")
           points
           (if (expr2-value expr1-value)
               (make-pdp-test-result (append header-2 (list "Correct")) 'success 1 0 0 empty points points)
               (make-pdp-test-result 
                (append header-2 
                        (list "Wrong Output:"
                              (format-code expr1-value)))
                'fail 0 1 0 empty 0 points)))))))))

(define (pdp-test-nonterm->results defs partial? test)
  (let* ([points (pdp-test-nonterm-points test)]
         [waittime (pdp-test-nonterm-waittime test)]
         [header 
          (append
           (test-caption (format "non-termination (~a seconds)" waittime) partial? points)
           (list
            (string-append "@para{" (pdp-test-nonterm-explanation test) "}")
            "Input:"
            (format-code (pdp-test-nonterm-expr test))))])
    (test-with-handler 
     header 
     (list "Error occured when calculation expression result")
     points
     (let* [(expr-value ((pdp-test-nonterm-thunk test)))]
       (if (equal? expr-value true)
           (make-pdp-test-result (append header (list "Correct")) 'success 1 0 0 empty points points)
           (make-pdp-test-result 
            (append header
                    (list "Terminated with result:"
                          (format-code expr-value)))
            'fail 0 1 0 empty 0 points))))))

(define (pdp-test-or->results defs partial? test)
  (let ([sub-results (map (test->results defs 'no)
                          (pdp-test-or-tests test))]
        [points (pdp-test-or-points test)])
    (prepend-test-result-lines
     (combine-test-results-or* points sub-results)
     (append
      (test-caption "or" partial? points)
      (list (string-append "@para{" (pdp-test-or-explanation test) "}"))))))

(define (pdp-test-and->results defs partial? test)
  (let ([sub-results (map (test->results defs 'no)
                          (pdp-test-and-tests test))]
        [points (pdp-test-and-points test)])
    (prepend-test-result-lines
     (combine-test-results-and* points sub-results)
     (append
      (test-caption "and" partial? points)
      (list (string-append "@para{" (pdp-test-and-explanation test) "}"))))))

(define (test-caption type partial? points)
  (list
   (if (not (equal? partial? 'no))
       (if (> points 0)
           (format "@subsubsection{Test (~a, ~a~a points)}"
                   type
                   points
                   (if partial? " partial" ""))
           (format "@subsubsection{Test (~a)}" type))
       (format "@subsubsub*section{Test (~a)}" type))
   (if partial? "" 'MARGIN-NOTE)))

(define-syntax test-with-handler
  (syntax-rules ()
    [(_ header errlines points expr)
     (with-handlers  
         [((lambda (x) true) 
           (lambda (x) (make-pdp-test-result 
                        (append header errlines (list (format-code x)))
                        'error
                        0
                        0
                        1
                        (list (format "Error: ~a" x))
                        0
                        points)))]
       expr)]))

(define (combine-test-results tr1 tr2)
  (make-pdp-test-result
   (append (pdp-test-result-lines tr1) (pdp-test-result-lines tr2))
   (if (equal? (pdp-test-result-type tr1) (pdp-test-result-type tr2))
       (pdp-test-result-type tr1)
       (if (equal? (pdp-test-result-type tr1) 'neutral)
           (pdp-test-result-type tr2)
           (if (equal? (pdp-test-result-type tr2) 'neutral)
               (pdp-test-result-type tr1)
               'partial)))
   (+ (pdp-test-result-success-count tr1) (pdp-test-result-success-count tr2))
   (+ (pdp-test-result-fail-count tr1) (pdp-test-result-fail-count tr2))
   (+ (pdp-test-result-error-count tr1) (pdp-test-result-error-count tr2))
   (append (pdp-test-result-error-messages tr1) (pdp-test-result-error-messages tr2))
   (+ (pdp-test-result-achieved-points tr1) (pdp-test-result-achieved-points tr2))
   (+ (pdp-test-result-total-points tr1) (pdp-test-result-total-points tr2))))

(define (combine-test-results* trs)
  (foldr combine-test-results empty-tr trs))

(define (combine-test-results-and* points trs)
  (let ([ctr (foldr combine-test-results empty-tr trs)])
    (if (equal? (pdp-test-result-type ctr) 'success)
        (make-pdp-test-result (pdp-test-result-lines ctr)
                              'success
                              (pdp-test-result-success-count ctr)
                              0
                              0
                              empty
                              points 
                              points)
        (make-pdp-test-result (pdp-test-result-lines ctr)
                              (pdp-test-result-type ctr)
                              (pdp-test-result-success-count ctr)
                              (pdp-test-result-fail-count ctr)
                              (pdp-test-result-error-count ctr)
                              (pdp-test-result-error-messages ctr)
                              (min points (pdp-test-result-achieved-points ctr))
                              points))))

(define (combine-test-results-or* points trs)
  (let ([any-succ (ormap (lambda (t) (equal? (pdp-test-result-type t) 'success)) trs)]
        [any-fail (ormap (lambda (t) (equal? (pdp-test-result-type t) 'fail)) trs)]
        [ctr (foldr combine-test-results empty-tr trs)])
    (if any-succ
        (make-pdp-test-result (pdp-test-result-lines ctr)
                              'success
                              1
                              0
                              0
                              empty
                              points 
                              points)
        (make-pdp-test-result (pdp-test-result-lines ctr)
                              (if any-fail 'fail 'error)
                              0
                              (if any-fail 1 0)
                              (if any-fail 0 1)
                              (pdp-test-result-error-messages ctr)
                              0
                              points))))


(define (prepend-test-result-lines tr lines)
  (make-pdp-test-result
   (append lines (pdp-test-result-lines tr))
   (pdp-test-result-type tr)
   (pdp-test-result-success-count tr)
   (pdp-test-result-fail-count tr)
   (pdp-test-result-error-count tr)
   (pdp-test-result-error-messages tr)
   (pdp-test-result-achieved-points tr)
   (pdp-test-result-total-points tr)))

(define (result->lines result)
  (append
   (pdp-test-result-lines result)
   (list 
    "@section{Results}"
    (format "Successes: ~a" (pdp-test-result-success-count result))
    (format "@linebreak{} Wrong Outputs: ~a" (pdp-test-result-fail-count result))
    (format "@linebreak{} Errors: ~a" (pdp-test-result-error-count result))
    (format "@linebreak{} Achieved Points: ~a" (pdp-test-result-achieved-points result))
    (format "@linebreak{} Total Points (rounded): ~a/~a" 
            (round (pdp-test-result-achieved-points result))
            (ceiling (pdp-test-result-total-points result))))))

(define (add-margin-note tr)
  (make-pdp-test-result
   (map
    (lambda (line) (if (equal? line 'MARGIN-NOTE)
                       (format "@margin-note[#:left? #t]{~a/~a}"
                               (pdp-test-result-achieved-points tr)
                               (pdp-test-result-total-points tr))
                       line))
    (pdp-test-result-lines tr))
   (pdp-test-result-type tr)
   (pdp-test-result-success-count tr)
   (pdp-test-result-fail-count tr)
   (pdp-test-result-error-count tr)
   (pdp-test-result-error-messages tr)
   (pdp-test-result-achieved-points tr)
   (pdp-test-result-total-points tr)))

(define (format-code code)
  (let ([prnt (if (exn:fail? code) (exn-message code) 
                  (let ([sport (open-output-string)])
                    (pretty-write code sport)
                    (get-output-string sport)))])
    (if (string-contains? "#<" (format "~s" prnt))
        (format "@para{~s}" prnt)
        (format "@racketblock{@~a}" prnt))))

#;(define-pdp-test-suite test-tests "03" "This test suite tests your solution against our requirements."
    (test-file 
     "2.rkt" "We codewalk problem 2" ([a 1])
     (test-group-with-points
      "stock-total-volume" "" 3 ([b (+ 2 5)] [c (+ 2 6)])
      (test-equal? (+ 2 3) (+ 3 2) 2 "2 + 3 should be equal to 3 + 2")
      (test-equal? (+ a b) c 2 "2 + 5 +1 should be equal to 7"))))

#;(run-pdp-tests test-tests)
