;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2020w1-f-p5-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment f-p5)

(@cwl ???)   ;fill in your CWL here (same CWL you put for 110 problem sets)

(@problem 1) ;THIS IS REALLY PROBLEM 5.  DO NOT EDIT OR DELETE THIS LINE!


(require 2htdp/image)
(require 2htdp/universe)

;;
;; This problem focuses on a SMALL PART OF THE WORLD PROGRAM in problem 4.
;; To make things easier for you we removed things from the starter file
;; that you do not need to solve this problem.
;;
;; Skip down to the function section for the problem description.
;;


;; =================
;; Data definitions:

(@htdd Mole)
(define-struct mole (x y label visible))
;; Mole is (make-mole Integer Integer (listof String) Boolean)
;; interp. a mole (or square) on the screen, with x,y in screen coordinates
;;         a list of strings making up a text label (one string per line)
;;         and a boolean flag indicating whether it is visible
(define M1 (make-mole  50 150 (list "pset 9") true))
(define M2 (make-mole 200  50 (list "midterm" "2") true))
(define M3 (make-mole 350 100 (list "tandem" "worklists") false))
(define M4 (make-mole 50  300 (list "Zoom") false))
(define M5 (make-mole 150 250 (list "COVID" "19") true))
(define M6 (make-mole 350 350 (list "2020") true))

#;
(define (fn-for-mole m)
  (... (mole-x m)
       (mole-y m)
       (mole-label m)
       (mole-visible m)))


;; (listof Mole)
(define LOM123456 (list M1 M2 M3 M4 M5 M6))
(define LOM12 (list M1 M2))

;; =================
;; Functions:


;;
;; There is some redunancy in the bodies of the appear-one-mole and
;; disappear-mole-at-xy functions.  We want you to produce the design
;; for an abstract function based on these two functions.  We want you
;; to:
;;
;;   - design a new function called find-and-replace based on
;;     abstraction from these two examples
;;   - your design of find-and-replace should include signature,
;;     purpose, tests, template tag and function
;;   - then go back and revise the function definitions for appear-one-mole
;;     and disappear-mole-at-xy so that they both call find-and-replace
;;     You will also want to update the @template tag for appear-one-mole
;;     and disappear-mole-at-xy
;;   - DO NOT ADD ANY NEW TESTS FOR APPEAR-ONE-MOLE OR DISAPPEAR-MOLE-AT-XY
;;     as part of the refactoring.
;;   - If you write tests for find-and-replace that end up calling
;;     overlapped-by? you must make sure to only call overlapped-by? with
;;     the same arguments that the existing tests call it with.  We have
;;     a special stub for overlapped-by? at the end of the file and it
;;     only works for those arugments.
;;
;;
;; As always, a file that does not run will lose a significant number of marks.
;; Run your work often, so that you can check and fix errors as soon as they
;; creep in.  Also be sure to run every time before you submit.
;;

(@htdf appear-one-mole)
(@signature (listof Mole) -> (listof Mole))
;; If an invisible mole is in lom, make the first one appear

(check-expect (appear-one-mole empty) empty)
(check-expect (appear-one-mole (list (make-mole 200 300 "abc" true)))
              (list (make-mole 200 300 "abc" true)))
(check-expect (appear-one-mole (list (make-mole 200 300 "abc" false)))
              (list (make-mole 200 300 "abc" true)))
(check-expect (appear-one-mole (list (make-mole 200 300 "abc" true)
                                     (make-mole 300 400 "def" true)))
              (list (make-mole 200 300 "abc" true)
                    (make-mole 300 400 "def" true)))
(check-expect (appear-one-mole (list (make-mole 300 400 "def" true)
                                     (make-mole 200 300 "abc" false)))
              (list (make-mole 300 400 "def" true)
                    (make-mole 200 300 "abc" true)))

(@template (listof Mole))

(define (appear-one-mole lom)
  (cond [(empty? lom) empty]
        [else
         (if (not (mole-visible (first lom)))
             (cons (appear (first lom))
                   (rest lom))
             (cons (first lom)
                   (appear-one-mole (rest lom))))]))

(@signature Mole -> Mole)
;; produce mole w/ same x, y and label, but visible
(check-expect (appear (make-mole 10 20 (list "x") false))
              (make-mole 10 20 (list "x") true))
(check-expect (appear (make-mole 30 40 (list "y") true))
              (make-mole 30 40 (list "y") true))

(@template Mole)

(define (appear m)
  (make-mole (mole-x m)
             (mole-y m)
             (mole-label m)
             true))

(@htdf disappear-mole-at-xy)
(@signature (listof Mole) Integer Integer -> (listof Mole))
;; If there is a mole overlapped by x,y disappear it.
;; CONSTRAINT: moles may not overlap

(check-expect (disappear-mole-at-xy empty 100 200) empty)
(check-expect (disappear-mole-at-xy (list (make-mole 300 400 (list "y") true))
                                    301 403)
              (list (make-mole 300 400 (list "y") false)))
(check-expect (disappear-mole-at-xy (list (make-mole 100 200 (list "x") true))
                                    101 103)
              (list (make-mole 100 200 (list "x") true)))
(check-expect (disappear-mole-at-xy (list (make-mole 100 200 (list "x") true)
                                          (make-mole 300 400 (list "y") true))
                                    301 403)
              (list (make-mole 100 200 (list "x") true)
                    (make-mole 300 400 (list "y") false)))
(check-expect (disappear-mole-at-xy (list (make-mole 100 200 (list "x") true)
                                          (make-mole 300 400 (list "y") true))
                                    101 103)
              (list (make-mole 100 200 (list "x") true)
                    (make-mole 300 400 (list "y") true)))

(@template (listof Mole))

(define (disappear-mole-at-xy lom x y)
  (cond [(empty? lom) empty]
        [else
         (if (overlapped-by? (first lom) x y)
             (cons (disappear (first lom))
                   (rest lom))
             (cons (first lom)
                   (disappear-mole-at-xy (rest lom) x y)))]))


(@signature Mole -> Mole)
;; produce mole w/ same x, y and label, but not visible
(check-expect (disappear (make-mole 10 20 (list "x") false))
              (make-mole 10 20 (list "x") false))
(check-expect (disappear (make-mole 30 40 (list "y") true))
              (make-mole 30 40 (list "y") false))

(@template Mole)

(define (disappear m)
  (make-mole (mole-x m)
             (mole-y m)
             (mole-label m)
             false))




;;
;; THIS IS A SPECIAL STUB THAT is smart enough (in a stupid way) to make
;; sure that the above tests for disappear-mole-at-xy pass.  It's no smarter
;; than that, and is NOT A CLUE TO HOW TO SOLVE PROBLEM 4 OF THE EXAM.  It
;; is just good enough for the tests above to pass pre-and -post the
;; refactoring, which is good enough for you to solve the problem above.
;; YOU ARE VERY STRONGLY ENCOURAGED NOT TO WASTE YOUR TIME LOOKING AT THE
;; FUNCTION BODY FOR THIS STUB DURING THE EXAM.
;; 
(@htdf overlapped-by?)

(@signature Mole Integer Integer -> Boolean)
;; Produce true if the mole is overlapped by the x, y position
;; !!! advanced stub to handle a few specific cases
(define (overlapped-by? m x y)
  (local [(define table
            (list
             (list (list (make-mole 300 400 (list "y") true) 301 403) #t)
             (list (list (make-mole 100 200 (list "x") true) 101 103) #f)
             (list (list (make-mole 100 200 (list "x") true) 301 403) #f)
             (list (list (make-mole 100 200 (list "x") true) 101 103) #f)
             (list (list (make-mole 300 400 (list "x") true) 101 103) #f)))]
    (if (not (false? (assoc (list m x y) table)))
        (cadr (assoc (list m x y) table))
        #f)))
