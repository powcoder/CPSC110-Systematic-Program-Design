;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2020w1-f-p4-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment f-p4)

(@cwl ???)   ;fill in your CWL here (same CWL you put for 110 problem sets)

(@problem 1) ;THIS IS REALLY PROBLEM 4.  DO NOT EDIT OR DELETE THIS LINE!

;;
;; The program is a (rather boring) version of a whack-a-mole game.  When the
;; game starts there are a number of labelled green squares on the screen; these
;; are the moles.  You may assume that the squares do not intersect (they do
;; not cover each other at all).  Each time the player clicks the mouse over
;; a square that square disappears (the mole was gently encouraged to retreat).
;; Every tick, one square that has previously disappeared reappears, so it can
;; be gently whacked again.
;;
;; You will see that there is ONLY one function in this program that is not
;; complete. It is the only function you need to design for this problem.  You
;; should complete the design of overlapped-by?, starting with the wish-list
;; entry at the end of this file.  Be sure to think carefully about boundary
;; conditions and how many check-expects this function needs to be thoroughly
;; tested.
;;
;; You will note that there are three failing tests in this starter, which are
;; all tests for disappear-mole-at-xy.  They are failing because overlapped-by?
;; is a stub.  These tests should pass when you complete your design of
;; overlapped-by?.
;;
;; You must include all relevant design recipe elements in your design.
;;
;; As always, a file that does not run will lose a significant number of marks.
;; Run your work often, so that you can check and fix errors as soon as they
;; creep in.  Also be sure to run every time before you submit.
;; 

(require 2htdp/image)
(require 2htdp/universe)

;; A simple whack-a-mole game

(@htdw Mole)

;; =================
;; Constants:

(define WIDTH 400)
(define HEIGHT 400)

(define SIZE 80)
(define COLOR  "green")

(define MTS  (empty-scene WIDTH HEIGHT))
(define MOLE (square SIZE "solid" COLOR))



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

(@htdf main)
(@signature (listof Mole) -> (listof Mole))
;; run the whack-a-mole game, starting with a list of moles
;; start the world with (main LOM123456)

;; <NO TESTS FOR MAIN FUNCTIONS>

(@template htdw-main)

(define (main lom)
  (big-bang lom                  ; (listof Mole)
    (on-tick  appear-one-mole 1) ; (listof Mole) -> (listof Mole)
    (to-draw  render-lom)        ; (listof Mole) -> Image
    (on-mouse handle-mouse)))    ; (listof Mole) Integer Integer MouseEvent
;;                                               -> (listof Mole)


(@htdf render-lom)
(@signature (listof Mole) -> Image)
;; render each visible mole at its position on MTS

(check-expect (render-lom empty) MTS)
(check-expect (render-lom LOM12)
              (place-image (mole-image M1) (mole-x M1) (mole-y M1)
                           (place-image (mole-image M2) (mole-x M2) (mole-y M2)
                                        MTS)))
(check-expect (render-lom LOM12)
              (place-mole M1
                          (place-mole M2 MTS)))
(check-expect (render-lom (list M4))
              MTS)

(@template (listof Mole))

(define (render-lom lom)
  (cond [(empty? lom) MTS]
        [else
         (place-mole (first lom)
                     (render-lom (rest lom)))]))


(@htdf place-mole)
(@signature Mole Image -> Image)
;; render the mole at its position on the image if it's visible

(check-expect (place-mole M1 MTS)
              (place-image (mole-image M1) (mole-x M1) (mole-y M1) MTS))
(check-expect (place-mole M2 MTS)
              (place-image (mole-image M2) (mole-x M2) (mole-y M2) MTS))
(check-expect (place-mole M4 MTS)
              MTS)
              
;(define (place-mole m img) img)

(@template Mole)

(define (place-mole m img)
  (if (mole-visible m)
      (place-image (mole-image m) (mole-x m) (mole-y m) img)
      img))

(@htdf mole-image)
(@signature Mole -> Image)
;; produce an image for m, a green box with text on top

(check-expect (mole-image M1) (overlay (text "pset 9" 20 "black") MOLE))
(check-expect (mole-image M3) (overlay (above (text "tandem" 20 "black")
                                              (text "worklists" 20 "black"))
                                       MOLE))

(@template Mole)

(define (mole-image m)
  (overlay (foldr (lambda (t img) (above (text t 20 "black") img))
                  empty-image (mole-label m))
           MOLE))

(@htdf handle-mouse)
(@signature (listof Mole) Integer Integer MouseEvent -> (listof Mole))
;; On button-down, if x,y overlaps a mole make it invisible.
(check-expect (handle-mouse LOM12 50 160 "button-down")
              (list (make-mole  50 150 (list "pset 9") false)
                    M2))
(check-expect (handle-mouse LOM12 50 200 "move")
              LOM12)
(check-expect (handle-mouse (list M4) 350 200 "button-down")
              (list M4))

(@template MouseEvent)

(define (handle-mouse lom x y me)
  (cond [(mouse=? me "button-down")
         (disappear-mole-at-xy lom x y)]
        [else lom]))

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
;; Complete the design of this function. If mx and my are the x and y position
;; of the mole then a mole is considered to overlap an x, y position if:
;;       x is in [mx - SIZE/2, mx + SIZE/2]  
;;  AND  y is in [my - SIZE/2, my + SIZE/2]
;;
;; Remember that the notation [5, 12] means numbers in the range between
;; 5 and 12, INCLUDING 5 and 12.
;;

(@htdf overlapped-by?)
(@signature Mole Integer Integer -> Boolean)
;; Produce true if the mole is overlapped by the x, y position
;; !!!

(define (overlapped-by? m x y) true) ; stub
