;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2020W2-MT1-P4-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment mt1-p4)

(@cwl ???)   ;fill in your CWL here (same CWL you put for 110 problem sets)

(@problem 1) ;this is actually problem 4.1 - DO NOT EDIT OR DELETE THIS LINE!

;;
;; PLEASE READ THESE DIRECTIONS SLOWLY AND CAREFULLY AT LEAST TWO TIMES BEFORE
;; PROCEEDING.  FAILURE TO FOLLOW THESE DIRECTIONS MAY CAUSE YOU TO LOSE ALL
;; MARKS FOR THIS PROBLEM.
;;
;; In this problem we are giving you a partially complete world program.  We
;; want you to complete the program by doing three things:
;;  1. Fix a mistake in the design for add-balloon. This is a small (one line)
;;     change.  If you find yourself doing more than one line you are doing
;;     too much.
;;  2. Fix two mistakes in the design of next-balloons and/or next-balloon.
;;     This is a larger change, but still somewhat localized.
;;  3. Complete the design of render-balloons. This requires completing a
;;     function design starting from the wish list entry.
;;
;; NOTE CAREFULLY:
;;
;; - Except for the @cwl annotation, do not edit any part of this file above
;;   the marked line (right after the main function definition). Editing any
;;   part of the file above the marked line 143 will cause you to lose all
;;   marks for this problem.
;;
;; - Your goal is to fix and complete THIS PROGRAM.
;; - In 1 and 2 you should fix the mistakes, so that the functions work
;;   properly (as stated in the purpose) and are consistent with design rules.
;; - In 3 of course you will have to do more work, but again be sure to stay
;;   consistent with the program we are giving you.  Don't improve it, complete
;;   it.
;;
;; - We recommend you solve the three parts in the 1, 2, 3 order.
;;
;; - You MUST follow all applicable design rules.  Correctly working functions
;;   that ignore the recipes will earn few if any marks. It might be the case
;;   that the person who wrote the code below did not always follow the
;;   recipes.  Just think, if they had, then your task would be easier!
;;



;; place multiple balloons that slowly grow until they pop

(@htdw ListOfBalloon)
;;==========================================

;; Constants

(define WIDTH 400)
(define HEIGHT 600)

(define FALL-SPEED 2)     ;balloon falls this many pixels  per tick
(define GROW-SPEED 1)     ;balloon radius increases pixels per tick

(define INITIAL-RADIUS 10);starting radius
(define MAX-RADIUS 100)   ;balloon can = this radius, but pops > here


(define MTS (empty-scene WIDTH HEIGHT))




;;=================================================

;; Data Definitions:

(@htdd Balloon)

(define-struct balloon (x y r))
;; Balloon is one of:
;;  - false
;;  - (make-balloon Number Number Number)
;; interp.
;;   false means the balloon has popped (no meaningful x, y or radius)
;;   otherwise the balloon's x and y coordinates and radius, all in pixels
;;   REMEMBER y increases going down
;;   CONSTRAINT: r <= MAX-RADIUS, past this balloon is represented as false
;;   NOTE: no constraints on x, y. We don't care if balloons leave window

(define B1 false)
(define B2 (make-balloon 100 50 23))
(define B3 (make-balloon 20 30 50))

(@dd-template-rules one-of            ;2 cases
                    atomic-distinct   ;false
                    compound)         ;3 fields

(define (fn-for-balloon b)
  (cond [(false? b) (...)]
        [else
         (... (balloon-x b) 
              (balloon-y b) 
              (balloon-r b))]))



(@htdd ListOfBalloon)
;; ListOfBalloon is one of:
;; - empty
;; - (cons Balloon ListOfBalloon)
;; interp. a list of balloons
(define LOB1 empty)
(define LOB2 (cons B1 empty))
(define LOB3 (cons B1 (cons B2 empty)))

(@dd-template-rules one-of           ;2 cases
                    atomic-distinct  ;empty
                    compound         ;cons
                    ref              ;(first lob) is Balloon
                    self-ref)        ;(rest lob) is ListOfBalloon

(define (fn-for-lob lob)
  (cond [(empty? lob)(...)]
        [else
         (... (fn-for-balloon (first lob))
              (fn-for-lob (rest lob)))]))



;;===================================================

;; Functions   

(@htdf main)
(@signature ListOfBalloon -> ListOfBalloon)
;; start the world with (main empty)

(@template htdw-main)

(define (main lob)
  (big-bang lob               ;ListOfBalloon
    (on-tick  next-balloons)  ;ListOfBalloon -> ListOfBalloon
    (to-draw  render-balloons);ListOfBalloon -> Image
    (on-mouse add-balloon)))  ;ListOfBalloon Integer Integer MouseEvent
;;                            ;   -> ListOfBalloon


;; DO NOT EDIT ANYTHING ABOVE HERE
;; DO NOT EDIT ANYTHING ABOVE HERE
;; DO NOT EDIT ANYTHING ABOVE HERE

(@htdf add-balloon)
(@signature ListOfBalloon Integer Integer MouseEvent -> ListOfBalloon)
;; on mouse click add a Balloon at mouse x, y with INITIAL-RADIUS
(check-expect (add-balloon (cons (make-balloon 1 2 3)
				 (cons false
				       empty))
                           10 40
                           "button-down")
              (cons (make-balloon 10 40 INITIAL-RADIUS)
                    (cons (make-balloon 1 2 3)
			  (cons false
                                empty))))

(check-expect (add-balloon empty 90 100 "drag") empty)

;(define (add-balloon lob x y me) lob) ;stub

(@template MouseEvent)

(define (add-balloon lob x y me)
  (cond [(mouse=? me "button-down")
         (cons (make-balloon y x INITIAL-RADIUS) lob)]
        [else lob]))


(@problem 2) ;this is actually problem 4.2 - DO NOT EDIT OR DELETE THIS LINE!

(@htdf next-balloons)
(@signature ListOfBalloon -> ListOfBalloon)
;; produce the next lob by producing the next balloon for every balloon
(check-expect (next-balloons empty) empty)
(check-expect (next-balloons (cons (make-balloon 10 20 30)
                                   (cons (make-balloon 30 40 MAX-RADIUS)
                                         (cons false
                                               empty))))
              (cons (make-balloon 10 (+ 20 FALL-SPEED)(+ 30 GROW-SPEED))
                    (cons false
                          (cons false
                                empty))))

;(define (next-balloons lob) lob) ;stub

(@template ListOfBalloon)

(define (next-balloons lob)
  (cond [(empty? lob) empty]
        [else
         (cons (next-balloon (first lob))
               (next-balloons (rest lob)))]))

(@htdf next-balloon)
(@signature Balloon -> Balloon)
;; move y by FALL-SPEED; grow radius by GROW-SPEED, pop if > MAX-RADIUS
(check-expect (next-balloon false) false)
(check-expect (next-balloon (make-balloon 10 20 30))
              (make-balloon 10 (+ 20 FALL-SPEED) (+ 30 GROW-SPEED)))
(check-expect (next-balloon (make-balloon 5 10 (- MAX-RADIUS GROW-SPEED)))
              (make-balloon 5 (+ 10 FALL-SPEED) MAX-RADIUS))
(check-expect (next-balloon (make-balloon 10 20 MAX-RADIUS))
              false)

;(define (next-balloon b) b) ;stub

(define (next-balloon b)
  (if (< (+ (balloon-r b) GROW-SPEED) MAX-RADIUS)
      (make-balloon (balloon-x b)
                    (+ (balloon-y b) FALL-SPEED)
                    (+ (balloon-r b) GROW-SPEED))
      false))


(@problem 3) ;this is actually problem 4.3 - DO NOT EDIT OR DELETE THIS LINE!

(@htdf render-balloons)
(@signature ListOfBalloon -> Image)
;; render all balloons as green circle radius r at x,y, if popped show nothing
;; !!!

(define (render-balloons lob) MTS)





