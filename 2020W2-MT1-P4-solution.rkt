;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2020W2-MT1-P4-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment mt1-p4)

(@cwl ???)   ;fill in your CWL here (same CWL you put for 110 problem sets)

(@problem 1) ;this is actually problem 4.1 - DO NOT EDIT OR DELETE THIS LINE!

;; place multiple balloons that slowly grow until they pop

(@htdw ListOfBalloon)
;;==========================================

;; Constants

(define WIDTH 400)
(define HEIGHT 600)

(define INITIAL-RADIUS 10)
(define MAX-RADIUS 100)   ;ballon can = this radius, but pops > here

(define FALL-SPEED 2)     ;pixels  per tick
(define GROW-SPEED 1)     ;radius pixels per tick

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
;;   otherwise the balloons x and y coordinates and radius, all in pixels
;;   REMEMBER Y increases going down
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
         (cons (make-balloon x y INITIAL-RADIUS) lob)]
        [else lob]))


(@problem 2)

(@htdf next-balloons)
(@signature ListOfBalloon -> ListOfBalloon)
;; produce the next lob by ticking clock for every balloon in lob
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
;; produce next Balloon, increase x by FALL-SPEED, r by GROW-SPEED
(check-expect (next-balloon false) false)
(check-expect (next-balloon (make-balloon 10 20 30))
              (make-balloon 10 (+ 20 FALL-SPEED) (+ 30 GROW-SPEED)))
(check-expect (next-balloon (make-balloon 5 10 (- MAX-RADIUS GROW-SPEED)))
              (make-balloon 5 (+ 10 FALL-SPEED) MAX-RADIUS))
(check-expect (next-balloon (make-balloon 10 20 MAX-RADIUS))
              false)

;(define (next-balloon b) b) ;stub

(@template Balloon)

(define (next-balloon b)
  (cond [(false? b) false]
        [else
         (if (<= (+ (balloon-r b) GROW-SPEED) MAX-RADIUS)
             (make-balloon (balloon-x b)
                           (+ (balloon-y b) FALL-SPEED)
                           (+ (balloon-r b) GROW-SPEED))
             false)]))


(@problem 3)

(@htdf render-balloons)
(@signature ListOfBalloon -> Image)
;; Place each balloon on MTS
(check-expect (render-balloons empty) MTS)
(check-expect (render-balloons (cons (make-balloon 10 20 30)
                                     (cons (make-balloon 20 30 40)
                                           (cons false
						 (cons (make-balloon 50 60 70)
						       empty)))))
              (place-image (circle 30 "solid" "green")
                           10 20
                           (place-image (circle 40 "solid" "green")
                                        20 30
					(place-image (circle 70 "solid" "green")
						     50 60
						     MTS))))

;(define (render-balloons lob) MTS) ;stub

(@template ListOfBalloon)

(define (render-balloons lob)
  (cond [(empty? lob) MTS]
        [else
         (place-balloon (first lob)
                        (render-balloons (rest lob)))]))


(@htdf place-balloon)
(@signature Balloon Image -> Image)
;; Place balloon image of radius r, at x, y on img
(check-expect (place-balloon false MTS) MTS)
(check-expect (place-balloon false empty-image) empty-image)
(check-expect (place-balloon (make-balloon 10 20 30) MTS)
              (place-image (circle 30 "solid" "green") 10 20 MTS))
(check-expect (place-balloon (make-balloon 10 20 30) empty-image)
              (place-image (circle 30 "solid" "green")
			   10 20
			   empty-image))

;(define (place-balloon b img) img) ;stub

(@template Balloon)

(define (place-balloon b img)
  (cond [(false? b) img]
        [else
         (place-image (circle (balloon-r b) "solid" "green")
                      (balloon-x b)
                      (balloon-y b)
                      img)]))



