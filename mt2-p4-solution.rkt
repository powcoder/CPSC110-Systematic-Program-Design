;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mt2-p4-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment mt2-p4)

(@cwl ???)   ;fill in your CWL here (same CWL you put for 110 problem sets)

(@problem 1) ;THIS FILE ACTUALLY HAS 5, 6 & 7.  DO NOT EDIT OR DELETE THIS LINE!


;;
;; Complete the design of the following function by writing the template tag
;; and the function definition.  Your answer must use built-in abstract
;; functions.  Any answer that includes any part of the recursive Natural
;; template or the (listof X) template will receive 0 marks.  The best answer
;; is a composition of two built-in abstract functions.
;;

(@htdf sum-interval)
(@signature Integer Integer -> Integer)
;; Produce the sum of the integers in [lo, hi].
;; CONSTRAINT: lo <= hi
(check-expect (sum-interval 5 5) (+ 5))
(check-expect (sum-interval 0 1) (+ 0 1))
(check-expect (sum-interval 3 6) (+ 3 4 5 6))
(check-expect (sum-interval -8 -6) (+ -8 -7 -6))

;(define (sum-interval lo hi) 0)

(@template fn-composition use-abstract-fn)

;; NOTE: We write the solution with lambda to help you learn lambda.
;;       During the exam we expected you to use local, not lambda.

(define (sum-interval lo hi)
  (foldr + 0
         (build-list (+ (- hi lo) 1)
                     (lambda (x) (+ lo x)))))




(@problem 2) ;THIS IS REALLY PROBLEM 6.  DO NOT EDIT OR DELETE THIS LINE!

(@htdf set-difference)
(@signature (listof Number) (listof Number) -> (listof Number))
;; produce list of the elements in lst1 that are not in lst2
;; CONSTRAINT: no duplicates in lst1; and no duplicates in lst2                         		
(check-expect (set-difference (list 1 3 7 2) (list 1 2)) (list 3 7))
(check-expect (set-difference (list) (list 3 5)) (list))
(check-expect (set-difference (list 3 5) (list)) (list 3 5))

(@template use-abstract-fn fn-composition)

(define (set-difference lst1 lst2)
  (filter (lambda (i1)
            (not
             (ormap (lambda (i2) (= i1 i2))
                    lst2)))
          lst1))



(@problem 3) ;THIS IS REALLY PROBLEM 7.  DO NOT EDIT OR DELETE THIS LINE!

;;
;; Complete the design of the following function by writing the template tag
;; and the function definition.  Your answer must use built-in abstract
;; functions.  Any answer that includes any part of the recursive Natural
;; template or the (listof X) template will receive 0 marks.
;;

(@htdd Countdown)
;; Countdown is one of:
;;  - false
;;  - Natural
;; interp. A countdown to 0 indicating number of seconds left, or false
;;         to indicate the countdown ended more than one second ago. So
;;         they countdown  5, 4, 3, 2, 1, 0, false...

(@htdf next-countdowns)
(@signature (listof Countdown) -> (listof Countdown))
;; advance every countdown, sequence is ..., 3, 2, 1, 0, false, <remove>
(check-expect (next-countdowns empty) empty)
(check-expect (next-countdowns (list 0 3 1 false 2)) (list false 2 0    1))

(@template fn-composition use-abstract-fn)

(define (next-countdowns loc)
  (local [;; (@template Countdown)
          (define (next-c c)
            (cond [(false? c) false]
                  [else
                   (if (zero? c)
                       false
                       (sub1 c))]))]
	 (map next-c
	      (filter (lambda (c) (not (false? c))) loc))))


;;
;; Just as an aside, functions like (lambda (c) (not (false? c))) can
;; be built easily using compose.  
;;
#;
(define (next-countdowns loc)
  (local [(define (next-c c)
            (cond [(false? c) false]
                  [else
                   (if (zero? c)
                       false
                       (sub1 c))]))]
    (map next-c 
         (filter (compose not false?)
                 loc))))




