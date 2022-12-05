;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2020W1-MT1-P5-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/image)

(@assignment mt1-p3)

(@cwl ???)   ;fill in your CWL here (same CWL you put for 110 problem sets)

(@problem 1) ;this is actually problem 3 - DO NOT EDIT OR DELETE THIS LINE!

;; Given the following definitions:

(define FOO 7)
(define BAR 6)

(define (alpha x)
  (if (> x 3)
      (beta x)
      FOO))

(define (beta y)
  (* y BAR))

;;
;; Show the step by step evaluation of the following expression.
;;
;; None of your steps should be commented out. Put another way, 
;; do not use any comments of any kind below.  Also you program
;; must run without errors.
;; 

(alpha (* 3 (+ 2 4)))

(alpha (* 3 6))

(alpha 18)

(if (> 18 3)
    (beta 18)
    FOO)

(if true
    (beta 18)
    FOO)

(beta 18)

(* 18 BAR)

(* 18 6)

108
