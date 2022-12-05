;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2020W1-MT1-P1-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment mt1-p1)

(@cwl ???)   ;fill in your CWL here (same CWL you put for 110 problem sets)

(@problem 1) ;DO NOT DELETE this line

;;
;; First read through the following PARTIAL data definition.  You do NOT need
;; to, and you MUST NOT edit or complete the data definition.
;;

(@htdd QualComp)
;; QualComp is one of:
;;  - "less-than"
;;  - "equal"
;;  - "greater-than"
;; interp. a qualitative difference between two quantities
;; <examples are redundant for enumerations>


(@htdf compare)
(@signature Number Number -> QualComp)
;; produce qualitative comparison of n1 to n2
(check-expect (compare 3.5  9) "less-than")
(check-expect (compare 10  10) "equal")
(check-expect (compare 11   4) "greater-than")

;(define (compare n1 n2) "less-than") ;stub

(@template Number)

(define (compare n1 n2)
  (cond [(< n1 n2) "less-than"]
	[(= n1 n2) "equal"]
	[else "greater-than"]))
