;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2020W1-MT1-P1-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment mt1-p1)

(@cwl ???)   ;fill in your CWL here (same CWL you put for 110 problem sets)

(@problem 1) ;Actually problem 2, DO NOT EDIT OR DELETE this line

;;
;; Consider the following type comment and partial interpretation:
;;

;; Foo is one of:
;;  - Number
;;  - "x"
;;  - "y"
;;  - "z"
;;  - Boolean
;; interp. ...
;;         If a Number is in [1, 100]


(@dd-template-rules one-of              ;5 cases
		    atomic-non-distinct ;Number
		    atomic-distinct     ;"x"
		    atomic-distinct     ;"y"
		    atomic-distinct     ;"z"
		    atomic-non-distinct);Boolean


(define (fn-for-foo f)
  (cond [(number? f) (... f)]
        [(and (string? f) (string=? f "x")) (...)]
        [(and (string? f) (string=? f "y")) (...)]
        [(string? f)                        (...)]
        [else (... f)]))

#;
(define (fn-for-foo f)
  (cond [(number? f) (... f)]
        [(and (string? f) (string=? f "x")) (...)]
        [(and (string? f) (string=? f "y")) (...)]
        [(and (string? f) (string=? f "z")) (...)]
        [else (... f)]))
