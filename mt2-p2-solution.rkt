;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2020w1-mt2-p2-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment mt2-p2)

(@cwl ???)   ;fill in your CWL here (same CWL you put for 110 problem sets)

(@problem 1) ;THIS IS REALLY PROBLEM 2.  DO NOT EDIT OR DELETE THIS LINE!

;; Consider the following definition.  Then answer the 5 multiple-choice
;; questions that follow.  Provide your answers by entering the appropriate
;; value for the constants my-answer-question-1 through my-answer-question-5.
;; For example:  (define my-answer-question-1 "e").

;; As always, a file that does not run will lose a significant number of marks.
;; Run your work often, so that you can check and fix errors as soon as they
;; creep in.  Also be sure to run every time before you submit.

;; We have added special tests at the end of the file that check whether your
;; answers are well-formed.  Any submission that fails any of those tests will
;; receive a grade of 0.

(define (a lst)
  (map b lst))

(define (b n)
  (local [(define (one x) (* x 1))
          (define (two x) (* x b))
          (define (three x) (* x n n n))
          (define a n)
          (define b (* n n))
          (define c (* n n n))]
    (one (two (three n)))))


;; Question 1:
;; Is one a closure?
;; [a] Yes
;; [b] No

(define my-answer-question-1 "b")


;; Question 2:
;; Is two a closure?
;; [a] Yes
;; [b] No

(define my-answer-question-2 "a")


;; Question 3:
;; Is three a closure?
;; [a] Yes
;; [b] No

(define my-answer-question-3 "a")


;; Question 4:
;; How many definitions are lifted each time b is called?
;; [a] 1
;; [b] 2
;; [c] 3
;; [d] 4
;; [e] 6

(define my-answer-question-4 "e")


;; Question 5:
;; In total, how many definitions are lifted when the expression
;; (a (list 4 5)) is evaluated?  Replace the 00 below with the
;; correct number

(define my-answer-question-5 12)

;;============================================================================
;;============================================================================
;;============================================================================
;; You should ignore this code.  Do not read or edit below here.

;; This code is here so that running the file will verify that your answer is
;; well-formed.
(check-satisfied my-answer-question-1
		 (λ (a)
		   (member? a (list "a" "b"))))
(check-satisfied my-answer-question-2
		 (λ (a)
		   (member? a (list "a" "b"))))
(check-satisfied my-answer-question-3
		 (λ (a)
		   (member? a (list "a" "b"))))
(check-satisfied my-answer-question-4
		 (λ (a)
		   (member? a (list "a" "b" "c" "d" "e"))))  
(check-satisfied my-answer-question-5
		 integer?)
