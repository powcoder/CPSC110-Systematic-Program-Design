;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2020w1-f-p6-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment f-p6)

(@cwl ???)   ;fill in your CWL here (same CWL you put for 110 problem sets)

(@problem 1) ;THIS IS REALLY PROBLEM 6.  DO NOT EDIT OR DELETE THIS LINE!
#|
This problem involves a fair amount of careful thinking and then a small amount
of editing.  Work slowly and systematically, and do not make it harder than it
needs to be.

Below is a function to solve nqueens puzzles.  In an nqueens puzzle the goal is
to try to find a way to put n chess queens on an nxn chess board. This code
works properly as it appears below.  It has the behaviour of trying to solve the
puzzle for the given board size.  If it can't it fails by producing false; if it
can it produces the first set of queen positions it found that solve the puzzle.
There are no bugs or any other hidden problems with this code.

In working on this program, one interesting question to consider is how many
boards does the search visit before it finds a solution.  We don't care how many
if it fails.

What we want you to do is revise the function so that when it finds a solution,
instead of producing the solution (the board), it instead produces the total
number of boards fn-for-board visited to find that solution.

To help you here are some more details about what you need to do.  We stress
again, that this problem involves careful thinking more than lots of typing.

Clearly the signature and purpose change.  In addition the check-expects have to
change.  To help, we are telling you that the correct answers for n 1 to 8 are:

   1    2
   2    false
   3    false
   4    16
   5    6
   6    251
   7    12
   8    1970

you may also need to change the template tag, and you definitely need to edit
some parts of the function definition. But once again we stress think carefully
and make focused systematic edits.  That's the key to this problem.

You must NOT CHANGE THE LANGUAGE THE STARTER FILE IS SET TO.  Your solution must
use the intermediate student with lambda language.

|#

;; =================
;; Data definitions:

(@htdd Position)
;; Position is Natural
;; interp. positions on the board
;;         if    N is the number of queens
;;         then  (sqr N) is the number of positions on the board
;;         so    this number should be in [0, (- (sqr N) 1)]
(define P1 0)        ;upper left corner of board
(define P2 (- 16 1)) ;lower left corner of 4x4 board


(@htdd Board)
;; Board is (listof Position)  up to N elements long
;; interp. the positions of the queens that have been placed on the board
(define BD1 empty)           ;no queens placed
(define BD2 (list 0))        ;one queen in upper left corner
(define BD3 (list 14 8 7 1)) ;a solution to 4x4 puzzle 



;; =================
;; Functions:

(@htdf nqueens)
(@signature Natural -> Board or false)
;; produce first found solution for n queens of size N; or false if none exists
(check-expect (nqueens 1) (list 0))
(check-expect (nqueens 2) false)
(check-expect (nqueens 3) false)
(check-expect (nqueens 4) (list 14 8 7 1))
(check-expect (nqueens 5) (list 23 16 14 7 0))
(check-expect (nqueens 6) (list 34 26 18 17 9 1))
(check-expect (nqueens 7) (list 47 38 29 27 18 9 0))
(check-expect (nqueens 8) (list 59 49 46 34 29 23 12 0))

(@template encapsulated backtracking genrec arb-tree)

(define (nqueens N)          
  ;; Termination argument:
  ;; Trivial cases:
  ;;   bd is solved or it is impossible to add a queen and get a valid board
  ;; 
  ;; Reduction step:
  ;;   add a queen at every possible empty position (after last queen)
  ;; 
  ;; Since board is finite, and each board is explored at most once, 
  ;; search will definitely terminate. (But the search space does grow
  ;; really fast!)
  
  (local [;; Board -> Board or false
          (define (fn-for-bd bd)
            (cond [(solved? bd) bd]
                  [else
                   (fn-for-lobd (next-boards bd))]))

          ;; (listof Board) -> Board or false
          (define (fn-for-lobd lobd)
            (cond [(empty? lobd) false]
                  [else
                   (local [(define try (fn-for-bd (first lobd)))]
                     (if (not (false? try))
                         try
                         (fn-for-lobd (rest lobd))))]))
          
          
          ;; Board -> Boolean
          ;; Produce true if board has N queens.
          (define (solved? bd) (= (length bd) N))


          ;; Board -> (listof Board)
          ;; produce next valid boards by adding a queen at every new position 
          ;;   - that comes after all existing queen positions (not required)
          ;;   - that does not attack any existing queens
          ;; (@template fn-composition use-abstract-fn)
          (define (next-boards bd)
            (local [(define max-so-far (foldr max -1 bd))] ;highest queen pos
              (map (lambda (p2) (cons p2 bd)) 
                   (filter (lambda (p2)
                             (and (> p2 max-so-far)
                                  (andmap (lambda (p1)     
                                            (not (attack? p2 p1)))
                                          bd)))
                           (build-list (sqr N) identity)))))
          
          ;; Position Position -> Boolean
          ;; produce true if queens at position a and b attack each other
          (define (attack? pa pb)
            (local [(define x1 (pos-x pa))
                    (define y1 (pos-y pa))
                    (define x2 (pos-x pb))
                    (define y2 (pos-y pb))]
              (or (= x1 x2)                           ;same row
                  (= y1 y2)                           ;same column
                  (= (/ (- y2 y1) (- x2 x1))  1)      ;same slope  1 diagonal
                  (= (/ (- y2 y1) (- x2 x1)) -1))))   ;same slope -1 diagonal
          
          
          ;; Pos -> Natural
          ;; produce the row or column number in [0, N) for the given position
          (define (pos-x p) (remainder p N))
          (define (pos-y p) (quotient  p N))]
    
    (fn-for-bd empty)))

