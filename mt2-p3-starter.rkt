;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mt2-p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment mt2-p3)

(@cwl ???)   ;fill in your CWL here (same CWL you put for 110 problem sets)

(@problem 1) ;THIS FILE ACTUALLY HAS 3 AND 4.  DO NOT EDIT OR DELETE THIS LINE!

;;
;; This starter file contains one data definition, and then has two separate
;; function designs you must complete.  Both functions operate on the same
;; data definition, so this will save you time because you just have to learn
;; one data definition to design two functions.
;;
;; HINT: One of the problems in this file involves the try-catch template.
;;       Be sure that you use the template and the @template origin properly.
;;

(@htdd TTree)
(define-struct node (k v lef mid rig))
;; TTree is one of:
;;  - false
;;  - (make-node Integer String TTree TTree TTree)
;; interp. a ternary tree.  Each node has a key, a value, and three branches.
;;         Note that there are NO constraints that govern sorting or arrangement
;;         of the tree.  If a given key is in the tree it could be anywhere,
;;         and it could also be repeated.
;;
(define TT1 #f) ;remember that #f is just an alternative way of writing false
(define TT2 (make-node 2 "two" #f #f #f))
(define TT3 (make-node 3 "three"
                       TT2
                       (make-node 6 "six" #f #f #f)
                       (make-node 1 "one" #f #f #f)))

(define (fn-for-ttree tt)
  (cond [(false? tt) (...)]
        [else
         (... (node-k tt)
              (node-v tt)
              (fn-for-ttree (node-lef tt))
              (fn-for-ttree (node-mid tt))
              (fn-for-ttree (node-rig tt)))]))


(@htdf fold-ttree)
(@signature (Integer String X X X -> X) X TTree -> X)
;; abstract fold for TTree
(check-expect (fold-ttree make-node false TT3) TT3)

(@template TTree)

(define (fold-ttree c b tt)
  (cond [(false? tt) b]
        [else
         (c (node-k tt)
            (node-v tt)
            (fold-ttree c b (node-lef tt))
            (fold-ttree c b (node-mid tt))
            (fold-ttree c b (node-rig tt)))]))

;;
;; Problem 3
;;
;; Complete the design of the following function with signature, purpose,
;; commented out stub, check-expects, @template tag and function.  You must
;; follow all applicable design rules.
;;
;; NOTE: If it is reasonable to use the fold-ttree function then you will
;;       lose 20% marks from an otherwise correct solution if you do not
;;       use it.
;;
;;       If it is not reasonable to use the fold-ttree function, but you
;;       do use it then you will lose 30% off an otherwise correct solution.
;;
;; The function must consume a TTree and produce the number of nodes in the
;; tree for which all three children are false.  So, for example:
;; 
;;  (count-leaves TT1) should produce 0
;;  (count-leaves TT2) should produce 1
;;  (count-leaves TT3) should produce 3
;;
;(@htdf count-leaves) ;YOU MUST uncomment this line when you start



(@problem 2) ;this is actually problem 4; DO NOT EDIT OR DELETE THIS LINE
;;
;; Problem 4
;;
;; Complete the design of the following function with signature, purpose,
;; commented out stub, check-expects, @template tag and function.  You must
;; follow all applicable design rules.
;;
;; NOTE: If it is reasonable to use the fold-ttree function then you will
;;       lose 20% marks from an otherwise correct solution if you do not
;;       use it.
;;
;;       If it is not reasonable to use the fold-ttree function, but you
;;       do use it then you will lose 30% off an otherwise correct solution.
;;
;; The function must consume a TTree and an integer, and search the tree
;; for the first node with that key.  If found it should produce that node's
;; value. It should fail if not found.
;;

;(@htdf find-val) ;YOU MUST uncomment this line when you start
