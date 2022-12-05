;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mt2-p3-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
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
;;         of the tree.  If a given tree is in the tree it could be anywhere,
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
;; NOTE: If it is appropriate to use the fold-ttree function then you will
;;       lose 20% marks from an otherwise correct solution if you do not
;;       use it.
;;
;;       If it is not appropriate to use the fold-tree function, but you
;;       do use it then you will lose 30% off an otherwise correct solution.
;;
;; The function must consume a TTree and produce the number of nodes in the
;; tree for which all three children are false.  So, for example:
;; 
;;  (count-leaves TT1) should produce 0
;;  (count-leaves TT2) should produce 1
;;  (count-leaves TT3) should produce 3
;;
(@htdf count-leaves)


(@signature TTree -> Natural)
;; produce number of nodes with 3 false children
(check-expect (count-leaves TT1) 0)
(check-expect (count-leaves TT2) 1)
(check-expect (count-leaves TT3) 3)
(check-expect (count-leaves
               (make-node "20" "twenty"
                          (make-node "30" "thirty"
                                     (make-node "5" "five"  #f #f TT2)
                                     (make-node "6" "six"   #f TT2 #f)
                                     (make-node "7" "seven" TT2 #f #f))
                          (make-node "30" "thirty"
                                     (make-node "5" "five"  #f #f TT2)
                                     (make-node "6" "six"   #f TT2 #f)
                                     (make-node "7" "seven" TT2 #f #f))
                          (make-node "30" "thirty"
                                     (make-node "5" "five"  #f #f TT2)
                                     (make-node "6" "six"   #f TT2 #f)
                                     (make-node "7" "seven" TT2 #f #f))))
              9)

;; It can be done this way, but this is a good case for the fold function
#;#;
(@template TTree)

(define (count-leaves tt)
  (cond [(false? tt) 0]
        [else
         (if (and (false? (node-lef tt))
                  (false? (node-mid tt))
                  (false? (node-rig tt))) 
             1
             (+ (count-leaves (node-lef tt))
                (count-leaves (node-mid tt))
                (count-leaves (node-rig tt))))]))


(@template use-abstract-fn)

(define (count-leaves tt)
  (fold-ttree (lambda (k v rnrl rnrm rnrr)
                (if (and (zero? rnrl) (zero? rnrl) (zero? rnrl))
                    1
                    (+  rnrl rnrm rnrr)))
              0              
              tt))


(@problem 2) ;this is actually problem 4; DO NOT EDIT OR DELETE THIS LINE
;;
;; Problem 4
;;
;; Complete the design of the following function with signature, purpose,
;; commented out stub, check-expects, @template tag and function.  You must
;; follow all applicable design rules.
;;
;; NOTE: If it is appropriate to use the fold-ttree function then you will
;;       lose 20% marks from an otherwise correct solution if you do not
;;       use it.
;;
;;       If it is not appropriate to use the fold-tree function, but you
;;       do use it then you will lose 30% off an otherwise correct solution.
;;
;; The function must consume a TTree and an integer, and search the tree
;; for the first node with that key.  If found it should produce that node's
;; value. It should fail if not found.
;;

(@htdf find-val)

(@signature TTree Integer -> String or false)
;; produce value from first occurence of key; or fail

(check-expect (find-val TT1 1) false)
(check-expect (find-val TT2 2) "two")
(check-expect (find-val TT3 3) "three")
(check-expect (find-val TT3 2) "two")
(check-expect (find-val TT3 6) "six")
(check-expect (find-val TT3 1) "one")
(check-expect (find-val TT3 9) false)

(@template TTree try-catch)

(define (find-val tt key)
  (cond [(false? tt) false]
        [else
         (if (= (node-k tt) key)
             (node-v tt)
             (local [(define try (find-val (node-lef tt) key))]
               (if (not (false? try))
                   try
                   (local [(define try (find-val (node-mid tt) key))]
                     (if (not (false? try))
                         try
                         (find-val (node-rig tt) key))))))]))


		       
