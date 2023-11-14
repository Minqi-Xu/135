;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bstd) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment07, Problem 1

(define-struct node (key val left right))
;; A Node is a (make-node Nat Sym BSTD BSTD)
;; requires: key > every key in left BSTD
;;           key < every key in right BSTD

;; A binary search tree dictionary (BSTD) is one of:
;; * empty
;; * Node


;;
;; Test case from assignment
;;

;; leaves
(define n1 (make-node 1 'a '() '()))
(define n4 (make-node 4 'g '() '()))
(define n6 (make-node 6 'o '() '()))
(define n9 (make-node 9 'x '() '()))
(define n14 (make-node 14 'z '() '()))

;; interior nodes
(define n12 (make-node 12 'y n9  n14))
(define  n7 (make-node 7 'd n6 n12))
(define  n3 (make-node 3 'b n1  n4))
(define  n5 (make-node 5 'o n3  n7))
(define root1 n5)


;; Q1a)

;;(range-count dict low high) produces the number of
;; keys that are >= low and < high in dict
;; range-count: BSTD Nat Nat -> Nat
;; requires: low < high
;; Examples:
(check-expect (range-count root1 20 30) 0)
(check-expect (range-count root1 10 13) 1)

(define (range-count dict low high)
  (cond[(empty? dict) 0]
       [(< (node-key dict) low) (range-count (node-right dict) low high)]
       [(>= (node-key dict) high) (range-count (node-left dict) low high)]
       [else (+ 1
                (range-count (node-right dict) low high)
                (range-count (node-left dict) low high))]))


;; Tests some tests for Q1a)
(check-expect (range-count root1 20 30) 0)
(check-expect (range-count root1 10 12) 0)
(check-expect (range-count root1 10 13) 1)
(check-expect (range-count root1 12 13) 1)
(check-expect (range-count root1 4 8) 4)
(check-expect (range-count root1 0 15) 9)


;; Q1b)

;; (range-query dict low high) produces a list of values whose
;; keys in the dict are in the range >= low and < high. The
;; list of values produced are in ascending order by their key.
;; requires: low < high
;; Examples:
(check-expect (range-query root1 10 12) '())
(check-expect (range-query root1 4 8) '(g o o d))

(define (range-query dict low high)
  (cond[(empty? dict) empty]
       [(< (node-key dict) low) (range-query (node-right dict) low high)]
       [(>= (node-key dict) high) (range-query (node-left dict) low high)]
       [else (append (range-query (node-left dict) low high)
                     (list (node-val dict))
                     (range-query (node-right dict) low high))]))

;; Tests some tests for Q1b)
(check-expect (range-query root1 20 30) '())
(check-expect (range-query root1 10 12) '())
(check-expect (range-query root1 10 13) '(y))
(check-expect (range-query root1 12 13)'(y))
(check-expect (range-query root1 4 8) '(g o o d))
(check-expect (range-query root1 0 15) '(a b g o o d x y z))
