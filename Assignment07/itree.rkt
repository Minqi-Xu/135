;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname itree) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment07, Problem 2

(define-struct bnode (val left right))
;; A BNode is a (make-bnode Nat ITree ITree)
;; requires: val > every val in left ITree
;;           val < every val in right ITree

;; An ITree (Inteveral Tree) is one of:
;; * a Sym (a leaf)
;; * a BNode (an boundary node)

;;
;; Test case from assignment
;;

;; interior nodes
(define n3 (make-bnode 3 'None 'a))
(define n9 (make-bnode 9 'c 'None))
(define n8 (make-bnode 8 'b n9))
(define n6 (make-bnode 6 n3  n8))


;; (it-lookup it n) produces the symbol from the it
;; that is associated with the interval that contains n
;; it-lookup: ITree Nat -> Sym
;; Examples:
(check-expect (it-lookup n6 2) 'None)
(check-expect (it-lookup n6 3) 'a)

(define (it-lookup it n)
  (cond[(symbol? it) it]
       [(< n (bnode-val it)) (it-lookup (bnode-left it) n)]
       [else (it-lookup (bnode-right it) n)]))

;; Tests from assignment:
(check-expect (it-lookup n6 0) 'None)
(check-expect (it-lookup n6 2) 'None)
(check-expect (it-lookup n6 3) 'a)
(check-expect (it-lookup n6 5) 'a)
(check-expect (it-lookup n6 6) 'b)
(check-expect (it-lookup n6 7) 'b)
(check-expect (it-lookup n6 8) 'c)
(check-expect (it-lookup n6 9) 'None)
(check-expect (it-lookup n6 10) 'None)