;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname unzip) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment08, Problem 2

;; purpose
;; (unzip lst) consumes a list of pairs(two-element lists), and produces a list
;;  of two lists. The first list contains the first element from each pair, and
;;  the second list contains the second element from each pair, in the original
;;  order.

;; contract
;; unzip: (listof (list X Y)) -> (list (listof X) (listof Y))

;; example
(check-expect (unzip empty) '(() ()))

;; function
(define (unzip lst)
  (local[;(first-element lst1) consumes a list and combine the first elements of
         ;;  each pairs and produces a list
         ;; first-element: (listof (list X Y)) -> (listof X)
         ;;(second-element lst2) consumes a list and combine the second elements
         ;;  of each pairs and produces a list
         ;; second-element: (listof (list X Y)) -> (listof Y)
         (define (first-element lst1)
           (cond[(empty? lst1) empty]
                [else (cons (first (first lst1))
                            (first-element (rest lst1)))]))
         (define (second-element lst2)
           (cond[(empty? lst2) empty]
                [else (cons (second (first lst2))
                            (second-element (rest lst2)))]))]
    (list (first-element lst) (second-element lst))))

;; tests
(check-expect (unzip '((1 "a") (2 b) ("3" a))) '((1 2 "3") ("a" b a)))