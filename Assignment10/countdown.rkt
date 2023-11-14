;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname countdown) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment10, Problem 2

;; An Operator (Op) is (anyof '+ '- '* '/)

;; A Binary Expression Tree (BET) is one of:
;; * Nat
;; * (list Op BET BET)

;; (countdown-numbers lonn target) produces a BET using the numbers in lon
;;   that evaluates to target, or false if no such BET exists.
;; countdown-numbers: (listof Nat) Nat -> (anyof BET false)

;; (swap i j lst) produces lst with the elements at positions
;;   i and j swapped
;; swap: Nat Nat (listof X) -> (listof X)
;;   requires: i, j < (length lst)

(check-expect (swap 0 0 empty) empty)
(check-expect (swap 0 3 '(0 1 2 3 4 5)) '(3 1 2 0 4 5))

(define (swap i j lst)
  (cond[(= i j) lst]
       [else (local[(define (n-th posi lst)
                      (cond[(zero? posi) (first lst)]
                           [else (n-th (sub1 posi) (rest lst))]))
                    (define i-th (n-th i lst))
                    (define j-th (n-th j lst))
                    (define (swap/acc i j lst posi)
                      (cond[(empty? lst) empty]
                           [(= i posi) (cons j-th
                                             (swap/acc i j (rest lst) (add1 posi)))]
                           [(= j posi) (cons i-th
                                             (swap/acc i j (rest lst) (add1 posi)))]
                           [else (cons (first lst)
                                       (swap/acc i j (rest lst) (add1 posi)))]))]
               (swap/acc i j lst 0))]))

;; (generate-permutations lst) produces all possible permutations of lst
;; generate-permutations: (listof X) -> (listof (listof X))

(check-expect (generate-permutations empty) empty)
(check-expect (generate-permutations '(2 4 8))
              '((2 4 8) (2 8 4) (8 4 2) (8 2 4) (4 8 2) (4 2 8)))

(define (generate-permutations lst)
  (local[(define len (foldr (lambda (x ans) (add1 ans)) 0 lst))
         (define (permutation/acc num now lst)
           (cond[(= num now) empty]
                [(= 1 num) lst]
                [else (append
                       (permutation/acc (sub1 num) 0 lst)
                       (permutation/acc num (add1 now)
                                        (swap (- (length lst) now 1)
                                              (- (length lst) num) lst)))]))
         (define data (append (permutation/acc len 0 lst)))]
    (first (foldr (lambda (x ans)
                    (cond[(= (remainder (first (rest ans)) len) 1)
                          (list (cons (cons x empty) (first ans))
                                (add1 (first (rest ans))))]
                         [else (list (cons (append (list x) (first (first ans)))
                                           (rest (first ans)))
                                     (add1 (first (rest ans))))]))
                  (list empty 1) data))))


;; (generate-tuples lst n) produces all tuples of length n of
;;   elements in lst.
;; generate-tuples: (listof X) Nat -> (listof (listof X))

(check-expect
 (generate-tuples '(+ -) 3)
 '((+ + +) (- + +) (+ - +) (- - +)
   (+ + -) (- + -) (+ - -) (- - -)))
(check-expect
 (generate-tuples '(+ -) 0)
 (list empty))

(define (generate-tuples lst n)
  (local[(define len (foldr (lambda (x ans) (add1 ans)) 0 lst))
         (define (n-th posi lst)
                      (cond[(zero? posi) (first lst)]
                           [else (n-th (sub1 posi) (rest lst))]))
         (define (my-reverse lst) (foldl cons empty lst))
         (define (each-stat stat posi)
           (cond[(= posi n) empty]
                [else (cons (n-th (floor (/ stat
                                            (expt len (- n posi 1))))
                                  lst)
                            (each-stat (remainder stat
                                                  (expt len (- n posi 1)))
                                       (add1 posi)))]))
         (define (combine/acc stat)
           (cond[(= stat (expt len n)) empty]
                [else (cons
                       (my-reverse (each-stat stat 0))
                       (combine/acc (add1 stat)))]))]
    (combine/acc 0)))

;; (create-bets nlon nloop) produces a list of all possible BET
;;   based off of nlon and nloop.
;; create-bets: (listof (listof Num)) (listof (listof Op)) ->
;;                                                    (listof BET)
;;   requires: (length nlon) - (length nloop) =1

;(check-expect (create-bets '((8 6 4 2)) '((/ + -)))
 ;             '((/ 8 (+ 6 (- 4 2)))
  ;              (/ 8 (+ (- 6 4) 2))
 ;               (/ (+ 8 6) (+ 4 2))
  ;              (/ (+ 8 (- 6 4)) 2)
   ;             (/ (+ (- 8 6) 4) 2)))

;(define (create-bets nlon nloop)
 ; (local[(define (create-left-right-pairs node-cnt)
  ;         (build-list node-cnt (lambda (x) (list (- (sub1 node-cnt) x) x))))
   ;      (define (create-tree-structure node-cnt)
    ;       (cond[(zero? node-cnt) empty]
     ;           [else (map
      ;                 (lambda (pair) (cons (create-tree-structure (first pair))
       ;                                     (create-tree-structure (second pair))))
       ;                (create-left-right-pairs node-cnt))]))]
    ;()))