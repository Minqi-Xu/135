;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname change) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment04, Problem 3

;; purpose
;; (count-change list) consumes a list of symbols and porduces a number
;; which is the total amount of change in cents
;; (identify-type sym) indentify which kind of coin, and return the correct
;; value of the coin
;; (make-change num) consumes a value in cents as a natural number and produces
;; a list of symbols adding up to that value.
;; (round-ammount num) is aimed to round the ammount with the correct rule
;; (make-change/count num num-nickel num-dime num-quarter num-loonie) has the
;; same function as (make-change num) and also, it count the number of each
;; coins(except toonie), and make sure that the number of coins(except toonie)
;; need to less than 4

;; contract
;; count-change: (listof Sym) -> Nat
;; identify-type: Sym -> Nat
;; make-change: Nat -> (listof Sym)
;; round-ammount: Nat -> Nat
;; make-change/count: Nat Nat Nat Nat Nat -> Nat

;; example
(check-expect (count-change (cons 'dime (cons 'dime empty))) 20)
(check-expect (identify-type 'nickel) 5)
(check-expect (make-change 11) (cons 'dime empty))
(check-expect (round-ammount 11) 10)
(check-expect (make-change/count 11 0 0 0 0) (cons 'dime empty))

;; constants
(define nickel 5)
(define dime 10)
(define quarter 25)
(define loonie 100)
(define toonie 200)

;; functions
(define (identify-type sym)
  (cond[(symbol=? sym 'nickel) nickel]
       [(symbol=? sym 'dime) dime]
       [(symbol=? sym 'quarter) quarter]
       [(symbol=? sym 'loonie) loonie]
       [(symbol=? sym 'toonie) toonie]
       [else 0]))

(define (count-change list)
  (cond[(empty? list) 0]
       [else (+ (identify-type (first list))
                (count-change (rest list)))]))

(define (round-ammount num)
  (cond[(<= (remainder num 5) 2) (- num (remainder num 5))]
       [else (+ (- num (remainder num 5)) 5)]))

(define (make-change/count num num-nickel num-dime num-quarter num-loonie)
  (cond[(>= num toonie) (cons 'toonie
                              (make-change/count (- num toonie)
                                                 0 0 0 0))]
       [(and (>= num loonie)
             (< num-loonie 3)) (cons 'loonie
                                     (make-change/count
                                      (- num loonie)
                                      0 0 0 (add1 num-loonie)))]
       [(and (>= num quarter)
             (< num-quarter 3)) (cons 'quarter
                                      (make-change/count
                                       (- num quarter)
                                       0 0 (add1 num-quarter) num-loonie))]
       [(and (>= num dime)
             (< num-dime 3)) (cons 'dime
                                  (make-change/count
                                   (- num dime)
                                   0 (add1 num-dime) num-quarter num-loonie))]
       [(and (>= num nickel)
             (< num-nickel 3)) (cons 'nickel
                                     (make-change/count
                                      (- num nickel) (add1 num-nickel)
                                      num-dime num-quarter num-loonie))]
       [else empty]))

(define (make-change num)
  (make-change/count (round-ammount num) 0 0 0 0))

;; tests
(check-expect (count-change (cons 'dime (cons 'quarter empty))) 35)
(check-expect (count-change (cons 'loonie (cons 'dime empty))) 110)
(check-expect (count-change (cons 'toonie (cons 'loonie empty))) 300)
(check-expect (count-change (cons 'nickel (cons 'USD empty))) 5)
(check-expect (count-change (make-change 137)) 135)
(check-expect (count-change (make-change 138)) 140)
(check-expect (count-change (make-change 10029)) 10030)
(check-expect (make-change 1210) (cons 'toonie (cons 'toonie (cons 'toonie
                                  (cons 'toonie (cons 'toonie (cons 'toonie
                                   (cons 'dime empty))))))))
(check-expect (make-change 320) (cons 'toonie (cons 'loonie (cons 'dime
                                 (cons 'dime empty)))))