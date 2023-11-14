;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname change-alf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment09, Problem 2

;; purpose
;; (count-change list) consumes a list of symbols and porduces a number
;; which is the total amount of change in cents
;; (make-change num) consumes a value in cents as a natural number and produces
;; a list of symbols adding up to that value.

;; contract
;; count-change: (listof Sym) -> Nat
;; make-change: Nat -> (listof Sym)
;; make-change/count: Nat Nat Nat Nat Nat -> Nat

;; example
(check-expect (count-change (cons 'dime (cons 'dime empty))) 20)
(check-expect (make-change 11) (cons 'dime empty))

;; constants
(define nickel 5)
(define dime 10)
(define quarter 25)
(define loonie 100)
(define toonie 200)

;; functions


(define (count-change lst)
  (local[;(identify-type sym) indentify which kind of coin, and return the correct
         ;; value of the coin
         ;;identify-type: sym -> Nat
         (define (identify-type sym)
           (cond[(symbol=? sym 'nickel) nickel]
                [(symbol=? sym 'dime) dime]
                [(symbol=? sym 'quarter) quarter]
                [(symbol=? sym 'loonie) loonie]
                [(symbol=? sym 'toonie) toonie]
                [else 0]))]
    (foldr + 0 (map identify-type lst))))

(define (make-change num)
  (local[;(round-ammount num) is aimed to round the ammount with the correct rule
         ;; (make-change/count num num-nickel num-dime num-quarter num-loonie) has the
         ;; same function as (make-change num) and also, it count the number of each
         ;; coins(except toonie), and make sure that the number of coins(except toonie)
         ;; need to less than 4
         ;; round-ammount: Nat -> Nat
         (define round
           (cond[(<= (remainder num 5) 2) (- num (remainder num 5))]
                [else (+ (- num (remainder num 5)) 5)]))
         ; (my-reverse lst0) produces the reversed list
         ;; my-reverse: (listof Any) -> (listof Any)
         (define (my-reverse lst0)
           (foldl cons empty lst0))
         (define toonie-num (/ (- round (remainder round toonie)) toonie))
         (define after-toonie (- round (* toonie toonie-num)))
         (define loonie-num (/ (- after-toonie (remainder after-toonie loonie))
                               loonie))
         (define after-loonie (- after-toonie (* loonie loonie-num)))
         (define quarter-num (/ (- after-loonie
                                   (remainder after-loonie quarter))
                                quarter))
         (define after-quarter (- after-loonie (* quarter quarter-num)))
         (define dime-num (/ (- after-quarter (remainder after-quarter dime))
                             dime))
         (define after-dime (- after-quarter (* dime dime-num)))
         (define nickel-num (/ after-dime nickel))
         (define toonie-list (build-list toonie-num (lambda(x) 'toonie)))
         (define loonie-list (build-list loonie-num (lambda(x) 'loonie)))
         (define quarter-list (build-list quarter-num (lambda(x) 'quarter)))
         (define dime-list (build-list dime-num (lambda(x) 'dime)))
         (define nickel-list (build-list nickel-num (lambda(x) 'nickel)))]
    (my-reverse (foldl cons
                       (foldl cons
                              (foldl cons
                                     (foldl cons
                                            (foldl cons empty toonie-list)
                                            loonie-list)
                                     quarter-list)
                              dime-list)
                       nickel-list))))

;; tests
(check-expect (count-change (cons 'dime (cons 'quarter empty))) 35)
(check-expect (count-change (cons 'loonie (cons 'dime empty))) 110)
(check-expect (count-change (cons 'toonie (cons 'loonie empty))) 300)
(check-expect (count-change (cons 'nickel (cons 'USD empty))) 5)
(check-expect (count-change empty) 0)
(check-expect (count-change (cons 'USD (cons 'CAD empty))) 0)
(check-expect (make-change 1210) (cons 'toonie (cons 'toonie (cons 'toonie
                                  (cons 'toonie (cons 'toonie (cons 'toonie
                                   (cons 'dime empty))))))))
(check-expect (make-change 320) (cons 'toonie (cons 'loonie (cons 'dime
                                 (cons 'dime empty)))))
(check-expect (make-change 0) '())