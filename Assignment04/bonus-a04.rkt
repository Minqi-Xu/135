;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus-a04) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; purpose
;; (eval-poly list x) by calling (eval-poly/count list x power) to consumes
;; a list of numbers which representing the coefficients of a polynomial and
;; a value of x, and produces the result of evaluating the given polynomial
;; at the given value of this.
;; (eval-poly/count list x power) consumes a list of numbers which representing
;; the coefficients of a polynomial and a value of x and the power, then
;; produces the result of evaluating the given polynomial at the given value
;; of this.
;; (eval-poly/expt x power count) is a function that has the same function as
;; (expt x power) written with recursion and use count to determine when the
;; recursion end.

;; contract
;; eval-poly: (listof Num) Num -> Num
;; eval-poly/count: (listof Num) Num Nat -> Num
;; eval-poly/expt: Num Nat Nat -> Num

;; examples
(check-expect (eval-poly (cons 2 (cons 3.5 empty)) 8) 30)
(check-expect (eval-poly/count (cons 1.5 (cons 2.5 empty)) 2 0) 6.5)
(check-expect (eval-poly/expt 2 10 0) 1024)

;; functions
(define (eval-poly/expt x power count)
  (cond[(zero? power) 1]
       [(< count power)
        (* x (eval-poly/expt x power (add1 count)))]
       [else 1]))

(define (eval-poly/count list x power)
  (cond[(empty? list) 0]
       [else (+ (* (first list) (eval-poly/expt x power 0))
                (eval-poly/count (rest list) x (add1 power)))]))

(define (eval-poly list x)
  (eval-poly/count list x 0))

;; tests
(check-expect (eval-poly (cons 1.4 (cons 4 (cons 0 (cons 2 empty)))) 3) 67.4)
(check-expect (eval-poly (cons 2 empty) 3) 2)
(check-expect (eval-poly empty 3) 0)
(check-expect (eval-poly (cons 1.4 (cons 4 (cons 0 (cons 2 empty)))) 0) 1.4)