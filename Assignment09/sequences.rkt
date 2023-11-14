;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sequences) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment09, Problem 3

;; purpose
;; (sequence n1 n2 sym) consums two numbers (the first two numbers of the
;;   potential sequence) and a symbol (either 'arithmetic or 'geometric). Then
;;   produces a function that consumes a single natural number (the length of
;;   the sequence to produce) and produces a list containing the numbers in the
;;   sequence.
;; requires:
;;            the value input must always can produce a valid sequence.

;; contract
;; sequence: Num Num Sym -> (Nat -> (listof Num))

;; examples
(check-expect ((sequence 1 2 'arithmetic) 0) empty)
(check-expect ((sequence 1 2 'geometric) 0) empty)

;; function
(define (sequence n1 n2 sym)
  (lambda (len)
      (cond[(symbol=? sym 'arithmetic)
            (build-list len (lambda (x) (+ (* x (- n2 n1)) n1)))]
           [(symbol=? sym 'geometric)
            (build-list len (lambda (x) (* (expt (/ n2 n1) x) n1)))])))

;; tests
(check-expect ((sequence 5 7 'arithmetic) 6) '(5 7 9 11 13 15))
(check-expect ((sequence 2 6 'geometric) 4) '(2 6 18 54))