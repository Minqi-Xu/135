;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname prime) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment04, Problem 4

;; purpose
;; (prime? num) consumes a natural number and produces true if that number
;; is prime, and false otherwise.
;; (prime?/loop num divisor) that consumes whether the natural number num is
;; prime, by factoring divisor and check the remainder
;; (next-prime num) consumes a natural number and produces the next prime
;; strictly greater than that number.
;; (prime-range min max) consumes two natural numbers and produces the list
;; of all prime numbers in the interval that starts with the first number and
;; ends with the second number (inclusive).

;; contract
;; prime?: Nat -> Bool
;; prime?/loop: Nat Nat -> Bool
;; next-prime: Nat -> Nat
;; prime-range: Nat Nat -> (listof Nat)

;; example
(check-expect (prime? 17) true)
(check-expect (prime?/loop 17 16) true)
(check-expect (next-prime 15) 17)
(check-expect (prime-range 1 10)
              (cons 2 (cons 3 (cons 5 (cons 7 empty)))))

;;functions
(define (prime?/loop num divisor)
  (cond[(= divisor 1) true]
       [(= 0 (remainder num divisor)) false]
       [else (prime?/loop num (sub1 divisor))]))

(define (prime? num)
  (cond[(<= num 1) false]
       [else (prime?/loop num (sub1 num))]))

(define (next-prime num)
  (cond[(prime? (add1 num)) (add1 num)]
       [else (next-prime (add1 num))]))

(define (prime-range min max)
  (cond[(>= min max) empty]
       [(prime? min) (cons min (prime-range (add1 min) max))]
       [else (prime-range (add1 min) max)]))

;; test
(check-expect (prime? 1) false)
(check-expect (prime? 0) false)
(check-expect (prime? 2) true)
(check-expect (prime? 101) true)
(check-expect (next-prime 0) 2)
(check-expect (next-prime 1) 2)
(check-expect (next-prime 50) 53)
(check-expect (next-prime 7) 11)
(check-expect (prime-range 10 1) empty)
(check-expect (prime-range 15 15) empty)
(check-expect (prime-range 11 11) empty)
(check-expect (prime-range 2 20)
              (cons 2 (cons 3 (cons 5 (cons 7 (cons 11
               (cons 13 (cons 17 (cons 19 empty)))))))))