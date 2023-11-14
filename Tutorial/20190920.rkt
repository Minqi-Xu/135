;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |20190920|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Purpose
;;(fizzbuzz n) Consumes a number n and produces the
;;proper response following the rules of fizzbuzz.

;;Contract
;;fizzbuzz: Int -> (Anyof Str Num)

;;Example
(check-expect (fizzbuzz 3) "Fizz")

;;Function Body
(define (fizzbuzz n)
  (cond[(and (= (remainder n 5) 0)
             (= (remainder n 3) 0)) "FizzBuzz"];[(= (remainder n 15) 0) "FizzBuzz"]
       [(= (remainder n 3) 0) "Fizz"]
       [(= (remainder n 5) 0) "Buzz"]
       [else n]))

;;Test
(check-expect (fizzbuzz -5) "Buzz")
(check-expect (fizzbuzz 17) 17)
(check-expect (fizzbuzz 30) "FizzBuzz")
(check-expect (fizzbuzz 0) "FizzBuzz")


;;(grt location cash) Consumes a location and cash
;;and determines if you can take the bus home
;;grt: Sym Num -> Str

(define(grt location cash)
  (cond[(symbol=? location 'Home) "You're already home."]
       [(cond[(>= cash 3.25) false]
             [else true])
       "You have to walk home."]
       [else "Take the bus home."]))