;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname vowels) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment04, Problem 1

;; purpose
;; (count-vowels string) converts string into list and then count how
;; many vowels appear
;; (count-vowels/list list) shows that how many vowels appear in the string
;; (vowels? ch) determine whether the character ch belongs to vowel

;; contract
;; count-vowels: Str -> Nat
;; count-vowels/list: (listof Char) -> Nat
;; vowels?: Char -> Bool

;; example
(check-expect (count-vowels "aeiou bdk") 5)
(check-expect (count-vowels/list (cons #\a (cons #\p empty))) 1)
(check-expect (vowels? #\o) true)

;; functions
(define (vowels? ch)
  (cond[(or (char=? ch #\a)
            (char=? ch #\e)
            (char=? ch #\i)
            (char=? ch #\o)
            (char=? ch #\u)) true]
       [else false]))

(define (count-vowels/list list)
  (cond[(empty? list) 0]
       [(vowels? (first list))
        (+ 1 (count-vowels/list (rest list)))]
       [else (count-vowels/list (rest list))]))

(define (count-vowels string)
  (count-vowels/list (string->list string)))

;; test
(check-expect (count-vowels "") 0)
(check-expect (count-vowels "a") 1)
(check-expect (count-vowels "bdk") 0)
(check-expect (count-vowels "@#$ %^!&*") 0)
(check-expect (count-vowels "aeiou") 5)
(check-expect (count-vowels "the quick brown fox jumped over the lazy dog") 12)
(check-expect (count-vowels "12345667789") 0)
(check-expect (count-vowels "empty") 1)
