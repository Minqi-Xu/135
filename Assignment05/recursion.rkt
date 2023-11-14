;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment05, Problem 2

;; purpose
;;  (my-list-ref lon index) consumes a list of numbers and an index, and
;; produce the element in the list at the consumed index (start at 0), and will
;; output false if the index is greater than the length of lon
;;  (my-list-ref/position lon index position) consumes a list of numbers, an
;; index and a position, and produce the element in the list at the consumed
;; index (start at 0), the parameter position is in order to record the position
;; of the first element of lon in original lon 

;; contract
;; my-list-ref: (listof Num) Nat -> (anyof Num false)
;; my-list-ref/position: (listof Num) Nat Nat -> Num

;; example
(check-expect(my-list-ref '(1 2 3 4) 2) 3)
(check-expect(my-list-ref/position '(1 2 3 4 5) 4 0) 5)

;; functions
(define (my-list-ref/position lon index position)
  (cond[(= index position) (first lon)]
       [else (my-list-ref/position (rest lon) index (add1 position))]))

(define (my-list-ref lon index)
  (cond[(>= index (length lon)) false]
       [else (my-list-ref/position lon index 0)]))

;; test
(check-expect (my-list-ref '(5 4 3 2 1) 2) 3)
(check-expect (my-list-ref '(2) 20) false)
(check-expect (my-list-ref '(2) 0) 2)

;; purpose
;;  (zip lon los) consumes two lists with the same length (a list of numbers
;; and a list of strings). The function produces an association list where the
;; keys are the elements of the first list, and the values are the
;; corresponding elements of the second list
;;       requirement:  lon and los have the same length

;; contract
;; zip: (listof Num) (listof Str) -> (listof (list Num Str))

;; example
(check-expect (zip '(1 2 3 4) '("a" "b" "c" "d"))
              '((1 "a") (2 "b") (3 "c") (4 "d")))

;; function
(define (zip lon los)
  (cond[(empty? lon) empty]
       [else (cons (list (first lon) (first los))
                   (zip (rest lon) (rest los)))]))

;; test
(check-expect (zip empty empty) empty)
(check-expect (zip '(1 0) '("Water" "loo"))
              '((1 "Water") (0 "loo")))

;; purpose
;;  (count-symbol/2D list sym) consumes two values, a (listof (listof Sym)) and
;; a Sym and produces the total number of times that Symbol occurs in any of
;; the list of Symbols
;;  (count-symbol/1D los sym) consumes two values, a (listof Sym) and a Sym and
;; produces the total number of times that Symbol occurs in the list

;; contract
;; count-symbol/2D: (listof (listof Sym)) Sym -> Nat
;; count-symbol/1d: (listof Sym) Sym -> Nat

;; example
(check-expect (count-symbol/2D '((a a b) () (a)) 'a) 3)
(check-expect (count-symbol/1D '(a a b) 'a) 2)

;; functions
(define (count-symbol/1D los sym)
  (cond[(empty? los) 0]
       [(symbol=? sym (first los)) (+ 1 (count-symbol/1D (rest los) sym))]
       [else (count-symbol/1D (rest los) sym)]))

(define (count-symbol/2D list sym)
  (cond[(empty? list) 0]
       [else (+ (count-symbol/1D (first list) sym)
                (count-symbol/2D (rest list) sym))]))

;; test
(check-expect (count-symbol/2D '() 'a) 0)
(check-expect (count-symbol/2D '((a a b) () (a c)) 'b) 1)
(check-expect (count-symbol/2D '((a a b) () (a)) 'c) 0)
(check-expect (count-symbol/2D '((a b) (a b) (a d)) 'd) 1)