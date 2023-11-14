;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pnormp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment03, Problem 2

;;purpose
;;  (pnormp power list) calculate the required value with the given
;; list and the given power
;;  (element-2 list) get the second element of the list
;;  (element-3 list) get the third element of the list

;;contract
;; element-2: (listof Num) -> Num
;; element-3: (listof Num) -> Num
;; pnormp: Int (listof Num) -> Num


;; require
;; power must be positive integer

;;example
(check-expect (pnormp 3 (cons 3 (cons -4 (cons 5 empty)))) 216)
(check-expect (element-2 (cons 3 (cons -4 (cons 5 empty)))) -4)
(check-expect (element-3 (cons 3 (cons -4 (cons 5 empty)))) 5)

;;helper function
(define (element-2 list)
  (first (rest list)))

(define (element-3 list)
  (first (rest (rest list))))

;;function body
(define (pnormp power list)
  (cond[(equal? list empty) 0]
       [(equal? (rest list) empty) (expt (abs (first list)) power)]
       [(equal? (rest (rest list)) empty)
        (+ (expt (abs (first list)) power)
           (expt (abs (element-2 list)) power))]
       [else (+ (expt (abs (first list)) power)
                (expt (abs (element-2 list)) power)
                (expt (abs (element-3 list)) power))]))

;;test
(check-expect (pnormp 4 (cons 2 (cons -3 empty))) 97)
(check-expect (pnormp 1 (cons 2 empty)) 2)
(check-expect (pnormp 9 empty) 0)
