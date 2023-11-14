;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |20191004|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (sum-num list)
  (cond[(empty? list) 0]
       [else (+ (first list) (sum-num (rest list)))]))

(define (factorial n)
  (cond[(= n 0) 1]  ; (zero? n)
       [else (* n (factorial (- n 1)))]))

(define (strings-equal? los)
  (cond[(empty? los) true]
       [(empty? (rest los)) true]
       [(not (string=? (first los) (first (rest los)))) false]
       [else (strings-equal? (rest los))]))
;;[else (and (string=? (first los)(first (rest los)))
;;           (string-equal? (rest los)))]))


(define week-const (* 7 24))
(define day-const 24)

(define (tot-time list)
  (cond[(empty? list) 0]
       [else (+ (first list) (tot-time (rest list)))]))

(define (week-num time)
  (cond[(< time week-const) 0]
       [else (+ 1 (week-num (- time week-const)))]))

(define (day-num time)
  (cons[(< time day-const) 0]
       [else (+ 1 (day-num (- time day-const)))]))

(define (manage-time tasks)
  (cons (week-num (tot-time tasks))
        (cons (day-num (- (tot-time tasks) (* (week-num (tot-time tasks)) week-const)))
              (cons (- (tot-time tasks) (+ (* (week-num (tot-time tasks)) week-const)
                                           (* (day-num (- (tot-time tasks)
                                                          (* (week-num (tot-time tasks))
                                                             week-const))) day-const))) empty))))