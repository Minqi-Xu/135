;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname remove-duplicates) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment04, Problem 2

;; purpose
;; (remove-duplicates list) consumes a list of numbers and produces
;; the same list, with all but eh last occurrence of each number removed
;; (appear? num list) checks whether num appears in the list
;;       requires: list have to be the list of Num

;; contract
;; remove-duplicates: (listof Num) -> (listof Num)

;; example
(check-expect (remove-duplicates (cons 1 (cons 3 (cons 1 (cons 2
                                  (cons 4 (cons 2 (cons 7 (cons 2
                                   (cons 5 empty))))))))))
              (cons 3 (cons 1 (cons 4 (cons 7 (cons 2 (cons 5 empty)))))))
(check-expect (appear? 3 (cons 1 (cons 3 (cons 1 (cons 2 empty))))) true)

;; functions
(define (appear? num list)
  (cond[(empty? list) false]
       [(= num (first list)) true]
       [else (appear? num (rest list))]))

(define (remove-duplicates list)
  (cond[(empty? list) empty]
       [(appear? (first list) (rest list))
        (remove-duplicates (rest list))]
       [else (cons (first list) (remove-duplicates (rest list)))]))

;; tests
(check-expect (remove-duplicates (cons 1 (cons 1 (cons 1 (cons 1
                                  (cons 1 (cons 1 (cons 1 (cons 1
                                   (cons 1 empty))))))))))
              (cons 1 empty))
(check-expect (remove-duplicates empty) empty)
(check-expect (remove-duplicates (cons 1 (cons 2 (cons 3 (cons 4
                                  (cons 5 (cons 6 (cons 7 (cons 8
                                   (cons 9 empty))))))))))
              (cons 1 (cons 2 (cons 3 (cons 4
               (cons 5 (cons 6 (cons 7 (cons 8
                (cons 9 empty))))))))))
(check-expect (remove-duplicates (cons 1 empty)) (cons 1 empty))
(check-expect (remove-duplicates (cons 1 (cons 1 (cons 2 (cons 2 empty)))))
              (cons 1 (cons 2 empty)))