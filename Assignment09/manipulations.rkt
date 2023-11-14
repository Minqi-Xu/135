;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname manipulations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment09, Problem 4

;; (rotate-right lst) moves the element at the end of a lst to the front, with
;;   all other elements remaining unchanged

;; rotate-right: (listof Any) -> (listof Any)

(check-expect (rotate-right '(a)) '(a))
(check-expect (rotate-right empty) empty)

(define (rotate-right lst)
  (cond[(empty? lst) empty]
       [else (local[(define reversed-lst (foldl cons empty lst))]
               (cons (first reversed-lst)
                     (foldl cons empty (rest reversed-lst))))]))

(check-expect (rotate-right '(a b c d e f)) '(f a b c d e))

;; (rotate-left lst) moves the element a the front of a list to the end, with
;;   all other elements remaining unchanged

;; rotate-left: (listof Any) -> (listof Any)

(check-expect (rotate-left '(a)) '(a))
(check-expect (rotate-left empty) empty)

(define (rotate-left lst)
  (cond[(empty? lst) empty]
       [else (foldr cons (cons (first lst) empty) (rest lst))]))

(check-expect (rotate-left '(a b c d e f)) '(b c d e f a))

;; (prefix n lst) produces the first n elements of the list, if the length of
;;   the list is less than n, produces the entire list.

;; prefix: Nat (listof Any) -> (listof Any)

(check-expect (prefix 3 '(a b c d e f)) '(a b c))
(check-expect (prefix 10 '(a b c d e f)) '(a b c d e f))
(check-expect (prefix 3 empty) empty)

(define (prefix n lst)
  (local[; (my-reverse lst0) produces the reversed list
         ;; my-reverse: (listof Any) -> (listof Any)
         (define (my-reverse lst0)
           (foldl cons empty lst0))]
    (my-reverse (first (foldl (lambda (x ans)
                                (cond[(< (second ans) n)
                                      (list (cons x (first ans))
                                            (add1 (second ans)))]
                                     [else ans]))
                              (list empty 0)
                              lst)))))

(check-expect (prefix 0 '(a b c d e f)) empty)
(check-expect (prefix 1 '(a b c d e f)) '(a))

;; (insert-at position new lst) inserts a new element after the specified
;;   position in a list, with all precedcing element unchanged, and with all
;;   elements after that position shifted to be after the new element. If the
;;   length is less than the specified position, add the new element to the end

;; insert-at: Nat Any (listof Any) -> (listof Any)

(check-expect (insert-at 5 'x '(a b c d e f)) '(a b c d e x f))
(check-expect (insert-at 0 'x '(a b c d e f)) '(x a b c d e f))
(check-expect (insert-at 1 'x empty) '(x))
(check-expect (insert-at 0 'x empty) '(x))

(define (insert-at position new lst)
  (local [(define len (foldr (lambda (x ans) (add1 ans)) 0 lst))]
    (cond[(empty? lst) (list new)]
         [(>= position len) (rotate-left (cons new lst))]
         [else (local[(define (my-reverse lst0)
                         (foldl cons empty lst0))]
               (my-reverse (first (foldl (lambda (x ans)
                                           (cond[(= (second ans) position)
                                                 (list
                                                  (cons x
                                                        (cons new (first ans)))
                                                  (+ 2 (second ans)))]
                                                [else
                                                 (list (cons x (first ans))
                                                       (add1 (second ans)))]))
                                         (list empty 0)
                                         lst))))])))

(check-expect (insert-at 7 'x '(a b c d e f)) '(a b c d e f x))
(check-expect (insert-at 100 'x '(a b c d e f)) '(a b c d e f x))
(check-expect (insert-at 0 'x '(a)) '(x a))
(check-expect (insert-at 1 'x '(a)) '(a x))
(check-expect (insert-at 100 'x '(a)) '(a x))
(check-expect (insert-at 100 'x empty) '(x))

