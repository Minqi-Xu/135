;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment06, Problem 2

(define-struct pr (first-name last-name))
;; A personnel record (PR) is a (make-pr Str Str)

;; An employee database (ED) is a (listof PR)
;; In ED, there cannot be two equal PRs in one ED

(define ed-kitchener (list (make-pr "Gord" "Downie")
                           (make-pr "Rob" "Baker")
                           (make-pr "Gord" "Sinclair")
                           (make-pr "Paul" "Langlois")
                           (make-pr "Johnny" "Fay")))

(define ed-waterloo (list (make-pr "Byron" "Stroud")
                          (make-pr "Jed" "Simon")
                          (make-pr "Gord" "Downie")
                          (make-pr "Devin" "Townsend")
                          (make-pr "Rob" "Baker")))

;; purpose
;; (pr=? pr1 pr2) consumes two PRs and produces true if they are identical
;; (pr<? pr1 pr2 sym) consumes two PRs and a Sym. The Sym can either be 'first
;;  or 'last and determines if the initial comparison should be based on the
;;  first or the last name. The prdicate produces true if the first PR precedes
;;  the second one lexicographically. If the initial comparison compares two
;;  equal names, the prodicate will consider the other name. If both PRs have
;;  identical first and last names, the predicate will produce false
;; (pr>? pr1 pr2 sym) consumes two PRs and a Sym. The Sym can either be 'first
;;  or 'last and determines if the initial comparison should be based on the
;;  first or the last name. The prdicate produces true if the first PR is
;;  lexicographically larger than the second one. If the initial comparison
;;  compares two equal names, the prodicate will consider the other name. If
;;  both PRs have identical first and last names, the predicate will produce
;;  false

;; contract
;; pr=?: PR PR -> Bool
;; pr<?: PR PR (Anyof 'first 'last) -> Bool
;; pr>?: PR PR (Anyof 'first 'last) -> Bool

;; example
(check-expect (pr=? (make-pr "Gord" "Down") (make-pr "Gord" "Down")) true)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gard" "Duwn") 'first) false)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gard" "Duwn") 'first) true)

;; function
(define (pr=? pr1 pr2)
  (and (string=? (pr-first-name pr1) (pr-first-name pr2))
       (string=? (pr-last-name pr1) (pr-last-name pr2))))

(define (pr<? pr1 pr2 sym)
  (cond[(pr=? pr1 pr2) false]
       [(and (symbol=? sym 'first) (string<? (pr-first-name pr1)
                                             (pr-first-name pr2))) true]
       [(and (symbol=? sym 'first) (string>? (pr-first-name pr1)
                                             (pr-first-name pr2))) false]
       [(and (symbol=? sym 'first) (string=? (pr-first-name pr1)
                                             (pr-first-name pr2)))
        (string<? (pr-last-name pr1) (pr-last-name pr2))]
       [(and (symbol=? sym 'last) (string<? (pr-last-name pr1)
                                            (pr-last-name pr2))) true]
       [(and (symbol=? sym 'last) (string>? (pr-last-name pr1)
                                            (pr-last-name pr2))) false]
       [(and (symbol=? sym 'last) (string=? (pr-last-name pr1)
                                            (pr-last-name pr2)))
        (string<? (pr-first-name pr1) (pr-first-name pr2))]))

(define (pr>? pr1 pr2 sym)
  (cond[(pr=? pr1 pr2) false]
       [else (not (pr<? pr1 pr2 sym))]))

;; test
(check-expect (pr=? (make-pr "Gord" "Down") (make-pr "Gard" "Duwn")) false)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gord" "Down") 'first) false)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gord" "Duwn") 'first) true)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gard" "Down") 'first) false)
(check-expect
 (pr<? (make-pr "Gaard" "Down") (make-pr "Gard" "Duwn") 'first) true)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gord" "Down") 'last) false)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gord" "Duwn") 'last) true)
(check-expect
 (pr<? (make-pr "Gord" "Down") (make-pr "Gard" "Down") 'last) false)
(check-expect
 (pr<? (make-pr "Gzrd" "Down") (make-pr "Gard" "Duwn") 'last) true)
(check-expect
 (pr<? (make-pr "Gzrd" "Dzwn") (make-pr "Gard" "Duwn") 'last) false)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gord" "Down") 'first) false)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gord" "Duwn") 'first) false)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gard" "Down") 'first) true)
(check-expect
 (pr>? (make-pr "Gaard" "Down") (make-pr "Gard" "Duwn") 'first) false)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gord" "Down") 'last) false)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gord" "Duwn") 'last) false)
(check-expect
 (pr>? (make-pr "Gord" "Down") (make-pr "Gard" "Down") 'last) true)
(check-expect
 (pr>? (make-pr "Gaard" "Down") (make-pr "Gard" "Duwn") 'last) false)

;; purpose
;; (sort-ed ed sym) consumes an ED and a Sym, the Sym can either be 'last, which
;;  would sort it by last name, or 'first, which would sort it by first name. If
;;  the initial comparison compares two equal names, the predicate will consider
;;  the other name. And it will sort an employee database lexicographically.
;; (insert pr ed sym) consumes a PR, an ED and a Sym. It will insert pr into a
;;  ordered ED, the priority of comparison is determined by the Sym. The Sym can
;;  either be 'last, which would sort it by last name, or 'first, which would
;;  sort it by first name. If the initial comparison compares two equal names,
;;  the predicate will consider the other name.
;;         requires:  ed have to in ordered

;; contract
;; sort-ed: ED (Anyof 'first 'last) -> ED
;; insert: PR ED (Anyof 'first 'last) -> ED

;; example
(check-expect (sort-ed ed-kitchener 'last)
              (list (make-pr "Rob" "Baker")
                    (make-pr "Gord" "Downie")
                    (make-pr "Johnny" "Fay")
                    (make-pr "Paul" "Langlois")
                    (make-pr "Gord" "Sinclair")))
(check-expect (insert (make-pr "Johnny" "Fary")
                      (list (make-pr "Rob" "Baker")
                            (make-pr "Gord" "Downie")
                            (make-pr "Johnny" "Fay")
                            (make-pr "Paul" "Langlois")
                            (make-pr "Gord" "Sinclair")) 'last)
              (list (make-pr "Rob" "Baker")
                    (make-pr "Gord" "Downie")
                    (make-pr "Johnny" "Fary")
                    (make-pr "Johnny" "Fay")
                    (make-pr "Paul" "Langlois")
                    (make-pr "Gord" "Sinclair")))

;; function
(define (insert pr ed sym)
  (cond[(empty? ed) (cons pr empty)]
       [(pr>? pr (first ed) sym)
        (cons (first ed) (insert pr (rest ed) sym))]
       [else (cons pr ed)]))

(define (sort-ed ed sym)
  (cond[(empty? ed) empty]
       [else (insert (first ed) (sort-ed (rest ed) sym) sym)]))

;; test
(check-expect (sort-ed ed-waterloo 'first)
              (list (make-pr "Byron" "Stroud")
                    (make-pr "Devin" "Townsend")
                    (make-pr "Gord" "Downie")
                    (make-pr "Jed" "Simon")
                    (make-pr "Rob" "Baker")))
(check-expect (sort-ed ed-waterloo 'last)
              (list (make-pr "Rob" "Baker")
                    (make-pr "Gord" "Downie")
                    (make-pr "Jed" "Simon")
                    (make-pr "Byron" "Stroud")
                    (make-pr "Devin" "Townsend")))
(check-expect (sort-ed ed-kitchener 'first)
              (list (make-pr "Gord" "Downie")
                    (make-pr "Gord" "Sinclair")
                    (make-pr "Johnny" "Fay")
                    (make-pr "Paul" "Langlois")
                    (make-pr "Rob" "Baker")))

;; purpose
;; (merge-ed ed1 ed2) consumes 2 EDs, and merge them into a in ordered ED which
;;  is sorted by last name.
;; (merge-ed/order ed1 ed2) consumes 2 inordered EDs which are sorted by last
;;  name, and merge them into another in ordered ED which is sorted by last name
;;         require:  ed1 ed2 in (merge-ed/order ed1 ed2) must be in ordered

;; contract
;; merge-ed: ED ED -> ED
;; merge-ed/order: ED ED -> ED

;; example
(check-expect (merge-ed empty empty) empty)
(check-expect (merge-ed/order (sort-ed ed-waterloo 'last) empty)
              (sort-ed ed-waterloo 'last))

;; function
(define (merge-ed/order ed1 ed2)
  (cond[(empty? ed1) ed2]
       [(empty? ed2) ed1]
       [(pr=? (first ed1) (first ed2))
        (merge-ed/order (rest ed1) ed2)]
       [(pr<? (first ed1) (first ed2) 'last)
        (cons (first ed1) (merge-ed/order (rest ed1) ed2))]
       [else (cons (first ed2) (merge-ed/order ed1 (rest ed2)))]))

(define (merge-ed ed1 ed2)
  (merge-ed/order (sort-ed ed1 'last) (sort-ed ed2 'last)))

;; test
(check-expect (merge-ed ed-kitchener empty)
              (sort-ed ed-kitchener 'last))
(check-expect (merge-ed empty ed-waterloo)
              (sort-ed ed-waterloo 'last))
(check-expect (merge-ed ed-kitchener ed-waterloo)
              (list (make-pr "Rob" "Baker")
                    (make-pr "Gord" "Downie")
                    (make-pr "Johnny" "Fay")
                    (make-pr "Paul" "Langlois")
                    (make-pr "Jed" "Simon")
                    (make-pr "Gord" "Sinclair")
                    (make-pr "Byron" "Stroud")
                    (make-pr "Devin" "Townsend")))