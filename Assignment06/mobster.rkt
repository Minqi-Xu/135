;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname mobster) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment06, Problem 3

(define-struct goon (street-name abilities))
;; A Goon is a (make-goon Str Abilities).
;; An Abilities is a (list Nat Nat Nat), where the elements represent
;;   Loyalty, Wealth, and Influence.

(define goon-btt (make-goon "Bullet Tooth Tony" (list 8 2 5)))
(define goon-ca (make-goon "Cousin Avi" (list 3 9 8)))
(define goon-btb (make-goon "Boris, the Blade" (list 4 6 7)))
(define goon-fff (make-goon "Franky Four Fingers" (list 5 7 3)))

(define applicant-gg (make-goon "Gorgeous George" (list 3 5 4)))
(define applicant-s (make-goon "Sol" (list 5 7 10)))

;; A Gang is a (listof Goon)

(define my-gang (list goon-btt goon-ca goon-btb goon-fff))
(define my-gang2 (list goon-btt goon-ca goon-btb
                       goon-fff applicant-gg applicant-s))

;; A Job is a (list Nat Nat Nat), where the elements represent
;;   required Loyalty, required Wealth, and required Influence.

(define job-mule (list 5 0 1))
(define job-financer (list 3 8 5))
(define job-bribe (list 5 6 10))

;; A Job-list is a (listof Job)

(define my-jobs (list job-mule job-financer job-bribe))

;; purpose
;; (eval-goon) consumes a Goon and a Job, and produces false if one or more of
;;  the goon's abilities do not meet the requirements for the job. If all
;;  requirements are met, the function produces a Nat that expresses how
;;  qualified the goon is. This number is calculated by adding up all difference
;;  between the goon's abilities and the job's requirements.

;; contract
;; eval-goon: Goon Job -> (Anyof false Nat)

;; example
(check-expect (eval-goon goon-ca job-mule) false)

;; function
(define (eval-goon goon job)
  (cond[(or (< (first (goon-abilities goon)) (first job))
            (< (second (goon-abilities goon)) (second job))
            (< (third (goon-abilities goon)) (third job))) false]
       [else (+ (- (first (goon-abilities goon)) (first job))
                (- (second (goon-abilities goon)) (second job))
                (- (third (goon-abilities goon)) (third job)))]))

;; test
(check-expect (eval-goon goon-ca job-financer) 4)
(check-expect (eval-goon goon-fff job-mule) 9)
(check-expect (eval-goon goon-btt job-bribe) false)
(check-expect (eval-goon goon-btt job-financer) false)
(check-expect (eval-goon goon-btt job-mule) 9)
(check-expect (eval-goon goon-ca job-bribe) false)
(check-expect (eval-goon goon-btb job-mule) false)
(check-expect (eval-goon goon-btb job-financer) false)

;; purpose
;; (pick-goon gang job) consumes a Gang and a Job. Then produces the Goon that
;;  is most qualified for the job. If multiples goons are equally qualified, it
;;  will produces the first qualified one from the Gang. If no goon is qualified
;;  then produces false.
;; (pick-goon/check gang job) consumes a Gang and a Job. Then produces a Bool
;;  which represent whether one of the Goons in Gang meet the job requirements.
;; (pick-goon/acc gang job acc) consumes a Gang, a Job and an accelerater which
;;  represents up to now, who is the most qualified for the job (false if have
;;  not found yet)

;; contract
;; pick-goon: Gang Job -> (Anyof false Goon)
;; pick-goon/check: Gang Job -> Bool
;; pick-goon/acc: Gang Job (Anyof false Goon) -> (Anyof false Goon)

;; example
(check-expect (pick-goon my-gang job-mule) goon-btt)
(check-expect (pick-goon/check my-gang job-bribe) false)
(check-expect (pick-goon/acc my-gang job-financer false) goon-ca)

;; function
(define (pick-goon/acc gang job acc)
  (cond[(empty? gang) acc]
       [(and (false? acc) (integer? (eval-goon (first gang) job)))
        (pick-goon/acc (rest gang) job (first gang))]
       [(and (false? acc) (not (integer? (eval-goon (first gang) job))))
        (pick-goon/acc (rest gang) job acc)]
       [(and (integer? (eval-goon (first gang) job))
             (> (eval-goon (first gang) job) (eval-goon acc job)))
        (pick-goon/acc (rest gang) job (first gang))]
       [else (pick-goon/acc (rest gang) job acc)]))

(define (pick-goon/check gang job)
  (cond[(empty? gang) false]
       [(integer? (eval-goon (first gang) job)) true]
       [else (pick-goon/check (rest gang) job)]))

(define (pick-goon gang job)
  (cond[(pick-goon/check gang job) (pick-goon/acc gang job false)]
       [else false]))

;; test
(check-expect (pick-goon my-gang job-bribe) false)
(check-expect (pick-goon my-gang job-financer) goon-ca)
(check-expect (pick-goon (cons applicant-s my-gang) job-mule) applicant-s)
(check-expect (pick-goon my-gang2 job-mule) applicant-s)
(check-expect (pick-goon empty job-financer) false)

;; purpose
;; (find-difficult-jobs gang job-list) consumes a Gang and a Job-list, and
;;  produces a Job-list that contains all jobs that are too difficult for
;;  the gang-members to complete.

;; contract
;; find-difficult-jobs: Gang Job-list -> Job-list

;; example
(check-expect (find-difficult-jobs my-gang my-jobs) (list job-bribe))

;; function
(define (find-difficult-jobs gang job-list)
  (cond[(empty? job-list) empty]
       [(pick-goon/check gang (first job-list))
        (find-difficult-jobs gang (rest job-list))]
       [else (cons (first job-list)
                   (find-difficult-jobs gang (rest job-list)))]))

;; test
(check-expect (find-difficult-jobs my-gang empty) empty)
(check-expect (find-difficult-jobs empty empty) empty)

;; purpose
;; (hire? gang job-list applicant) consumes a Gang, a Job-list, and a Goon(this
;;  parameter repreesents the applicant), and produces true if the applicant is
;;  qualified to perform a job that none of the current gang-members can, and
;;  false if every job is already covered by at least one current gang-member.
;; (check difficult-job-list applicant) consumes a Job-list and a Goon, and
;;  produces true if the applicant is qualified to perform a job in the
;;  difficult-job-list, false otherwise.

;; contract
;; hire?: Gang Job-list Goon -> Bool
;; check: Job-list Goon -> Bool

;; example
(check-expect (hire? my-gang my-jobs applicant-gg) false)
(check-expect (check empty applicant-gg) false)

;; function
(define (check difficult-job-list applicant)
  (cond[(empty? difficult-job-list) false]
       [(integer? (eval-goon applicant (first difficult-job-list))) true]
       [else (check (rest difficult-job-list) applicant)]))

(define (hire? gang job-list applicant)
  (cond[(empty? (find-difficult-jobs gang job-list)) false]
       [else (check (find-difficult-jobs gang job-list) applicant)]))

;; test
(check-expect (hire? my-gang my-jobs applicant-s) true)
(check-expect (hire? my-gang empty applicant-s) false)
(check-expect (hire? empty my-jobs applicant-gg) false)