;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname football) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment02, Problem 3


;;purpose
;;  (intentional-grounding? imminent-pressure inside-pocket to-team)
;; test whether player intentional ground through 3 parameters
;;  (intentional-grounding-penalty imminent-pressure inside-pocket
;; to-team eligible-receiver in-zone) test whether player intentional gound
;; through 4 parameters(the real rule)
;;  (intentional-grounding-penalty imminent-pressure inside-pocket to-team
;; eligible-receiver in-zone) output the whether the player will get penalty
;; and what penalty will he get

;;contract
;; intentional-grounding?: Bool Bool Bool -> Bool
;; intentional-grounding-correct?: Bool Bool Bool Bool -> Bool
;; intentional-grounding-penalty: Bool Bool Bool Bool Bool -> Sym

;;example
(check-expect (intentional-grounding? true false false) false)
(check-expect (intentional-grounding-correct? true true true true) false)
(check-expect (intentional-grounding-penalty true true true false true) 'Safety)


;;Function body
(define (intentional-grounding? imminent-pressure inside-pocket to-team)
  (and imminent-pressure inside-pocket (not to-team)))

(define (intentional-grounding-correct? imminent-pressure
                                        inside-pocket
                                        to-team
                                        eligible-receiver)
  (and imminent-pressure inside-pocket (or (not to-team)
                                           (not eligible-receiver))))

(define (intentional-grounding-penalty imminent-pressure
                                       inside-pocket
                                       to-team
                                       eligible-receiver
                                       in-zone)
  (cond[(not (intentional-grounding-correct? imminent-pressure
                                         inside-pocket
                                         to-team
                                         eligible-receiver)) 'None]
        [in-zone 'Safety]
        [else '10yds]))

;;test
(check-expect (intentional-grounding? true true false) true)
(check-expect (intentional-grounding-penalty true false false true true) 'None)
(check-expect (intentional-grounding-penalty true true true false false) '10yds)