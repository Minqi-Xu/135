;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname box-office) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment02, Problem 2

;;purpose
;;  (box-office-profits name studio num-actor num-explosion) Calculate the
;; box-office profits withfour factors name, studio, famous actors and
;; explosions
;;  (check-name name) Check the name of the movie and calculate whether
;; should receive any bonus/ penalty
;;  (check-studio studio) Check the name of the studio of the movie and
;; calculate whether should receive any bonus/ penalty
;;  (calculate-actor num-actor) Calculate the bonus for famous actors
;;  (calculate-explosion num-explosion) Calculate the bonus/penalty for
;; explosions

;; Contract
;; box-office-profits: Str Str Nat Nat -> Int
;; check-name: Str -> Int
;; check-studio: Str -> Int
;; calculate-actor: Nat -> Nat
;; calculate-explosion: Nat -> Int

;; Example
(check-expect (box-office-profits "Avengers: more endgames"
                                  "Marvel"
                                  4
                                  50)
              980)
(check-expect (check-name "Avengers: more endgames") 0)
(check-expect (check-studio "Marvel") 500)
(check-expect (calculate-actor 4) 200)
(check-expect (calculate-explosion 50) 280)


;; helper function
(define (check-name name)
  (cond[(equal? (substring name 0 3) "The")
        (cond[(< (string-length name) 10) (- 25 50)]
             [else (- 50)])]
       [else
        (cond[(< (string-length name) 10) 25]
             [else 0])]))

(define (check-studio studio)
  (cond[(equal? studio "Marvel") 500]
       [(equal? studio "DC") (- 250)]
       [else 0]))

(define (calculate-actor num-actor)
  (* 50 num-actor))

(define (calculate-explosion num-explosion)
  (+ (- 20) (* 6 num-explosion)))

;; Function Body
(define (box-office-profits name studio num-actor num-explosion)
  (+ (check-name name)
     (check-studio studio)
     (calculate-actor num-actor)
     (calculate-explosion num-explosion)))

;; Test
(check-expect (box-office-profits "Superman v Superman" "DC" 2 100) 430)
(check-expect (box-office-profits "The Slog" "New Line Cinema" 0 0) (- 45))
(check-expect (box-office-profits "The ABCDEFGHIJK" "Marvel" 1 4) 504)
(check-expect (box-office-profits "Rise" "DC" 5 500) 3005)