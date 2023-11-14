;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |20191025|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct card (suit value))

(define-struct hand (card1 card2 card3))

(define first-card (make-card 'hearts 3))
(define second-card (make-card 'spades 1))
(define third-card (make-card 'clubs 12))
(define my-hand (make-hand first-card second-card third-card))

(define (card-template card)
  (... (card-suit card) ...
       (card-value card) ...))

(define (template-hand hand)
  (... (hand-card1 hand) ...
       (hand-card2 hand) ...
       (hand-card3 hand) ...))

(define hearts-score 5)
(define diamonds-score 4)
(define spades-score 0)
(define clubs-score -5)

(define (suit-score card)
  (cond[(symbol=? 'hearts (card-suit card)) hearts-score]
       [(symbol=? 'diamonds (card-suit card)) diamonds-score]
       [(symbol=? 'spades (card-suit card)) spades-score]
       [(symbol=? 'clubs (card-suit card)) clubs-score]))

(define (card-score card)
  (+ (suit-score card) (card-value card)))

(define (hand-score hand)
  (+ (max (card-score (hand-card1 hand)) 0)
     (max (card-score (hand-card2 hand)) 0)
     (max (card-score (hand-card3 hand)) 0)))

