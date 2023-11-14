;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment02, Problem 1

;; a)
(define (q1a-alt n a?)
  (cond [(and a? (>= n 0)) (add1 n)]
        [(and a? (< n 0)) (sub1 n)]
        [else 0]))

;; b)
(define (q1b-alt a? b? c?)
  (cond [(and a? b?) 'elm]
        [(and a? (not c?)) 'birch]
        [a? 'cedar]
        [(and (not a?) b?) 'pine]
        [(and (not a?) (not c?)) 'birch]
        [else 'cherry]))

;; c)
(define (q1c-alt a? b? c?)
  (cond [(and c? a?) 'oak]
        [(and c? b?) 'maple]
        [c? 'willow]
        [(and a? b?) 'walnut]
        [a? 'dogwood]
        [(not b?) 'sumac]
        [else 'buckthorn]))

;; d)
(define (q1d-alt a? b? c?)
  (cond [(and c? b?) 'spruce]
        [(and (not (equal? c? b?)) (equal? a? true)) 'hazel]
        [(and (equal? c? true) (equal? b? false) (equal? a? false)) 'hickory]
        [(and (equal? c? false) (equal? b? true) (equal? a? false)) 'spruce]
        [(and (not a?) (not b?) (not c?)) 'larch]
        [else 'hazel]))