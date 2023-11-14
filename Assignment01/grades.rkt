;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS 135 2019 Fall
;; Assignment 01, Problem 4


(define (final-cs135-grade mid1 mid2 final assignment)
  (+ (* 0.1 mid1)
     (* 0.15 mid2)
     (* 0.5 final)
     (* 0.2 assignment)
     5))

(define (cs135-final-exam-grade-needed mid1 mid2 assignment)
  (/ (- 60
        (* 0.1 mid1)
        (* 0.15 mid2)
        (* 0.2 assignment)
        5)
     0.5))