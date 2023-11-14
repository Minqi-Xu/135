;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bmi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS 135 Fall 2019
;; Assignment 01, Problem 3

(define (body-mass-index m h)
  (/ m (sqr h)))


(define convert-const-pond-kg 0.45359237)
(define convert-const-inch-m 0.0254)

(define (body-mass-index-imperial pounds feet inches)
  (/ (* pounds convert-const-pond-kg)
     (sqr (+ (* 12 feet convert-const-inch-m)
             (* inches convert-const-inch-m)))))