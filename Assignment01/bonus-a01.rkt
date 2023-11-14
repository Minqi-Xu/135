;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus-a01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS 135 Fall 2019
;; Assignment 01, Problem 5


(define (cs135-participation tot-num num-correct num-wrong)
  (* 100 (/ (+ (- (* (max (* (/ 3 4) tot-num) num-correct) 2)
                  (* (abs (- (* (/ 3 4) tot-num) num-correct)) 2))
               (- (* (max (abs (- (* (/ 3 4) tot-num) num-correct))
                          num-wrong) 1)
                  (* (abs (- (* (/ 3 4) tot-num) num-correct num-wrong)) 1)))
            (* (* (/ 3 4) tot-num) 2))))