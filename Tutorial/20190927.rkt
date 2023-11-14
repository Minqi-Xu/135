;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |20190927|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define my-list (cons 1 (cons 2 (cons 3 empty))))

(define (three-two-D my-list)
  (cons (first my-list) (rest (rest my-list))))

(define (find-x point)
  (first point))

(define (find-y point)
  (first (rest point)))

(define (slope point1 point2)
  (/ (- (find-y point1) (find-y point2)) (- (find-x point1) (find-x point2))))

(define (perpendicular? point1 point2 point3 point4)
  (= -1 (* (slope point1 point2) (slope point3 point4))))
;; haven't do the cond of the horizontal and vertical to avoid divided by 0