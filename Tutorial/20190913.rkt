;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |20190913|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (f a b)
  (sqrt(+ (sqr a) (sqr b))));;(sqrt(+ (* a a) (* b b))))

(define standard 1.5)

(define taste-weight 0.25)

(define cost-weight 0.3)

(define sandwich-like-weight 0.3)

(define (evaluate-food x y z)
  (+ standard
     (* x taste-weight)
     (* y cost-weight)
     (* z sandwich-like-weight)))

(evaluate-food 5 7 10)

(define (pull x y)
  (abs(quotient (remainder x (expt 10 (+ y 1))) (expt 10 y))))