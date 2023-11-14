;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname shortwalk) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment03, Problem 5

;; purpose
;;  (make-point x y) consumes x and y and produces the list of length 2
;; containing x y
;;  (x-coord point) accesses the x coordinate of point
;;  (y-coord point) accesses the y coordinate of point
;;  (make-step start direction distance) consumes a startingpoint one the
;; plain, a direction and a  distance and produces a new point which is
;; the position after moving the specified distance in the specified direction
;;        requires:     distance must be 'N or 'S or 'E or 'W
;;  (two-steps start direction distance) consumes a starting point, a list of
;; two directions and a list of two distances and produces a new point on the
;; plane which is the position after following the two steps.
;;        requires:     elements of distance must be 'N or 'S or 'E or 'W


;; contract
;; make-point: Num Num -> (listof Num)
;; x-coord: (listof Num) -> Num
;; y-coord: (listof Num) -> Num
;; make-step: (listof Num) Sym Num -> (listof Num)
;; make-step: (listof Num) (listof Sym) (listof Num) -> (listof Num)

;; example
(check-expect (make-point 5 6) (cons 5 (cons 6 empty)))
(check-expect (x-coord (cons 5 (cons 6 empty))) 5)
(check-expect (y-coord (cons 5 (cons 6 empty))) 6)
(check-expect (make-step (make-point 2 3) 'E 4) (make-point 6 3))
(check-expect (two-steps (make-point 1 -1)
                         (cons 'N (cons 'E empty))
                         (cons 2 (cons 3 empty)))
              (make-point 4 1))

;;helper function
(define (make-point x y)
  (cons x (cons y empty)))

(define (x-coord point)
  (first point))

(define (y-coord point)
  (first (rest point)))

;;function body
(define (make-step start direction distance)
  (cond[(symbol=? direction 'N)
        (cons (x-coord start) (cons(+ (y-coord start) distance) empty))]
       [(symbol=? direction 'S)
        (cons (x-coord start) (cons(- (y-coord start) distance) empty))]
       [(symbol=? direction 'E)
        (cons (+ (x-coord start) distance) (rest start))]
       [else (cons (- (x-coord start) distance) (rest start))]))

(define (two-steps start direction distance)
  (make-step (make-step start (first direction) (first distance))
             (first (rest direction))
             (first (rest distance))))


;;test
(check-expect (make-step (make-point 5 -3) 'S 2) (make-point 5 -5))
(check-expect (make-step (make-point 0 0) 'W 4) (make-point -4 0))
(check-expect (two-steps (make-point 3 3)
                         (cons 'E (cons 'S empty))
                         (cons 4 (cons 1 empty)))
              (make-point 7 2))
(check-expect (two-steps (make-point 1 1)
                         (cons 'N (cons 'W empty))
                         (cons 5 (cons 10 empty)))
              (make-point -9 6))