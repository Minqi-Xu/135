;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname not-catan) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment06, Problem 1

(define-struct inventory (blocks wood sheep wheat rocks))
;; An Inventory is a (make-inventory Nat Nat Nat Nat Nat)
;; requires: wheat >= sheep
;;           wheat >= 4

(define-struct cost (blocks wood sheep wheat rocks))
;; A Cost is a (make-cost Nat Nat Nat Nat Nat)

;; template's contract
;; inventory-template: Inventory -> Any

;; template
(define (inventory-template inventory)
  (... (inventory-blocks inventory) ...
       (inventory-wood inventory) ...
       (inventory-sheep inventory) ...
       (inventory-wheat inventory) ...
       (inventory-rocks inventory) ...))

;; purpose
;; (valid-inventory? inventory) consumes Any as an argument and produces ture
;;  if the argument is a valid Inventory.

;; contract
;; valid-inventory?: Any -> Bool

;; example
(check-expect (valid-inventory? (make-inventory 3 2 4 5 1)) true)

;; function
(define (valid-inventory? inventory)
  (and (inventory? inventory)
       (integer? (inventory-blocks inventory))
       (>= (inventory-blocks inventory) 0)
       (integer? (inventory-wood inventory))
       (>= (inventory-wood inventory) 0)
       (integer? (inventory-sheep inventory))
       (>= (inventory-sheep inventory) 0)
       (integer? (inventory-wheat inventory))
       (>= (inventory-wheat inventory) 0)
       (>= (inventory-wheat inventory) (inventory-sheep inventory))
       (>= (inventory-wheat inventory) 4)
       (integer? (inventory-rocks inventory))
       (>= (inventory-rocks inventory) 0)))

;; test
(check-expect (valid-inventory? (make-inventory 3 2 2 3 1)) false)
(check-expect (valid-inventory? (list 3 2 4 5 1)) false)
(check-expect (valid-inventory? 'inventory) false)
(check-expect (valid-inventory? (make-inventory 0 2 3 1000000 5)) true)
(check-expect (valid-inventory? (make-inventory -1 2 3 4 5)) false)

;; purpose
;; (affordable? inventory cost) consumes an Inventory and a Cost. Then produces
;;  true if the cost is covered by the inventory and if the inventory and if the
;;  inventory remains a valid Inventory after subtracting the cost, and false
;;  otherwise.

;; contract
;; affordable?: Inventory Cost -> Bool

;; example
(check-expect
    (affordable? (make-inventory 3 2 4 5 1) (make-cost 0 2 0 1 0)) true)

;; function
(define (affordable? inventory cost)
  (and (valid-inventory? inventory)
       (valid-inventory? (make-inventory (- (inventory-blocks inventory)
                                            (cost-blocks cost))
                                         (- (inventory-wood inventory)
                                            (cost-wood cost))
                                         (- (inventory-sheep inventory)
                                            (cost-sheep cost))
                                         (- (inventory-wheat inventory)
                                            (cost-wheat cost))
                                         (- (inventory-rocks inventory)
                                            (cost-rocks cost))))))

; test
(check-expect
    (affordable? (make-inventory 3 2 5 5 1) (make-cost 0 2 0 1 0)) false)
(check-expect
    (affordable? (make-inventory 3 2 6 5 1) (make-cost 0 0 0 0 0)) false)
(check-expect
    (affordable? (make-inventory 3 2 5 5 1) (make-cost 100 100 2 0 100 )) false)

;; purpose
;; (thief inventory) consumes an Inventory as argument and attempts to steal one
;;  unit of each resource. If this is not possible, the thief leaves this one
;;  resource unchanged.
;; (thief/acc inventory acc) consumes an Inventory as argument and acceleration
;;  and does the same thing as thief with an additional acceleration which
;;  stores the kind of resource is producing.
;; (sub1-blocks inventory) consumes an Inventory as argument and produces
;;  another Inventory whose blocks is substracted by 1
;; (sub1-wood inventory) consumes an Inventory as argument and produces another
;;  Inventory whose wood is substracted by 1
;; (sub1-sheep inventory) consumes an Inventory as argument and produces another
;;  Inventory whose sheep is substracted by 1
;; (sub1-wheat inventory) consumes an Inventory as argument and produces another
;;  Inventory whose wheat is substracted by 1
;; (sub1-rocks inventory) consumes an Inventory as argument and produces another
;;  Inventory whose rocks is substracted by 1

;; contract
;; thief: Inventory -> Inventory
;; thief/acc: Inventory Nat -> Inventory
;; sub1-blocks: Inventory -> Inventory
;; sub1-wood: Inventory -> Inventory
;; sub1-sheep: Inventory -> Inventory
;; sub1-wheat: Inventory -> Inventory
;; sub1-rocks: Inventory -> Inventory

;; example
(check-expect
 (thief (make-inventory 3 2 4 5 0)) (make-inventory 2 1 3 4 0))
(check-expect
 (thief/acc (make-inventory 3 2 4 5 0) 1) (make-inventory 2 1 3 4 0))
(check-expect
 (sub1-blocks (make-inventory 3 2 4 5 0)) (make-inventory 2 2 4 5 0))
(check-expect
 (sub1-wood (make-inventory 3 2 4 5 0)) (make-inventory 3 1 4 5 0))
(check-expect
 (sub1-sheep (make-inventory 3 2 4 5 0)) (make-inventory 3 2 3 5 0))
(check-expect
 (sub1-wheat (make-inventory 3 2 4 5 0)) (make-inventory 3 2 4 4 0))
(check-expect
 (sub1-rocks (make-inventory 3 2 4 5 1)) (make-inventory 3 2 4 5 0))

;; function
(define (sub1-blocks inventory)
  (make-inventory (sub1 (inventory-blocks inventory))
                  (inventory-wood inventory)
                  (inventory-sheep inventory)
                  (inventory-wheat inventory)
                  (inventory-rocks inventory)))

(define (sub1-wood inventory)
  (make-inventory (inventory-blocks inventory)
                  (sub1 (inventory-wood inventory))
                  (inventory-sheep inventory)
                  (inventory-wheat inventory)
                  (inventory-rocks inventory)))

(define (sub1-sheep inventory)
  (make-inventory (inventory-blocks inventory)
                  (inventory-wood inventory)
                  (sub1 (inventory-sheep inventory))
                  (inventory-wheat inventory)
                  (inventory-rocks inventory)))

(define (sub1-wheat inventory)
  (make-inventory (inventory-blocks inventory)
                  (inventory-wood inventory)
                  (inventory-sheep inventory)
                  (sub1 (inventory-wheat inventory))
                  (inventory-rocks inventory)))

(define (sub1-rocks inventory)
  (make-inventory (inventory-blocks inventory)
                  (inventory-wood inventory)
                  (inventory-sheep inventory)
                  (inventory-wheat inventory)
                  (sub1 (inventory-rocks inventory))))

(define (thief/acc inventory acc)
  (cond[(and (= acc 5) (valid-inventory? (sub1-rocks inventory)))
        (sub1-rocks inventory)]
       [(and (= acc 5) (not (valid-inventory? (sub1-rocks inventory))))
        inventory]
       [(and (= acc 1) (valid-inventory? (sub1-blocks inventory)))
        (thief/acc (sub1-blocks inventory) (add1 acc))]
       [(and (= acc 1) (not (valid-inventory? (sub1-blocks inventory))))
        (thief/acc inventory (add1 acc))]
       [(and (= acc 2) (valid-inventory? (sub1-wood inventory)))
        (thief/acc (sub1-wood inventory) (add1 acc))]
       [(and (= acc 2) (not (valid-inventory? (sub1-wood inventory))))
        (thief/acc inventory (add1 acc))]
       [(and (= acc 3) (valid-inventory? (sub1-sheep inventory)))
        (thief/acc (sub1-sheep inventory) (add1 acc))]
       [(and (= acc 3) (not (valid-inventory? (sub1-sheep inventory))))
        (thief/acc inventory (add1 acc))]
       [(and (= acc 4) (valid-inventory? (sub1-wheat inventory)))
        (thief/acc (sub1-wheat inventory) (add1 acc))]
       [else (thief/acc inventory (add1 acc))]))

(define (thief inventory)
  (thief/acc inventory 1))

;; test
(check-expect
 (thief (make-inventory 3 2 4 5 1)) (make-inventory 2 1 3 4 0))
(check-expect
 (thief (make-inventory 0 2 4 5 1)) (make-inventory 0 1 3 4 0))
(check-expect
 (thief (make-inventory 3 0 4 5 1)) (make-inventory 2 0 3 4 0))
(check-expect
 (thief (make-inventory 3 2 0 5 1)) (make-inventory 2 1 0 4 0))
(check-expect
 (thief (make-inventory 3 2 4 4 1)) (make-inventory 2 1 3 4 0))
(check-expect
 (thief (make-inventory 0 0 4 5 1)) (make-inventory 0 0 3 4 0))
(check-expect
 (thief (make-inventory 0 2 0 5 1)) (make-inventory 0 1 0 4 0))
(check-expect
 (thief (make-inventory 0 2 4 5 0)) (make-inventory 0 1 3 4 0))
(check-expect
 (thief (make-inventory 3 0 0 5 1)) (make-inventory 2 0 0 4 0))
(check-expect
 (thief (make-inventory 3 0 4 5 0)) (make-inventory 2 0 3 4 0))
(check-expect
 (thief (make-inventory 3 2 0 5 0)) (make-inventory 2 1 0 4 0))
(check-expect
 (thief (make-inventory 0 0 0 5 1)) (make-inventory 0 0 0 4 0))
(check-expect
 (thief (make-inventory 0 0 4 5 0)) (make-inventory 0 0 3 4 0))
(check-expect
 (thief (make-inventory 0 0 4 5 0)) (make-inventory 0 0 3 4 0))
(check-expect
 (thief (make-inventory 0 2 0 5 0)) (make-inventory 0 1 0 4 0))
(check-expect
 (thief (make-inventory 3 0 0 5 0)) (make-inventory 2 0 0 4 0))
(check-expect
 (thief (make-inventory 0 0 0 5 0)) (make-inventory 0 0 0 4 0))