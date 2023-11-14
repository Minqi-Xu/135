;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname winsys) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment07, Problem 3

;; An RTree (Rectangle Tree) is one of:
;; * Sym (a leaf)
;; * XNode (a boundary node for an x value)
;; * YNode (a boundary node for a y value)

(define-struct xnode (val left right))
;; An XNode is a (make-xnode Nat RTree RTree)
;; requires: val > every xnode-val in left RTree
;;           val < every xnode-val in right RTree

(define-struct ynode (val below above))
;; A YNode is a (make-ynode Nat RTree RTree)
;; requires: val > every ynode-val in below RTree
;;           val < every ynode-val in above RTree

;;
;; Q3a
;;
;; rtree-template: RTree -> Any
;; xnode-template: XNode -> Any
;; ynode-template: YNode -> Any

(define (rtree-template rtree)
  (cond[(symbol? rtree) ...]
       [(xnode? rtree) (xnode-template rtree)]
       [(ynode? rtree) (ynode-template rtree)]))

(define (xnode-template xnd)
  (... (xnode-val xnd) ...
       (rtree-template (xnode-left xnd)) ...
       (rtree-template (xnode-right xnd)) ...))

(define (ynode-template ynd)
  (... (ynode-val ynd) ...
       (rtree-template (ynode-below ynd)) ...
       (rtree-template (ynode-above ynd)) ...))


;;
;; Q3b
;;

;;
;;  Test cases
;;

;; testcase 1: a square
(define l1 (make-xnode  0  'None 'Content))
(define r1 (make-xnode 70   l1   'None))
(define b1 (make-ynode  0  'None  r1))
(define t1 (make-ynode 70   b1   'None))

;; testcase 2: a simple window
(define x60 (make-xnode 60 'Menu   'x))
(define y60 (make-ynode 60 'Content x60))
(define  l2 (make-xnode  0 'None    y60))
(define  r2 (make-xnode 70  l2     'None))
(define  b2 (make-ynode  0 'None    r2))
(define  t2 (make-ynode 70  b2     'None))

;; (rt-lookup rt pos) produces the symbol from the RTree that is associated 
;; with the rectangle that contains pos
;; rt-lookup: RTree Posn -> Sym
;; (xnode-lookup xnd pos) consumes a XNode and produces the symbol
;; that is associated with the rectangle that contains pos
;; xnode-lookup: XNode Posn -> Sym
;; (ynode-lookup ynd pos) consumes a YNode and produces the symbol
;; that is associated with the rectangle that contains pos
;; ynode-lookup: Ynode Posn -> Sym
;; Examples:
(check-expect (rt-lookup 'Desktop (make-posn 2 2))
              'Desktop)
(check-expect (rt-lookup (make-xnode 5 'left 'right) (make-posn 2 2))
              'left)
(check-expect (rt-lookup (make-xnode 5 'left 'right) (make-posn 5 5))
              'right)
(check-expect (rt-lookup (make-ynode 5 'bottom 'top) (make-posn 2 2))
              'bottom)
(check-expect (rt-lookup (make-ynode 5 'bottom 'top) (make-posn 5 5))
              'top)

(define (rt-lookup rt pos)
  (cond[(symbol? rt) rt]
       [(xnode? rt) (xnode-lookup rt pos)]
       [(ynode? rt) (ynode-lookup rt pos)]))

(define (xnode-lookup xnd pos)
  (cond[(< (posn-x pos) (xnode-val xnd))
        (rt-lookup (xnode-left xnd) pos)]
       [else (rt-lookup (xnode-right xnd) pos)]))

(define (ynode-lookup ynd pos)
  (cond[(< (posn-y pos) (ynode-val ynd))
        (rt-lookup (ynode-below ynd) pos)]
       [else (rt-lookup (ynode-above ynd) pos)]))

;; pos on bottom boundary
(check-expect (rt-lookup t1 (make-posn 0 35)) 'Content)

;; pos on left boundary
(check-expect (rt-lookup t1 (make-posn 35 0)) 'Content)

;; pos on top boundary
;; recall that rectangle only includes x < (xnode-val rt)
(check-expect (rt-lookup t1 (make-posn 70 35)) 'None)

;; pos on right boundary
;; recall that rectangle only includes y < (ynode-val rt)
(check-expect (rt-lookup t1 (make-posn 35 70)) 'None)

;;
;; Q3c
;;
;; (rt-max-x rt)
;; produce the maximum value of any XNode in rt
;; or produce 'None if there are no XNodes in rt
;; rt-max-x: RTree -> (anyof Nat 'None)
;; (rt-max-x/acc rt x-max) produce the maximum value of any XNode in rt
;; or produce x-max if there are no Xnodes in rt with recording an accelerator
;; x-max which store the current maximum Xnode-val
;; rt-max-x/acc: RTree Int -> Int
;; (xnode-max-x/acc xnd x-max) produce the maximum value of any XNode in xnd
;; or produce x-max if there are no Xnodes in xnd with recording an accelerator
;; xnode-max-x/acc: XNode Int -> Int
;; (ynode-max-x/acc xnd x-max) produce the maximum value of any XNode in ynd
;; or produce x-max if there are no Xnodes in ynd with recording an accelerator
;; ynode-max-x/acc: YNode Int -> Int
;; examples:
(check-expect (rt-max-x 'a) 'None)
(check-expect (rt-max-x (make-ynode 8 'a 'b)) 'None)
(check-expect (rt-max-x (make-xnode 8 'a 'b)) 8)
(check-expect (rt-max-x/acc 'a -9999999999) -9999999999)
(check-expect (xnode-max-x/acc (make-xnode 8 'a 'b) -9999999999) 8)
(check-expect (ynode-max-x/acc (make-ynode 8 'a 'b) -9999999999) -9999999999)

(define (xnode-max-x/acc xnd x-max)
  (max (rt-max-x/acc (xnode-left xnd) x-max)
       (xnode-val xnd)
       (rt-max-x/acc (xnode-right xnd) x-max)))

(define (ynode-max-x/acc ynd x-max)
  (max (rt-max-x/acc (ynode-below ynd) x-max)
       (rt-max-x/acc (ynode-above ynd) x-max)))

(define (rt-max-x/acc rt x-max)
  (cond[(symbol? rt) x-max]
       [(xnode? rt) (xnode-max-x/acc rt x-max)]
       [(ynode? rt) (ynode-max-x/acc rt x-max)]))

(define (rt-max-x rt)
  (cond[(= -9999999999 (rt-max-x/acc rt -9999999999)) 'None]
       [else (rt-max-x/acc rt -9999999999)]))

;; test
(check-expect (rt-max-x t1) 70)
(check-expect (rt-max-x t2) 70)
  
;;
;; Q3d
;;

(define-struct win (wid rtree))
;; A Win (Window) is a (make-win Nat RTree)
;; requires: points outside the window are labelled 'None


;; (move-win wi x y)
;; moves the Win wi, x pixels along the x-axis
;; and y pixels in the y-axis.
;; move-win: Win Int Int -> Win
;; (move-rt rt x y) moves the RTree rt, x pixels along the x-axis
;; and y pixels in the y-axis.
;; move-rt: RTree Int Int -> RTree
;; examples:
(check-expect (move-win (make-win 12 'a) 1 3)
              (make-win 12 'a))
(check-expect (move-rt (make-xnode 8 'a 'b) 1 4)
              (make-xnode 9 'a 'b))
(check-expect (move-rt (make-ynode 8 'a 'b) 1 5)
              (make-ynode 13 'a 'b))

(define (move-rt rt x y)
  (cond[(symbol? rt) rt]
       [(xnode? rt)
        (make-xnode (+ (xnode-val rt) x)
                    (move-rt (xnode-left rt) x y)
                    (move-rt (xnode-right rt) x y))]
       [(ynode? rt)
        (make-ynode (+ (ynode-val rt) y)
                    (move-rt (ynode-below rt) x y)
                    (move-rt (ynode-above rt) x y))]))

(define (move-win wi x y)
  (cond[(symbol? (win-rtree wi)) wi]
       [else (make-win (win-wid wi)
                       (move-rt (win-rtree wi) x y))]))

(check-expect (move-win (make-win 9 (make-xnode 8 'a 'b)) 1 4)
              (make-win 9 (make-xnode 9 'a 'b)))
(check-expect (move-win (make-win 10 (make-ynode 8 'a 'b)) 1 3)
              (make-win 10 (make-ynode 11 'a 'b)))
(check-expect (move-win (make-win 1 t2) -10 -10)
              (make-win 1 (make-ynode 60
                            (make-ynode -10 'None
                               (make-xnode 60
                                 (make-xnode -10 'None
                                   (make-ynode 50 'Content
                                     (make-xnode 50 'Menu 'x)))
                                 'None)) 'None)))

;;
;; Q3e
;;


;; A WinSys is one of:
;; * empty
;; * (cons Win WinSys)


;; (winsys-lookup ws pos)
;; produces the wid of the top window in ws that contains pos
;; and the Sym correspond to the rectange that contains pos
;; winsys-lookup: WinSys Posn -> (list Nat Sym)

;; (winsys-lookup/sorted ws pos) produces the wid of the top window
;; in ws (which is sorted already) taht contains pos and the Sym
;; correspond to the rectange that contains pos
;; winsys-lookup: WinSys Posn -> (list Nat Sym)
;; require:   ws must be sorted non-decreasing based on win-wid

;; (sort-wi ws) produces the sorted list of Win decreasingly based on win-wid
;; sort-wi: WinSys -> WinSys
;; (insert wi ws) insert the Win (wi) into the ordered WinSys at the
;; correct position
;; insert: Win WinSys -> WinSys
;; require:   ws must be sorted non-decreasing based on win-wid

(check-expect (winsys-lookup empty (make-posn 1 4))
              (list 0 'None))
(check-expect (winsys-lookup/sorted (list (make-win 1 (make-ynode 8 'a 'b))
                                          (make-win 3 (make-xnode 8 'a 'b)))
                                    (make-posn 2 2)) (list 1 'a))
(check-expect (sort-wi (list (make-win 3 (make-xnode 8 'a 'b))
                                          (make-win 1 (make-ynode 8 'a 'b))))
              (list (make-win 1 (make-ynode 8 'a 'b))
                    (make-win 3 (make-xnode 8 'a 'b))))
(check-expect (insert (make-win 2 (make-xnode 3 'c 'd))
                      (list (make-win 1 (make-ynode 8 'a 'b))
                            (make-win 3 (make-xnode 8 'a 'b))))
              (list (make-win 1 (make-ynode 8 'a 'b))
                    (make-win 2 (make-xnode 3 'c 'd))
                    (make-win 3 (make-xnode 8 'a 'b))))

(define (insert wi ws)
  (cond[(empty? ws) (cons wi empty)]
       [(> (win-wid wi) (win-wid (first ws)))
        (cons (first ws) (insert wi (rest ws)))]
       [else (cons wi ws)]))

(define (sort-wi ws)
  (cond[(empty? ws) empty]
       [else (insert (first ws)
                     (sort-wi (rest ws)))]))

(define (winsys-lookup/sorted ws pos)
  (cond[(empty? ws) (list 0 'None)]
       [(symbol=? 'None (rt-lookup (win-rtree (first ws)) pos))
        (winsys-lookup/sorted (rest ws) pos)]
       [else (list (win-wid (first ws))
                   (rt-lookup (win-rtree (first ws)) pos))]))

(define (winsys-lookup ws pos)
  (winsys-lookup/sorted (sort-wi ws) pos))

(check-expect (winsys-lookup (list (make-win 2 t1)
                                   (make-win 1 t2))
                             (make-posn 50 65))
              (list 1 'Menu))
(check-expect (winsys-lookup (list (move-win (make-win 2 t2) 10 10)
                                   (make-win 1 t1))
                             (make-posn 75 75))
              (list 2 'x))