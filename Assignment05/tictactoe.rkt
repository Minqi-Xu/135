;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tictactoe-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment05, Problem 4

;; A Tic-Tac-Toe Grid (T3Grid) is a (listof (listof (anyof 'X 'O '_)))
;; requires: all lists have the same length, and that length is odd
;;           The number of 'X and 'O is equal, or there is exactly one more 'X


;; Helpful Constants

(define grid1 '((_ _ _)
                (_ _ _)
                (_ _ _)))

(define grid2 '((X O O O _)
                (X X O _ _)
                (_ _ _ _ _)
                (_ _ _ _ _)
                (_ _ _ _ X)))

(define grid3 '((X)))

;; Note to students: The above O is the letter "Oh", not the number zero.

;; purpose
;;  (whose-turn t3grid) consumes a T3Grid and determines whose turn it is.
;; X goes first. If X goes next, then produce 'X, otherwise produce 'O.
;;  (count/2D t3gride sym) consumes a T3Gride and a symbol. Produce the
;; amount of the symbol sym in the T3Gride.
;;  (count/1D list sym) consumes a (listof Sym) and a symbol. Produce the
;; amount of the symbol sym in the (list of Sym)

;; contract
;; whose-turn: T3Grid -> (Anyof 'X 'O)
;; count/2D: T3Grid Sym -> Nat
;; count/1D: (listof Sym) Sym -> Nat

;; example
(check-expect (whose-turn grid1) 'X)
(check-expect (count/2D grid2 'O) 4)
(check-expect (count/1D '(X O X) 'X) 2)

;; function
(define (count/1D list sym)
  (cond[(empty? list) 0]
       [(symbol=? sym (first list))
        (+ 1 (count/1D (rest list) sym))]
       [else (count/1D (rest list) sym)]))

(define (count/2D t3grid sym)
  (cond[(empty? t3grid) 0]
       [else (+ (count/1D (first t3grid) sym)
                (count/2D (rest t3grid) sym))]))

(define (whose-turn t3grid)
  (cond[(zero? (- (count/2D t3grid 'X) (count/2D t3grid 'O))) 'X]
       [else 'O]))

;; test
(check-expect (whose-turn grid2) 'X)
(check-expect (whose-turn grid3) 'O)

;; purpose
;;  (grid-ref t3grid row col) consumes a T3grid and a row and column number, and
;; produces the symbol located at that location. (Row and column number start
;; counting from 0)
;;  (find-row t3grid row current) consumes a 2D list and a row number and
;; current row number. And produce the row (listof Sym) on the specific row
;; numbered row (start counting from 0)

;; contract
;; grid-ref: T3Grid Nat Nat -> (Anyof 'X 'O '_)
;; find-row: (listof (listof (Anyof 'X 'O '_))) Nat Nat -> (listof (Anyof 'X 'O '_))

;; example
(check-expect (grid-ref grid2 1 2) 'O)
(check-expect (find-row grid2 1 0) '(X X O _ _))

;; functions
(define (find-row t3grid row current)
  (cond[(= row current) (first t3grid)]
       [else (find-row (rest t3grid) row (add1 current))]))

(define (grid-ref t3grid row col)
  (list-ref (find-row t3grid row 0) col))

;; test
(check-expect (grid-ref grid1 2 1) '_)
(check-expect (grid-ref grid2 0 0) 'X)
(check-expect (grid-ref grid3 0 0) 'X)

;; purpose
;;  (get-column t3grid col) consums a T3Grid, and a column number, and produces
;; a list of the symbols in that column

;; contract
;; get-column: (listof (listof (Anyof 'X 'O '_))) Nat -> (listof (Anyof 'X 'O '_))

;; example
(check-expect (get-column grid2 1) '(O X _ _ _))

;; function
(define (get-column t3grid col)
  (cond[(empty? t3grid) empty]
       [else (cons (list-ref (first t3grid) col)
                   (get-column (rest t3grid) col))]))

;; test
(check-expect (get-column grid2 0) '(X X _ _ _))
(check-expect (get-column grid1 0) '(_ _ _))
(check-expect (get-column grid3 0) '(X))
(check-expect (get-column grid2 3) '(O _ _ _ _))

;; purpose
;;  (will-win? t3grid row col sym) consumes a T3Grid, a row number, a column
;; number, and a player (either 'X or 'O). Then produces true if that player
;; would win by placing a marker at the given location, and false otherwise.
;; Note that if the player making an illegal move, it will produce false.
;;  (judge los sym flag current) consumes a (listof Sym) and a Symbol sym, and
;; a flag and and the current location. Then produces true if all the elements
;; (ignore the difference while current location is at the flag)in los is
;; exactly sym, and false otherwise.

;; contract
;; will-win?: T3Grid Nat Nat (Anyof 'X 'O) -> Bool
;; judge: (listof (Anyof 'X 'O '_)) (Anyof 'X 'O) -> Bool

;; example
(check-expect (will-win? grid1 0 0 'X) false)
(check-expect (judge '(X X X O X) 'X 3 0) true)

;; functions
(define (judge los sym flag current)
  (cond[(empty? los) true]
       [(= flag current) (judge (rest los) sym flag (add1 current))]
       [(not (symbol=? sym (first los))) false]
       [else (judge (rest los) sym flag (add1 current))]))

(define (will-win? t3grid row col sym)
  (cond[(not (symbol=? '_ (grid-ref t3grid row col))) false]
       [(not (symbol=? sym (whose-turn t3grid))) false]
       [else (or (judge (find-row t3grid row 0) sym col 0)
                 (judge (get-column t3grid col) sym row 0))]))

;; test
(check-expect (will-win? '((X X _)
                           (O X O)
                           (O _ _))
                         0 2 'X) true)
(check-expect (will-win? '((X O X)
                           (O X O)
                           (O X _))
                         2 2 'X) false)
(check-expect (will-win? '((X O X)
                           (O X O)
                           (O X _))
                         0 1 'X) false)
(check-expect (will-win? '((X O X)
                           (O X O)
                           (O X _))
                         2 2 'O) false)