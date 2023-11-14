;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname book-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment05, Problem 3

;; A Book is a (list Str Str)

;; useful constants for the examples 
(define my-bookshelf '(("The Colour of Magic" "Pratchett, Terry")
                       ("Mostly Harmless" "Adams, Douglas")
                       ("Pyramids" "Pratchett, Terry")
                       ("A Brief History of Time" "Hawking, Stephen")))

(define discworld-books '(("The Colour of Magic" "Pratchett, Terry")
                          ("Pyramids" "Pratchett, Terry")))

;; contract
;; book-template: Book -> Any
;; listof-book-template: (listof Book) -> Any

;; templates
(define (book-template book)
  (... (first book) ...
   ... (second book) ...))

(define (listof-book-template lob)
  (cond[(empty? lob) ... ]
       [else ( ... (first lob) ...
               ... (listof-book-template (rest lob)) ...)]))

;; purpose
;;  (sort-books lob) consumes a (listof Book) and produces a list of the same
;; Books in shelf-order. Which means that the books are sorted according
;; lexicographically to the author's name("Last, First" format). Books by the
;; same author will be sorted lexicographically by their title.
;;  (insert book slob) inserts the Book into the sorted (listof Book) slob so
;; that the resulting (listof Book) is also sorted
;;   requires: slob is sorted in nondecreasing order

;; contract
;; sort-books: (listof Book) -> (listof Book)
;; insert: Book (listof Book) -> (listof Book)

;; example
(check-expect (sort-books my-bookshelf)
              '(("Mostly Harmless" "Adams, Douglas")
                ("A Brief History of Time" "Hawking, Stephen")
                ("Pyramids" "Pratchett, Terry")
                ("The Colour of Magic" "Pratchett, Terry")))

(check-expect (insert '("TBC" "Pratchett, Terry")
                      '(("Abc" "Pratchett, Terry")
                        ("EBC" "Pratchett, Terry")
                        ("ZBC" "Pratchett, Terry")))
              '(("Abc" "Pratchett, Terry")
                ("EBC" "Pratchett, Terry")
                ("TBC" "Pratchett, Terry")
                ("ZBC" "Pratchett, Terry")))

;; functions
(define (insert book slob)
  (cond[(empty? slob) (cons book empty)]
       [(string>? (second book) (second (first slob)))
        (cons (first slob) (insert book (rest slob)))]
       [(and (string=? (second book) (second (first slob)))
             (string>? (first book) (first (first slob))))
        (cons (first slob) (insert book (rest slob)))]
       [else (cons book slob)]))

(define (sort-books lob)
  (cond[(empty? lob) empty]
       [else (insert (first lob) (sort-books (rest lob)))]))

;; test
(check-expect (sort-books discworld-books)
              '(("Pyramids" "Pratchett, Terry")
                ("The Colour of Magic" "Pratchett, Terry")))
(check-expect (sort-books empty) empty)
(check-expect (sort-books '(("TBC" "Pratchett, Terry")
                            ("Abc" "Pratchett, Terry")
                            ("EBC" "Pratchett, Terry")
                            ("ZBC" "Pratchett, Terry")))
              '(("Abc" "Pratchett, Terry")
                ("EBC" "Pratchett, Terry")
                ("TBC" "Pratchett, Terry")
                ("ZBC" "Pratchett, Terry")))
(check-expect (sort-books (cons '("QBBBB" "Pratchett, Terry")
                                my-bookshelf))
              '(("Mostly Harmless" "Adams, Douglas")
                ("A Brief History of Time" "Hawking, Stephen")
                ("Pyramids" "Pratchett, Terry")
                ("QBBBB" "Pratchett, Terry")
                ("The Colour of Magic" "Pratchett, Terry")))

;; purpose
;;  (books-by-author lob author) consumes a (listof Book) and the name of an
;; author, and produces a (listof Book) containing just the books written by
;; that author. The order of the books is unchanged.

;; contract
;; books-by-author: (listof Book) Str -> (listof Book)

;; example
(check-expect (books-by-author my-bookshelf "Pratchett, Terry")
              discworld-books)

;; function
(define (books-by-author lob author)
  (cond[(empty? lob) empty]
       [(string=? (second (first lob)) author)
        (cons (first lob) (books-by-author (rest lob) author))]
       [else (books-by-author (rest lob) author)]))

;; test
(check-expect (books-by-author my-bookshelf "King, Stephen")
              empty)
(check-expect (books-by-author my-bookshelf "Adams, Douglas")
              '(("Mostly Harmless" "Adams, Douglas")))
(check-expect (books-by-author (cons '("ABDJK" "Hawking, Stephen")
                                     my-bookshelf) "Hawking, Stephen")
              '(("ABDJK" "Hawking, Stephen")
                ("A Brief History of Time" "Hawking, Stephen")))

;; An AuthorIndex is a (listof (cons Str (listof Str)))
;; requires: The first Str in each inner list is unique

;; useful constants for the examples
(define my-index '(("Pratchett, Terry" "The Colour of Magic" "Pyramids")
                   ("Hawking, Stephen" "A Brief History of Time")
                   ("Adams, Douglas")))

;; purpose
;;  (book-by-author? authorindex author title) consumes an AuthorIndex, an
;; author, and a book title, and produces true if the index contains a book
;; by that name and author, false otherwise.
;;  (book-by-author?/1D books title) consumes an (listof Str) which represents
;; the names of books and a book title. Then produce true if the list contains
;; a book by that name, false otherwise.

;; contract
;; book-by-author?: AuthorIndex Str Str -> Bool
;; book-by-author?/1D: (listof Str) Str -> Bool

;; examples
(check-expect (book-by-author? my-index
                               "Pratchett, Terry"
                               "The Colour of Magic") true)
(check-expect (book-by-author?/1D '("QS" "AG" "IG" "WFD") "AS") false)

;; functions
(define (book-by-author?/1D books title)
  (cond[(empty? books) false]
       [(string=? title (first books)) true]
       [else (book-by-author?/1D (rest books) title)]))

(define (book-by-author? authorindex author title)
  (cond[(empty? authorindex) false]
       [(string=? author (first (first authorindex)))
        (book-by-author?/1D (rest (first authorindex)) title)]
       [else (book-by-author? (rest authorindex) author title)]))

;; test
(check-expect (book-by-author? my-index
                               "Pratchett, Terry"
                               "Pyramids") true)
(check-expect (book-by-author? my-index
                               "Pratchett, Terry"
                               "IT") false)
(check-expect (book-by-author? empty
                               "AA"
                               "BB") false)
(check-expect (book-by-author? my-index
                               "Hawking, Stephen"
                               "A Brief History of Time") true)
(check-expect (book-by-author? my-index
                               "Hawking, Stephen"
                               "IT") false)
(check-expect (book-by-author? my-index
                               "Adams, Douglas"
                               "IT") false)
(check-expect (book-by-author? my-index
                               "AAAA"
                               "Pyramids") false)
;; purpose
;;  (build-author-index lob loa) consumes a (listof Book) and a list of unique
;; authors (Strings). Then produces an AuthorIndex where the keys are the
;; authors consumed (in the same order) and the values are the titles of all
;; books by that author in the list of Book.
;;  (build-author-index lob author) consumes a (listof Book) and a author's name
;; then produces an single element of AuthorIndex (cons author (listof titles)).
;;   requirments:  the name of authors in loa must occured in lob

;; contract
;; build-author-index: (listof Book) (listof Str) -> AuthorIndex
;; build-author-index/1D: (listof Book) Str -> (listof Str)

;; examples
(check-expect (build-author-index my-bookshelf '("Adams, Douglas"
                                                 "Hawking, Stephen"))
              '(("Adams, Douglas" "Mostly Harmless")
                ("Hawking, Stephen" "A Brief History of Time")))

(check-expect (build-author-index/1D my-bookshelf "Adams, Douglas")
              '("Mostly Harmless"))

;; functions
(define (build-author-index/1D lob author)
  (cond[(empty? lob) empty]
       [(string=? author (second (first lob)))
        (cons (first (first lob))
              (build-author-index/1D (rest lob) author))]
       [else (build-author-index/1D (rest lob) author)]))

(define (build-author-index lob loa)
  (cond[(empty? loa) empty]
       [else (cons (cons (first loa) (build-author-index/1D lob (first loa)))
                   (build-author-index lob (rest loa)))]))

;; test
(check-expect (build-author-index my-bookshelf empty) empty)
(check-expect (build-author-index my-bookshelf '("Pratchett, Terry"))
              '(("Pratchett, Terry" "The Colour of Magic" "Pyramids")))
(check-expect (build-author-index my-bookshelf '("Adams, Douglas"
                                                 "Pratchett, Terry"))
              '(("Adams, Douglas" "Mostly Harmless")
                ("Pratchett, Terry" "The Colour of Magic" "Pyramids")))