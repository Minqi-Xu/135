;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname html) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment08, Problem 4

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag

;; a Tag is (cons Sym (listof HI))
;; (In other words, a Tag is a list where the first element is a symbol,
;;   and all other elements are HI values)

(define just-text "Hello, world!")
(define short-example '(p (h1 "Heading") "Text"))
(define html-example '(html (head (title "CS135"))
                            (body (h1 "Welcome")
                                  "More text...")))


;; A
;; (html->string hi) consumes an HI and produces the equivalent HTML text
;; html->string: HI -> Str

(check-expect (html->string "text") "text")

(define (html->string hi)
  (cond[(empty? hi) ""]
       [(string? hi) hi]
       [(symbol? (first hi))
        (string-append "<" (symbol->string (first hi)) ">"
                       (html->string (rest hi))
                       "</" (symbol->string (first hi)) ">")]
       [else (string-append (html->string (first hi))
                            (html->string (rest hi)))]))

(check-expect (html->string short-example)
              "<p><h1>Heading</h1>Text</p>")
(check-expect (html->string '(hr)) "<hr></hr>")

;; B
;; (remove-tag hi sym) consumes an HI and a Sym and removes all occurrences of
;;  that tag. When a tag is removed, its contents should be moved to the parent
;;  of the removed tag. If the root is removed, the function should return a
;;  list of its children.
;; remove-tag: HI Sym -> (Anyof (listof HI) HI)

(check-expect (remove-tag html-example 'b) html-example)

(define (remove-tag hi sym)
  (cond[(empty? hi) empty]
       [(and (symbol? (first hi))
             (symbol=? sym (first hi)))
        (rest hi)]
       [(and (list? (first hi))
             (symbol=? sym (first (first hi))))
        (cond[(empty? (rest (first hi))) (remove-tag (rest hi) sym)]
             [(empty? (rest (rest (first hi))))
              (cons (second (first hi)) (remove-tag (rest hi) sym))]
             [else (local;(combine-no-empty lst) consumes a listof (Anyof Str
                         ;;  Sym HI), and produces a listof (Anyof Str Sym HI)
                         ;;  which combined with the later elements without
                         ;;  empty
                         ;; combine-no-empty: (listof (Anyof Str Sym HI)) ->
                         ;;                   (listof (Anyof Str Sym HI))
                     [(define (combine-no-empty lst)
                        (cond[(empty? (rest lst))
                              (cons (first lst) (remove-tag (rest hi) sym))]
                             [else (cons (first lst)
                                         (combine-no-empty (rest lst)))]))]
                     (combine-no-empty (rest  (first hi))))])]
       [else (cons (first hi) (remove-tag (rest hi) sym))]))

(check-expect (remove-tag '(p "Hello, " (b "World") "!") 'b)
              '(p "Hello, " "World" "!"))
(check-expect (remove-tag '(p "Hello, " (b "World") "!") 'p)
              '("Hello, " (b "World") "!"))
(check-expect (remove-tag '(p "Hello, " (b) "!") 'b)
              '(p "Hello, " "!"))
(check-expect (remove-tag '(p "Hello, " (b "World" "Hi" "World") "!") 'b)
              '(p "Hello, " "World" "Hi" "World" "!"))

;; C
;; (bad-tags? hi) consumes an HI and produces true if it has broken at least one
;;  of the two rules.
;; bad-tags?: HI -> Bool

(check-expect (bad-tags? html-example) false)

(define (bad-tags? hi)
  (local[; (bad-tags?/acc hi parent name) consumes an HI, a Sym and a Sym. and
         ;;  produces true if it has broken at least one of the rules. false
         ;;  otherwise.
         ;; bad-tags?/acc: HI Sym Sym -> Bool
         (define (bad-tags?/acc hi parent name)
           (cond[(empty? hi) false]
                [(and (symbol? (first hi))
                      (symbol=? (first hi) 'hr)) true]
                [(and (symbol? (first hi))
                      (symbol=? (first hi) 'li))
                 (cond[(or (symbol=? parent 'ol)
                           (symbol=? parent 'ul))
                       (bad-tags?/acc (rest hi) parent name)]
                      [else true])]
                [(symbol? (first hi))
                 (bad-tags?/acc (rest hi) parent (first hi))]
                [(list? (first hi))
                 (or (bad-tags?/acc (first hi) name (first (first hi)))
                     (bad-tags?/acc (rest hi) parent name))]
                [else (bad-tags?/acc (rest hi) parent name)]))]
    (bad-tags?/acc hi 'none (first hi))))


(check-expect (bad-tags? '(body (hr "hello"))) true)
(check-expect (bad-tags? '(body (li "Q1") "text")) true)
(check-expect (bad-tags? '(body (ul "Q1") "text")) false)
(check-expect (bad-tags? '(ul "Q1" "text")) false)
(check-expect (bad-tags? '(body (ol "hello" (li "Q1")) "text")) false)