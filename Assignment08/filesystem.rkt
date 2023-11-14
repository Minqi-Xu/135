;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname filesystem) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment08, Problem 3

;; Put your header here

(require "filesystem-lib.rkt")

;; A Filesystem Object (FSObject) is one of:
;; * File
;; * Dir

(define-struct file (name size owner))
;; A File is a (make-file Str Nat Sym)

(define-struct dir (name owner contents))
;; A Dir is a (make-dir Str Sym (listof FSObject))


(define example-fs
  (make-dir "root" 'root
            (list
             (make-dir "Dan" 'dan
                       (list (make-file "log.txt" 768 'dan)
                             (make-file "profile.jpg" 60370 'dan)
                             (make-dir "music" 'dan
                                       (list (make-file "Thelonius Monk.mp3" 92227584 'dan)))))
             (make-dir "Slides" 'teaching
                       (list (make-dir "cs135" 'teaching
                                       (list (make-file "01-intro.pdf" 72244 'teaching)
                                             (make-file "11-trees.pdf" 123124 'teaching)
                                             (make-dir "system" 'root
                                                       (list (make-dir "logs" 'teaching empty)))))))
             (make-file "vmlinuz" 30 'root))))

;; A
(define (fs-template fs)
  (cond[(file? fs) (... (file-name fs) ...
                        (file-size fs) ...
                        (file-owner fs) ...)]
       [(dir? fs) (... (dir-name fs) ...
                       (dir-owner fs) ...
                       (dir-contents fs) ...)]))

(define (list-fs-template lst)
  (cond[(empty? lst) ...]
       [else (... (first lst) ...
                  (rest lst) ...)]))

;; B
;; (fsobject-name fs) consumes an FSObject and produces its name
;; fsobject: FSObject -> Str

(check-expect (fsobject-name (make-file "ABC" 400 'Dan)) "ABC")

(define (fsobject-name fs)
  (cond[(file? fs) (file-name fs)]
       [(dir? fs) (dir-name fs)]))

(check-expect (fsobject-name (make-dir "BCA" 'Dan
                                       (list (make-file "ABC" 400 'Dan)))) "BCA")

;; C
;; (count-files fs) consumes an FSObject and produces the total number of Files
;;  in the FSObject
;; count-files: FSObject -> Nat

(check-expect (count-files (make-file "README" 16 'me)) 1)

(define (count-files fs)
  (local[;(if-dir lst) consumes a list of FSObject and produces the total number of Files in
         ;; the list
         ;; if-dir: (listof FSObject) -> Nat
         (define (if-dir lst)
           (cond[(empty? lst) 0]
                [(dir? (first lst)) (+ (if-dir (dir-contents (first lst)))
                                       (if-dir (rest lst)))]
                [else (add1 (if-dir (rest lst)))]))]
    (cond[(dir? fs) (if-dir (dir-contents fs))]
         [else 1])))

(check-expect (count-files example-fs) 6)

;; D
;; (file-exists? fs los) consumes an FSObject and a (listof Str), and produces
;;  true if the list of strings represents a path to a File in the tree
;; file-exists?: FSObject (listof Str) -> Bool

(check-expect (file-exists? example-fs
                            (list "root" "Slides" "cs135" "11-trees.pdf")) true)

(define (file-exists? fs lst)
  (local[; (search-branch contents los) consumes a listof FSObject and a listof
         ;;  string and produces true if the list of strings represents a path
         ;;  to a File in the tree
         (define (search-branch contents los)
           (cond[(empty? contents) false]
                [(empty? los) false]
                [(and (file? (first contents))
                      (not (empty? (rest los)))) false]
                [(and (dir? (first contents))
                      (string=? (dir-name (first contents)) (first los)))
                 (file-exists? (first contents) los)]
                [(and (file? (first contents))
                 (string=? (file-name (first contents)) (first los)))
                 true]
                [else (search-branch (rest contents) los)]))]
    (cond[(empty? lst) false]
         [(file? fs) (string=? (file-name fs) (first lst))]
         [(string=? (dir-name fs) (first lst))
          (search-branch (dir-contents fs) (rest lst))]
         [else false])))

(check-expect (file-exists? example-fs (list "root" "Dan")) false)
(check-expect (file-exists? example-fs (list "readme.txt")) false)
(check-expect (file-exists? (make-file "ABC" 400 'Dan) (list "ABC")) true)
(check-expect (file-exists? example-fs
                            (list "root" "Slide" "cs135" "11-trees.pdf")) false)
(check-expect (file-exists? example-fs
                            (list "root" "Slides" "cs1356" "11-trees.pdf")) false)
(check-expect (file-exists? example-fs empty) false)

;; E
;; (remove-empty fs) consumes an FSObject, and produces a new tree with all
;;  empty directories removed. Note that there is no cascade.
;; remove-empty: FSObject -> FSObject

(check-expect (remove-empty example-fs)
              (make-dir "root" 'root
                (list
                  (make-dir "Dan" 'dan
                   (list (make-file "log.txt" 768 'dan)
                         (make-file "profile.jpg" 60370 'dan)
                         (make-dir "music" 'dan
                          (list (make-file "Thelonius Monk.mp3" 92227584 'dan)))))
                         (make-dir "Slides" 'teaching
                          (list (make-dir "cs135" 'teaching
                           (list (make-file "01-intro.pdf" 72244 'teaching)
                            (make-file "11-trees.pdf" 123124 'teaching)
                           (make-dir "system" 'root empty)))))
                         (make-file "vmlinuz" 30 'root))))

(define (remove-empty fs)
  (local[; (remove-empty/2 content) consumes a listof FSObject and produces
         ;;  a new tree with all empty directories removed.
         ;; remove-empty/2: (listof FSObject) -> FSObject
         (define (remove-empty/2 content)
         (cond[(empty? content) empty]
              [(and (dir? (first content))
                    (empty? (dir-contents (first content))))
               (remove-empty/2 (rest content))]
              [(dir? (first content))
               (cons (make-dir (dir-name (first content))
                               (dir-owner (first content))
                               (remove-empty/2 (dir-contents (first content))))
                     (remove-empty/2 (rest content)))]
              [else (cons (first content)
                           (remove-empty/2 (rest content)))]))]
    (cond[(file? fs) fs]
         [else (make-dir (dir-name fs)
                         (dir-owner fs)
                         (remove-empty/2 (dir-contents fs)))])))

(check-expect (remove-empty (make-file "ABC" 400 'Dan))
              (make-file "ABC" 400 'Dan))
    
;; F
;; (disk-hog fs) consumes an FSObject, and produces the Sym that owns the
;;  largest File. If the tree contains no files, the function produces false.
;; disk-hog: FSObject -> (Anyof Sym false)

(check-expect (disk-hog example-fs) 'dan)

(define (disk-hog fs)
  (local[;(disk-hog/acc content max-file) consumes a listof FSObject and a File
         ;; and produces the File with the maximum size in the listof FSObject
         ;; disk-hog/acc: (listof FSObject) File -> File
         (define (disk-hog/acc content max-file)
           (cond[(empty? content) max-file]
                [(and (file? (first content))
                      (> (file-size (first content))
                         (file-size max-file)))
                 (disk-hog/acc (rest content) (first content))]
                [(and (dir? (first content))
                      (> (file-size
                          (disk-hog/acc
                           (dir-contents (first content)) max-file))
                         (file-size
                          (disk-hog/acc (rest content) max-file))))
                      (disk-hog/acc (dir-contents (first content)) max-file)]
                [else (disk-hog/acc (rest content) max-file)]))]
    (cond[(file? fs) (file-owner fs)]
         [(= -1 (file-size (disk-hog/acc (dir-contents fs)
                                         (make-file "None" -1 'fff)))) false]
         [else (file-owner (disk-hog/acc (dir-contents fs)
                                         (make-file "None" -1 'fff)))])))

(check-expect (disk-hog (make-dir "secrets" 'cia empty)) false)
(check-expect (disk-hog (make-file "secrets" 1024 'Darius)) 'Darius)

;; G

;(define (owned-by fs name)
;  (local[(define (traverse contents name)
;           (cond[(contents)]))]))

;; H (bonus)
;; (remove-empty/cascade fs) consumes an FSObject, and produces a new tree with all
;;  empty directories removed.
;; remove-empty: FSObject -> FSObject

(check-expect (remove-empty/cascade (make-dir "root" 'root empty)) false)

(define (remove-empty/cascade fs)
  (local[(define fs-new (remove-empty fs))]
    (cond[(= (count-files fs) 0) false]
         [(equal? fs-new fs) fs]
         [else (remove-empty/cascade fs-new)])))

(check-expect (remove-empty/cascade
               (make-dir "root" 'root
                         (list (make-dir "a" 'root
                                         (list (make-dir "b" 'root empty)))
                               (make-file "c" 120 'root))))
              (make-dir "root" 'root (list (make-file "c" 120 'root))))