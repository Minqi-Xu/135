;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname travel-map) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Minqi Xu (20845758)
;; CS135 Fall 2019
;; Assignment10, Problem 1

;; A Node is a Sym
;; A Map is a (listof (list Node (listof Neighbour)))
;;   requires: Map is directed and acyclic
;; A Neighbour is a (list Node Nat) where the number indicates the
;;   travel time (in minutes) to the neighbour

(define southern-ontario
  '((Brantford ((Hamilton 30)))
    (Cambridge ((Brantford 30) (Hamilton 45) (London 70) (TO 80)))
    (Guelph ((Cambridge 30) (TO 80)))
    (Hamilton ())
    (London ((Brantford 70)))
    (KW ((Cambridge 30) (Guelph 35) (Stratford 40)))
    (Stratford ((London 50)))
    (TO ((Hamilton 60)))))

;; (travel-time G ori dest) consumes an origin and a destination in the form of
;;   two Sym as well as a Map. Then produces the travel time between origin and
;;   destination; if no path exists, it produces false.
;; travel-time: Map Sym Sym -> (anyof Nat false)
;;   requires: both origin and destination are on the map

(check-expect (travel-time southern-ontario 'Brantford 'London) false)

(define (travel-time G ori dest)
  (local[(define (neighbours G v)
           (cond[(symbol=? v (first (first G)))
                 (second (first G))]
                [else (neighbours (rest G) v)]))
         (define (find-route G a b)
           (cond[(symbol=? a b) (list b)]
                [else (local[(define rans
                               (find-route/list G (neighbours G a) b))]
                        (cond[(cons? rans) (cons a rans)]
                             [else false]))]))
         (define (find-route/list G lov b)
           (cond[(empty? lov) false]
                [else (local[(define fans (find-route G (first (first lov)) b))]
                        (cond[(cons? fans) fans]
                             [else (find-route/list G (rest lov) b)]))]))
         (define route (find-route G ori dest))
         (define (search-time los dest)
           (cond[(symbol=? dest (first (first los))) (second (first los))]
                [else (search-time (rest los) dest)]))
         (define (cal-time los ans)
           (cond[(empty? (rest los)) ans]
                [else (cal-time (rest los)
                                (+ ans
                                   (search-time (neighbours G (first los))
                                                (second los))))]))]
    (cond[(false? route) false]
         [else (cal-time route 0)])))

(check-expect (travel-time southern-ontario 'Guelph 'Hamilton) 90)
(check-expect (travel-time southern-ontario 'KW 'Hamilton) 90)

;; (all-paths ori dest G) consumes an origin and a destination in the form of two
;;   Sym as well as a Map. Then produces a list of all possible paths between
;;   origin and destination as a (listof (listof Sym)).
;; all-paths: Sym Sym Map -> (listof (listof Sym))
;;   requires:  both origin and destination are on the map

(check-expect (all-paths 'Stratford 'Guelph southern-ontario) empty)

(define (all-paths ori dest G)
  (local[(define (neighbours G v)
           (cond[(symbol=? v (first (first G)))
                 (second (first G))]
                [else (neighbours (rest G) v)]))
         (define (find-route/list lov)
           (cond[(empty? lov) empty]
                [else (local[(define route (all-path/acc
                                             (first (first lov))
                                             dest G empty))]
                        (foldl cons route (find-route/list (rest lov))))]))
         (define (all-path/acc ori dest G so-far)
           (cond[(symbol=? ori dest) (cons (list ori) so-far)]
                [else (foldr cons (map (lambda (x) (cons ori x))
                                       (find-route/list (neighbours G ori))) so-far)]))]
    (all-path/acc ori dest G empty)))

(check-expect (all-paths 'Guelph 'Hamilton southern-ontario)
              '((Guelph TO Hamilton)
               (Guelph Cambridge Hamilton)
               (Guelph Cambridge TO Hamilton)
               (Guelph Cambridge London Brantford Hamilton)
               (Guelph Cambridge Brantford Hamilton)))

;; (all-travel-times ori dest Map) consumes an origin and a destination in the
;;   form of two Sym as well as a Map. Then produces a list of all possible paths
;;   between origin and destination as well as their travel time.
;; all-travel-time: Sym Sym Map -> (listof (list Nat (listof Sym)))
;;   requires:  both origin and destination are on the map

(check-expect (all-travel-times 'Guelph 'Hamilton southern-ontario)
              '((140 Guelph TO Hamilton)
               (75 Guelph Cambridge Hamilton)
               (170 Guelph Cambridge TO Hamilton)
               (200 Guelph Cambridge London Brantford Hamilton)
               (90 Guelph Cambridge Brantford Hamilton)))
(check-expect (all-travel-times 'Stratford 'Guelph southern-ontario) empty)


(define (all-travel-times ori dest Map)
  (local[(define (neighbours G v)
           (cond[(symbol=? v (first (first G)))
                 (second (first G))]
                [else (neighbours (rest G) v)]))
         (define routes (all-paths ori dest Map))
         (define (search-time los dest)
           (cond[(symbol=? dest (first (first los))) (second (first los))]
                [else (search-time (rest los) dest)]))
         (define (cal-time los ans)
           (cond[(empty? (rest los)) ans]
                [else (cal-time (rest los)
                                (+ ans
                                   (search-time (neighbours Map (first los))
                                                (second los))))]))]
    (foldr (lambda (single-route ans) (cons (cons (cal-time single-route 0)
                                                  single-route)
                                            ans)) empty routes)))