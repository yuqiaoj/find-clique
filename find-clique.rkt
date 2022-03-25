;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname find-clique) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; A Node is a Sym 
  
;; A Graph is one of: 
;;   empty 
;;   (cons (list v (list w_1 ... w_n)) g) where g is a Graph
(define (node g) (first g))
(define (edges g) (second g))

;; Requires: 
;;   v, w_1, ...., w_n are Nodes 
;;   v is the in-neighbour to w_1, ... , w_n in the Graph 
;;   v does not appear as an in-neighbour in g
;; Requires: there is no node u in G such that there is
;;   an edge from u to u. For any two nodes u and v 
;;   in G, there can be at most one edge from u to v.

;; An undirected graph (UGraph) is a graph G such 
;; that for any two nodes u and v in G, if there 
;; is an edge from u to v, there is also an edge from 
;; v to u.

(define (find-clique/faster k g) (find-clique k g))

;; (find-clique k g) consumes a Nat k and a UGraph g, and produces a list of k
;;    Nodes representing a clique of size k in g if one exists, or produces
;;    false if no such clique exists
;; Example:
(check-expect (find-clique/faster 3 vegetables) '(watermelon cucumber zucchini))

;; find-clique: Nat UGraph -> (anyof (listof Node) false)
(define (find-clique k g)
  (local
    [(define-struct result (clique nbrs visited))
     ;; A Result is a (make-result (list of Node) (list of Node) (list of Node))
     ;;   * clique represents a clique within g
     ;;   * nbrs represents prospective nodes that are neighbours of some node
     ;;     in clique, that may or may not be part of the clique
     ;;   * visited represents nodes that have already been processed, and
     ;;     cannot exist in a clique of size k within g

     ;; (find-nbrs g u) consumes a Graph g and a Node u and produces a list of
     ;;    all the out-neighbours of u 
     ;; find-nbrs: Graph Node -> (listof Node)
     ;; Requires: u exists in g
     (define (find-nbrs g u)
       (cond [(symbol=? u (node (first g)))
              (edges (first g))]
             [else (find-nbrs (rest g) u)]))

     ;; (intersection lst1 lst2) produces a list of all elements shared by lst1
     ;;    and lst2 (taken from Assignment 10)
     ;; intersection: (listof Any) (listof Any) -> (listof Any)
     (define (intersection lst1 lst2)
       (foldl (lambda (x rror) (cond [(member? x lst2) (cons x rror)]
                                     [else rror])) empty lst1))

     ;; (candidates g r) consumes a UGraph g and an accumulator r, and produces
     ;;    a Result like r, but whith all Nodes in g with k or more than k edges
     ;;    into (result-nbrs r), and all nodes with less than k edges into
     ;;    (result-visited r)
     ;; candidates: UGraph -> Result
     (define (candidates g r)
       (cond
         [(empty? g) r]
         [(>= (length (edges (first g))) (sub1 k))
          (candidates (rest g) (make-result
                                empty (cons (node (first g)) (result-nbrs r))
                                (result-visited r)))]
         [else
          (candidates (rest g) (make-result
                                empty (result-nbrs r)
                                (cons (node (first g)) (result-visited r))))]))

     ;; (bron-kerbosch r) consumes a Result r and produces a (listof Node)
     ;;    representing a clique of size k in g, or false if no such clique
     ;;    exists in g; r is used as an accumulator to keep track of the clique
     ;;    currently being investigated, any prospective neighbours of the
     ;;    clique, and all Nodes in g that have already been processed
     ;; bron-kerbosch: Result -> (anyof false (listof Node))
     (define (bron-kerbosch r)
       (local [(define clq (result-clique r))
               (define nbr (result-nbrs r))
               (define vis (result-visited r))]
         (cond
           [(>= (length clq) k) clq]
           [(empty? nbr) false]
           [else
            (local
              [(define rror
                 (bron-kerbosch
                  (make-result (cons (first nbr) clq)
                               (intersection nbr (find-nbrs g (first nbr)))
                               (intersection vis (find-nbrs g (first nbr))))))]
              (cond [(boolean? rror)
                     (bron-kerbosch
                      (make-result clq (rest nbr) (cons (first nbr) vis)))]
                    [else rror]))])))]

    (bron-kerbosch (candidates g (make-result empty empty empty)))))

;; Tests:
(check-expect (find-clique/faster 0 vegetables) empty)
(check-expect (find-clique/faster 0 empty) empty)
(check-expect (find-clique/faster 1 empty) false)
(check-expect (find-clique/faster 1 vegetables) '(zucchini))
(check-expect (find-clique/faster 2 vegetables) '(cucumber zucchini))
(check-expect (find-clique/faster 3 vegetables) '(watermelon cucumber zucchini))
(check-expect (find-clique/faster 4 vegetables) '(rapini tatsoi kale turnip))
(check-expect (find-clique/faster 5 vegetables) false)
(check-expect (length (find-clique/faster 10 (make-big-clique 10))) 10)


;; (number->symbol n) converts a Num n into a Sym
;; number->symbol: Num -> Sym
(define (number->symbol n)
  (string->symbol (number->string n)))

;; (make-big-clique n) creates a clique of size n
;; make-big-clique: Nat -> UGraph
(define (make-big-clique n)
  (build-list
   n (lambda (x)
       (list (number->symbol x)
             (append
              (build-list x (lambda (y) (number->symbol y)))
              (build-list (- n x) (lambda (y) (number->symbol (+ 1 y x)))))))))

;; (time (length (find-clique 1000 (make-big-clique 1000))))

(define vegetables
  '((broccoli (kale cauliflower))
    (cabbage (kale))
    (cantaloupe (watermelon squash))
    (cauliflower (broccoli))
    (cucumber (kohlrabi watermelon zucchini))
    (eggplant (pepper potato))
    (kale (broccoli cabbage rapini tatsoi turnip))
    (kohlrabi (turnip cucumber))
    (mizuna (tatsoi))
    (pepper (tomato potato eggplant))
    (potato (eggplant pepper))
    (pumpkin (squash))
    (rapini (turnip tatsoi kale))
    (squash (cantaloupe pumpkin))
    (tatsoi (turnip mizuna kale rapini))
    (tomato (pepper))
    (turnip (kale kohlrabi rapini tatsoi))
    (watermelon (cucumber cantaloupe zucchini))
    (zucchini (watermelon cucumber))))

#|
Bibliography
------------

Vishwas, S. (2019, August 18). Using Bron Kerbosch algorithm to find maximal
    cliques in O(3^(N/3)). Retrieved from https://iq.opengenus.org/bron-kerbosch
    -algorithm/

Wikipedia contributors. (2021, November 12). Bron–Kerbosch algorithm. In
     Wikipedia, The Free Encyclopedia. Retrieved from https://en.wikipedia.org/w
     /index.php?title=Bron%E2%80%93Kerbosch_algorithm&oldid=1054949233

Wikipedia contributors. (2021, October 11). Clique problem. In Wikipedia, The
    Free Encyclopedia. Retrieved from https://en.wikipedia.org/w/index.php?title
    =Clique_problem&oldid=1049302087

|#