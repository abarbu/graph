(use graph)

(define (example-graph)
 (show-graph
  (alist->graph '((- g f 11)
                  (- e f 8)
                  (- e g 9)
                  (- d f 6)
                  (-> d e 15)
                  (- c e 5)
                  (-> b d 9)
                  (- b e 7)
                  (-> c b 8)
                  (- a b 7)
                  (- a d 5)))))

(define (mst-example)
 ;; total cost should be 39
 (mst
  (alist->undirected-graph
   '((a d 5) (a b 7)
     (b c 8) (b e 7) (b d 9)
     (c e 5)
     (d e 15) (d f 6)
     (e g 9) (e f 8)
     (g f 11)))
  edge-label))

(define (topological-sort-example)
 (digraph-topological-sort
  (alist->directed-graph
   '((7 11 #f) (7 8 #f) (5 11 #f) (3 8 #f) (3 10 #f)
     (11 2 #f) (11 9 #f) (11 10 #f) (8 9 #f)))))

(define (dijkistra-example)
 ;; ((5 . 20) (4 . 20) (6 . 11) (1 . 0) (3 . 9) (2 . 7))
 (map (lambda (a) (cons (vertex-label (car a)) (cdr a)))
      (let ((graph (alist->directed-graph
                    '((1 2 7) (1 3 9) (1 6 14)
                      (2 3 10) (2 4 15)
                      (3 6 2) (3 4 11)
                      (4 5 6) (5 6 9)))))
       (dijkstras-algorithm
        graph
        (car (graph-vertices graph))
        edge-label))))

(define (scc-example)
 ;; http://en.wikipedia.org/wiki/File:Scc.png
 ;; ((a b e) (c d h) (f g))
 (map (lambda (scc) (map vertex-label scc))
      (let ((graph (alist->directed-graph
                    '((a b)
                      (b c) (b e) (b f)
                      (c d) (c g)
                      (d c) (d h)
                      (e a) (e f)
                      (f g)
                      (g f)
                      (h g) (h d)))))
       (strongly-connected-components graph))))

(define (contract-edge-example)
 ;; will look like a v structure
 (let ((graph (alist->directed-graph '((a b) (c d)))))
  (contract-edge-between!
   graph
   (find (lambda (v) (equal? 'b (vertex-label v))) (graph-vertices graph))
   (find (lambda (v) (equal? 'd (vertex-label v))) (graph-vertices graph)))
  (show-graph graph vertex->label: #t)))

(define (show-graph-example)
 ;; ;; http://en.wikipedia.org/wiki/File:Scc.png
 (show-graph
  (alist->directed-graph
   '((a b)
     (b c) (b e) (b f)
     (c d) (c g)
     (d c) (d h)
     (e a) (e f)
     (f g)
     (g f)
     (h g) (h d)))
  vertex->label: #t))

(define (maximum-flow-example)
 ;; 12
 (pp (let ((graph
            (alist->directed-graph
             '((s a 5) (s b 10) (s d 4)
               (a c 1) (a t 3)
               (b c 7) (b e 7) (b d 3)
               (d e 6) (c t 5) (e t 4)))))
      (car (graph-maximum-flow
            graph
            ;; source
            (find-if (lambda (a) (eq? (vertex-label a) 's)) (graph-vertices graph))
            ;; sink
            (find-if (lambda (a) (eq? (vertex-label a) 't)) (graph-vertices graph))
            ;; capacity
            edge-label))))
 ;; 4
 (pp (let ((graph
            (alist->directed-graph
             '((1 2 8) (1 3 3)
               (2 4 3) (3 4 1)
               (2 5 1) (3 5 1)
               (4 6 2) (5 6 3)))))
      (car (graph-maximum-flow
            graph
            ;; source
            (find-if (lambda (a) (eq? (vertex-label a) '1)) (graph-vertices graph))
            ;; sink
            (find-if (lambda (a) (eq? (vertex-label a) '6)) (graph-vertices graph))
            ;; capacity
            edge-label))))
 ;; 5
 (pp (let ((graph
            (alist->directed-graph
             '((a d 3) (a b 3)
               (b c 4)
               (c a 3) (c d 1) (c e 2)
               (d e 2) (d f 6)
               (e b 1) (e g 1)
               (f g 9)))))
      (car (graph-maximum-flow
            graph
            ;; source
            (find-if (lambda (a) (eq? (vertex-label a) 'a)) (graph-vertices graph))
            ;; sink
            (find-if (lambda (a) (eq? (vertex-label a) 'g)) (graph-vertices graph))
            ;; capacity
            edge-label)))))

(define (graph-laplacian-matrix-example)
 ;; #(#(2  -1 -1  0  0  0)
 ;;   #(-1  3 -1  0 -1  0)
 ;;   #(-1 -1  3 -1  0  0)
 ;;   #(0  0  -1  2 -1  0)
 ;;   #(0 -1   0 -1  3 -1)
 ;;   #(0  0   0  0 -1  1))
 (pp (graph-laplacian-matrix
      (alist->undirected-graph
       ;; http://en.wikipedia.org/wiki/File:6n-graf.svg
       '((1 2) (1 5)
         (5 4) (5 2) (2 3)
         (3 4) (4 6))))))

(define (graph-complement-example)
 (show-graph (graph-complement
              (alist->directed-graph
               '((a b)
                 (b c) (b e) (b f)
                 (c d) (c g)
                 (d c) (d h)
                 (e a) (e f)
                 (f g)
                 (g f)
                 (h g) (h d))))))

(define (graph-clique-example)
 (list
  (let ((graph
         (alist->undirected-graph
          ;; http://en.wikipedia.org/wiki/File:6n-graf.svg
          '((1 2) (1 5)
            (5 4) (5 2) (2 3)
            (3 4) (4 6)))))
   ;; ((6 4) (4 3) (3 2) (4 5) (2 5 1))
   (map (lambda (l) (map vertex-label l)) (graph-maximal-cliques graph)))
  (let ((graph
         (alist->undirected-graph
          '((1 2) (1 5)
            (5 4) (5 2) (2 3)
            (3 4) (4 6) (5 3) (2 4)))))
   ;; ((6 4) (2 4 3 5) (2 5 1))
   (map (lambda (l) (map vertex-label l)) (graph-maximal-cliques graph)))))
