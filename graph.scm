(module graph * 
(import chicken scheme extras srfi-1)
(use srfi-1 srfi-18 srfi-69 miscmacros define-structure traversal)
(use nondeterminism object-graph files)

;; TODO this doesn't belong here
(define (snoc l x) (cons x l))

(define (ht) (make-hash-table))
(define (@ h k) (hash-table-ref h k))
(define (? h k) (hash-table-exists? h k))
(define (! h k x) (hash-table-set! h k x))

(define (register-node1 object)
 (let ((node (lookup-object-node object)))
  (if node
      node
      (register-node object))))

(define (show-object-graph/dot)
 (define (with-temporary-file _ f)
  (let* ((name (create-temporary-file))
         (result (f name)))
   (delete-file* name)
   result))
 (with-temporary-file
  "/tmp/dot.dot"
  (lambda (dot-file)
   (with-temporary-file
    "/tmp/dot.png"
    (lambda (png-file)
     (call-with-output-file dot-file render-graph/dot)
     (system (format #f "dot -Tpng ~a > ~a" dot-file png-file))
     (system (format #f "feh --force-aliasing ~a" png-file)))))))

;; http://en.wikipedia.org/wiki/Karger%27s_algorithm
;; http://en.wikipedia.org/wiki/Book:Graph_Algorithms
;; http://en.wikipedia.org/wiki/Book:Graph_Algorithms

;; edges are directed as far as this is concerned
(define-structure vertex label edges)
(define-structure edge label out in)
(define-structure graph vertices edges)

;; http://en.wikipedia.org/wiki/Graph_algorithms#Graph_algorithms

;;; what I want:
;; alist->graph vs alist->digraph
;; (mst digraph edge->weight) and (mst graph edge->weight)
;; should be extensible
;; different kind of representations: adjacency list, interleaved, dense and sparse matrix

(define-record-printer
 (edge obj port)
 (display (list (edge-label obj) (vertex-label (edge-out obj)) (vertex-label (edge-in obj))) port))

(define-record-printer
 (graph obj port)
 (pp (graph->alist obj) port))

(define (vertex-out-edges v) (remove-if-not (lambda (e) (eq? (edge-out e) v)) (vertex-edges v)))
(define (vertex-in-edges v) (remove-if-not (lambda (e) (eq? (edge-in e) v)) (vertex-edges v)))
(define (vertex-add-edge! v e) (set-vertex-edges! v (cons e (vertex-edges v))))
(define (vertex-delete-edge! v e) (set-vertex-edges! v (removeq e (vertex-edges v))))
(define (edge-between? v1 v2) (find-if (lambda (e) (eq? (edge-in e) v2)) (vertex-out-edges v1)))
(define (adjacent-vertices? v1 v2) (find-if (lambda (e) (eq? (edge-in e) v2)) (vertex-out-edges v1)))
(define (add-edge! e) (vertex-add-edge! (edge-in e) e) (vertex-add-edge! (edge-out e) e) e)
(define (delete-edge! e) (vertex-delete-edge! (edge-in e) e) (vertex-delete-edge! (edge-out e) e))
(define (vertex-incoming-edges? v) (not (null? (vertex-in-edges v))))
(define (copy-vertex v) (make-vertex (vertex-label v) (vertex-edges v)))
(define (copy-edge v) (make-edge (edge-label v) (edge-out v) (edge-in v)))

(define (alist->digraph alist)
 (let*
   ((vertices
     (map (lambda (label) (make-vertex label '())) (remove-duplicates
                                               (append (map first alist) (map second alist)))))
    (edges (map (lambda (l)
                 (add-edge! (make-edge (if (> (length l) 2) (third l) #f)
                                       (find-if (lambda (v) (equal? (first l) (vertex-label v))) vertices)
                                       (find-if (lambda (v) (equal? (second l) (vertex-label v))) vertices))))
                alist)))
  (make-graph vertices edges)))

;; (alist->digraph (digraph->alist g)) = g
;;   only when no duplicate labels exist
(define (digraph->alist graph)
 (map (lambda (e) (list (vertex-label (edge-out e)) (vertex-label (edge-in e)) (edge-label e)))
      (graph-edges graph)))

(define (copy-graph graph)
 (let* ((vertex-alist (map (lambda (v) (cons v (make-vertex (vertex-label v) '()))) (graph-vertices graph)))
        (edges (map (lambda (e)
                     (add-edge! (make-edge (edge-label e)
                                           (cdr (assoc (edge-in e) vertex-alist))
                                           (cdr (assoc (edge-out e) vertex-alist)))))
                    (graph-edges graph))))
  (make-graph (map cdr vertex-alist) edges)))

;; the graphs will not share any nodes
(define (digraph->graph graph)
 (let* ((graph (copy-graph graph)) (edges '()))
  (for-each (lambda (e)
             (unless (edge-between? (edge-in e) (edge-out e))
              (set! edges (cons (add-edge! (make-edge (edge-label e) (edge-in e) (edge-out e)))
                                edges))))
   (graph-edges graph))
  (make-graph (graph-vertices graph) (append edges (graph-edges graph)))))

(define (mst g edge->weight)
 (let ((root (car (graph-vertices g))))
  (let loop ((vertices (list root))
             (edges (vertex-out-edges root))
             (mst '()))
   (if (= (length vertices) (length (graph-vertices g)))
       mst
       (let* ((edge (minimump edge-label edges))
              (vertex (edge-in edge)))
        (loop (cons vertex vertices)
              (append
               (remove-if (lambda (e) (memq (edge-in e) vertices)) (vertex-out-edges vertex))
               (remove-if (lambda (e) (eq? (edge-in e) vertex)) edges))
              (cons edge mst)))))))

;; should be 39
;; (mst
;;  (digraph->graph
;;   (alist->digraph
;;    '((a d 5) (a b 7)
;;      (b c 8) (b e 7) (b d 9)
;;      (c e 5)
;;      (d e 15) (d f 6)
;;      (e g 9) (e f 8)
;;      (g f 11))))
;;  edge-label)

(define (topological-sort-from-node graph nodes)
 ;; TODO This modifies nodes!!
 ;; node must have no incoming arcs
 (let ((graph (copy-graph graph)))
  (let loop ((s nodes) (l '()))
   (if (null? s)
       (reverse l)
       (let ((edges (vertex-out-edges (car s))))
        (for-each delete-edge! edges)
        (loop (append (remove vertex-incoming-edges? (map edge-in edges)) (cdr s))
              (cons (car s) l)))))))

(define (graph-topological-sort graph)
 (topological-sort-from-node
  graph
  (remove vertex-incoming-edges? (graph-vertices graph))))

;; (topological-sort
;;  (alist->digraph
;;   '((7 11 #f) (7 8 #f) (5 11 #f) (3 8 #f) (3 10 #f)
;;     (11 2 #f) (11 9 #f) (11 10 #f) (8 9 #f))))

(define (tsp-f graph edge->weight zero cmp)
 ;; TODO must have positive weights
 (let ((best-solution #f)
       (best-cost zero))
  (for-effects
    (let loop ((vertices (cdr (graph-vertices graph)))
               (cost 0)
               (tour (list (car (graph-vertices graph)))))
     (when (cmp cost best-cost) (fail))
     (if (null? vertices)
         (let ((e (find-if (lambda (e) (eq? (edge-in e) (car tour)))
                           (vertex-out-edges (last tour)))))
          (unless (and e (not (cmp (+ (edge-label e) cost) best-cost))) (fail))
          (set! best-cost (+ (edge-label e) cost))
          (set! best-solution tour))
         (let ((e (a-member-of (vertex-edges (car tour)))))
          (when (memq (edge-out e) tour) (fail))
          (loop (removeq (edge-out e) vertices)
                (+ (edge->weight e) cost)
                (cons (edge-out e) tour))))))
  (list best-solution best-cost)))

(define (tsp graph edge->weight) (tsp-f graph edge->weight +inf.0 >=))
(define (tsp-partial graph edge->weight best) (tsp-f graph edge->weight best >=))
(define (max-tsp graph edge->weight) (tsp-f graph edge->weight 0 <))
(define (max-tsp-partial graph edge->weight best) (tsp-f graph edge->weight best <))

;; ;; 14
;; (map vertex-label
;;      (car
;;       (tsp
;;        (digraph->graph
;;         (alist->digraph
;;          '((a b 2) (a e 2) (a d 1) (a f 2)
;;            (b c 4) (b d 5) (b e 1)
;;            (c e 2) (c f 3) (d e 4) (e f 2))))
;;        edge-label)))

;; ;; 54
;; (map vertex-label
;;      (car
;;       (tsp
;;        (digraph->graph
;;         (alist->digraph
;;          '((a b 10) (a c 15) (a s 10) (a e 14) (a m 11)
;;            (b m 15) (b s 9) (b e 13) (b c 8)
;;            (c s 10) (c m 16) (c e 11)
;;            (e s 6) (e m 9)
;;            (s m 15))))
;;        edge-label)))

(define (dijkstras-algorithm graph node edge->weight)
 (let ((distances (alist->hash-table (map (lambda (v) (cons v +inf.0)) (graph-vertices graph)))))
  (hash-table-set! distances node 0)
  (let loop ((unvisited (removeq node (graph-vertices graph))) (node node))
   (if (null? unvisited)
       (hash-table->alist distances)
       (let ((current-distance (hash-table-ref distances node)))
        (for-each (lambda (e)
                   (when (< (+ current-distance (edge->weight e)) (hash-table-ref distances (edge-in e)))
                    (hash-table-set! distances (edge-in e) (+ current-distance (edge->weight e)))))
         (intersectionp (lambda (a b) (eq? (edge-in a) b)) (vertex-out-edges node) unvisited))
        (let ((node (minimump unvisited (lambda (v) (hash-table-ref distances v)))))
         (loop (removeq node unvisited) node)))))))

;; ((5 . 20) (4 . 20) (6 . 11) (1 . 0) (3 . 9) (2 . 7))
;; (pp (map (lambda (a) (cons (vertex-label (car a)) (cdr a)))
;;          (let ((graph (digraph->graph
;;                        (alist->digraph
;;                         '((1 2 7) (1 3 9) (1 6 14)
;;                           (2 3 10) (2 4 15)
;;                           (3 6 2) (3 4 11)
;;                           (4 5 6) (5 6 9))))))
;;           (dijkstras-algorithm 
;;            graph
;;            (car (graph-vertices graph))
;;            edge-label))))

(define (for-each-dfs f root graph)
 (let loop ((explored '()) (unexplored (list root)))
  (unless (null? unexplored)
   (let ((v (car unexplored)))
    (f v)
    (loop (cons v explored)
          (remove-duplicates
           (append
            (remove-if (lambda (a) (memq a explored))
                       (map edge-in (vertex-out-edges v)))
            (cdr unexplored))))))))

(define (for-each-bfs f root graph)
 (let loop ((explored '()) (unexplored (list root)))
  (unless (null? unexplored)
   (let ((v (car unexplored)))
    (f v)
    (loop (cons v explored)
          (remove-duplicates
           (append
            (cdr unexplored)
            (remove-if (lambda (a) (memq a explored))
                       (map edge-in (vertex-out-edges v))))))))))

(define (fold-dfs f i root graph)
 (let ((l i))
  (for-each-dfs (lambda (vertex) (set! l (f i vertex))) root graph)
  l))

(define (fold-bfs f i root graph)
 (let ((l i))
  (for-each-bfs (lambda (vertex) (set! l (f i vertex))) root graph)
  l))

;; http://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
(define (strongly-connected-components graph)
 (let ((index 0) (indices (ht)) (lowlinks (ht)) (S '()) (components '()))
  (define (go v)
   (! indices v index)
   (! lowlinks v index)
   (inc! index)
   (push! v S)
   (for-each
     (lambda (e)
      (let ((w (edge-in e)))
       (cond ((not (? indices w))
              (go w)
              (! lowlinks v (min (@ lowlinks v) (@ lowlinks w))))
             ((member w S)
              (! lowlinks v (min (@ lowlinks v) (@ indices w)))))))
    (vertex-out-edges v))
   (when (= (@ lowlinks v) (@ indices v))
    (let loop ((scc '()))
     (if (null? S)
         scc
         (let ((w (pop! S)))
          (push! w scc)
          (if (eq? w v)
              (push! scc components)
              (loop scc)))))))
  (for-each (lambda (vertex) (unless (? indices vertex) (go vertex)))
   (graph-vertices graph))
  components))

;; http://en.wikipedia.org/wiki/File:Scc.png
;; ((a b e) (c d h) (f g))
;; (pp (map (lambda (scc) (map vertex-label scc))
;;          (let ((graph (alist->digraph
;;                        '((a b)
;;                          (b c) (b e) (b f)
;;                          (c d) (c g)
;;                          (d c) (d h)
;;                          (e a) (e f)
;;                          (f g)
;;                          (g f)
;;                          (h g) (h d)))))
;;           (strongly-connected-components graph))))

(define (number-vertices graph)
 (for-each-indexed
  (lambda (vertex n) (setp-vertex-label! vertex (lambda (l) (cons n l))))
  graph)
 graph)

(define (cdr-vertices graph)
 (for-each-indexed
  (lambda (vertex n) (setp-vertex-label! vertex cdr))
  graph)
 graph)

(define (show-graph graph #!key (edge->label #f) (vertex->label #f))
 (reset-graph)
 (for-each (lambda (edge)
            (let* ((n1 (register-node1 (edge-out edge)))
                   (n2 (register-node1 (edge-in edge)))
                   (e (register-edge n1 n2)))
             (when edge->label
              (set-label e
                         (if (procedure? edge->label)
                             (edge->label e)
                             (format #f "~a" (edge-label e)))))))
  (graph-edges graph))
 (when vertex->label
  (for-each (lambda (vertex)
             (set-label (register-node1 vertex)
                        (if (procedure? vertex->label)
                            (vertex->label vertex)
                            (format #f "~a" (vertex-label vertex)))))
   (graph-vertices graph)))
 (show-object-graph/dot))

;; ;; http://en.wikipedia.org/wiki/File:Scc.png
;; (show-graph 
;;  (alist->digraph
;;   '((a b)
;;     (b c) (b e) (b f)
;;     (c d) (c g)
;;     (d c) (d h)
;;     (e a) (e f)
;;     (f g)
;;     (g f)
;;     (h g) (h d)))
;;  vertex->label: #t)
)
