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

;; edges are directed as far as this is concerned
(define-structure vertex label edges)
(define-structure edge label out in)
(define-structure graph vertices edges)

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
     (map (lambda (label) (make-vertex label '())) (remove-duplicatese
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

(define (topological-sort-from-node graph nodes)
 ;; TODO This modifies nodes
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
  (cons best-solution best-cost)))

(define (tsp graph edge->weight) (tsp-f graph edge->weight +inf.0 >=))
(define (tsp-partial graph edge->weight best) (tsp-f graph edge->weight best >=))
(define (max-tsp graph edge->weight) (tsp-f graph edge->weight 0 <))
(define (max-tsp-partial graph edge->weight best) (tsp-f graph edge->weight best <))

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
        (let ((node (minimump (lambda (v) (hash-table-ref distances v)) unvisited)))
         (loop (removeq node unvisited) node)))))))

(define (for-each-b/d-fs f root graph bfs? #!key (duplicate-nodes? #t))
 ;; default is dfs
 ;; f :: new -> parent -> r; parent is #f for the root
 ;; duplicate-nodes? never calls f with a node twice
 ;;   useful in undirected graphs
 (let loop ((explored '()) (unexplored (list (cons root #f))))
  (unless (null? unexplored)
   (display (map vertex-label explored))(newline)
   (display (map vertex-label (map car unexplored)))(newline)
   (let* ((p (car unexplored)))
    (f (car p) (cdr p))
    (loop (cons (car p) explored)
          (let* ((new (map (lambda (e) (cons (edge-in e) (car p)))
                           (vertex-out-edges (car p))))
                 (new (if duplicate-nodes?
                          new
                          (remove (lambda (a) (memq (car a) explored)) new)))
                 (merged (if bfs?
                             (append (cdr unexplored) new)
                             (append new (cdr unexplored)))))
           (if duplicate-nodes?
               merged
               (remove-duplicates (lambda (a b) (eq? (car a) (car b))) merged))))))))

(define (for-each-bfs f root graph #!rest args)
 (apply for-each-b/d-fs f root graph #t args))

(define (for-each-dfs f root graph #!rest args)
 (apply for-each-b/d-fs f root graph #f args))

(define (fold-dfs f i root graph #!rest args)
 (let ((l i))
  (apply for-each-dfs (lambda (vertex) (set! l (f i vertex)))
         root graph args)
  l))

(define (fold-bfs f i root graph #!rest args)
 (let ((l i))
  (apply for-each-bfs (lambda (vertex) (set! l (f i vertex))) 
         root graph args)
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

(define (contract-edge-between! graph v1 v2)
 (let ((v (make-vertex (gensym) '()))
       (edges (remove-duplicatesq
               (append (vertex-edges v1) (vertex-edges v2)))))
  (for-each (lambda (e)
             (delete-edge! e)
             (set-graph-edges! graph (removeq e (graph-edges graph))))
   edges)
  (set-graph-vertices! graph (cons v (removeq v1 (removeq v2 (graph-vertices graph)))))
  (for-each (lambda (e)
             (let ((out (if (or (eq? (edge-out e) v1)
                               (eq? (edge-out e) v2))
                            v
                            (edge-out e)))
                   (in (if (or (eq? (edge-in e) v1)
                              (eq? (edge-in e) v2))
                           v
                           (edge-in e))))
              (unless (or (eq? in out) (edge-between? out in))
               (let ((new-edge (make-edge (edge-label e) out in)))
                (add-edge! new-edge)
                (set-graph-edges! graph (cons new-edge (graph-edges graph)))))))
   edges)
  v))

(define (show-graph graph #!key (edge->label #f) (vertex->label #t))
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
)

