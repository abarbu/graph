(module graph *
(import chicken scheme extras srfi-1)
(use srfi-1 srfi-18 srfi-69 miscmacros define-structure traversal vector-lib list-utils)
(use nondeterminism object-graph files scheme2c-compatibility)

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
(define-structure directed-edge label out in)
(define-structure undirected-edge label u v)
(define-structure graph vertices edges)

(define (edge-label e)
 (cond ((directed-edge? e) (directed-edge-label e))
       ((undirected-edge? e) (undirected-edge-label e))
       (else (fuck-up))))

(define-record-printer
 (directed-edge obj port)
 (display (list '->
                (vertex-label (directed-edge-out obj))
                (vertex-label (directed-edge-in obj))
                (directed-edge-label obj))
          port))

(define-record-printer
 (undirected-edge obj port)
 (display (list '-
                (vertex-label (undirected-edge-u obj))
                (vertex-label (undirected-edge-v obj))
                (undirected-edge-label obj))
          port))

(define-record-printer
 (graph obj port)
 (display "(alist->graph '" port)
 (pp-without-newline (graph-edges obj)
  ;; (list (graph-vertices obj) (graph-edges obj))
  port)
 (display ")" port))

(define (assert-directed-graph graph)
 (unless (every directed-edge? (graph-edges graph))
  (error "Must be a directed graph")))
(define (assert-undirected-graph graph)
 (unless (every undirected-edge? (graph-edges graph))
  (error "Must be an undirected graph")))

(define (for-each-vertex f graph) (for-each f (graph-vertices graph)))
(define (for-each-indexed-vertex f graph) (for-each-indexed f (graph-vertices graph)))
(define (map-vertex f graph) (map f (graph-vertices graph)))
(define (map-indexed-vertex f graph) (map-indexed f (graph-vertices graph)))
(define (for-each-edge f graph) (for-each f (graph-edges graph)))
(define (for-each-indexed-edge f graph) (for-each-indexed f (graph-edges graph)))
(define (map-edge f graph) (map f (graph-edges graph)))
(define (map-indexed-edge f graph) (map-indexed f (graph-edges graph)))

;; Note that with undirected edges the out edges and in edges of a
;; vertex will not be disjoint
(define (vertex-out-edges v) (remove-if-not (lambda (e)
                                        (cond ((directed-edge? e) (eq? (directed-edge-out e) v))
                                              ((undirected-edge? e) (or (eq? (undirected-edge-u e) v)
                                                                       (eq? (undirected-edge-v e) v)))
                                              (else (fuck-up)))) (vertex-edges v)))
(define (vertex-in-edges v) (remove-if-not (lambda (e)
                                       (cond ((directed-edge? e) (eq? (directed-edge-in e) v))
                                             ((undirected-edge? e) (or (eq? (undirected-edge-u e) v)
                                                                      (eq? (undirected-edge-v e) v)))
                                             (else (fuck-up)))) (vertex-edges v)))
(define (vertex-add-edge! v e) (set-vertex-edges! v (cons e (vertex-edges v))))
(define (vertex-delete-edge! v e) (set-vertex-edges! v (removeq e (vertex-edges v))))
(define (edge-between? v1 v2) (find-if (lambda (e) (cond ((directed-edge? e) (eq? (directed-edge-in e) v2))
                                               ((undirected-edge? e)
                                                (or (eq? (undirected-edge-u e) v2)
                                                   (eq? (undirected-edge-v e) v2)))
                                               (else (fuck-up))))
                                  (vertex-out-edges v1)))
(define (directed-edge-between? v1 v2)
 (find-if (lambda (e) (if (directed-edge? e)
                     (eq? (directed-edge-in e) v2)
                     #f))
          (vertex-out-edges v1)))
(define (undirected-edge-between? v1 v2)
 (find-if (lambda (e) (if (undirected-edge? e)
                        (or (eq? (undirected-edge-u e) v2)
                           (eq? (undirected-edge-v e) v2))
                        #f))
          (vertex-out-edges v1)))
(define (add-edge! e)
 (cond ((directed-edge? e)
        (vertex-add-edge! (directed-edge-out e) e)
        (vertex-add-edge! (directed-edge-in e) e))
       ((undirected-edge? e)
        (vertex-add-edge! (undirected-edge-u e) e)
        (vertex-add-edge! (undirected-edge-v e) e))
       (else (fuck-up)))
 e)
(define (delete-edge! e)
 (cond ((directed-edge? e)
        (vertex-delete-edge! (directed-edge-in e) e)
        (vertex-delete-edge! (directed-edge-out e) e))
       ((undirected-edge? e)
        (vertex-delete-edge! (undirected-edge-u e) e)
        (vertex-delete-edge! (undirected-edge-v e) e))
       (else (fuck-up))))
(define (vertex-incoming-edges? v) (not (null? (vertex-in-edges v))))
(define (copy-vertex v) (make-vertex (vertex-label v) (vertex-edges v)))
(define (copy-edge e)
 (cond ((directed-edge? e)
        (make-directed-edge (directed-edge-label e) (directed-edge-out e) (directed-edge-in e)))
       ((undirected-edge? e)
        (make-undirected-edge (undirected-edge-label e) (undirected-edge-u e) (undirected-edge-v e)))
       (else (fuck-up))))
(define (vertex-neighbours v) (map (lambda (e)
                               (cond ((directed-edge? e) (directed-edge-in e))
                                     ((undirected-edge? e)
                                      (if (eq? (undirected-edge-u e) v)
                                          (undirected-edge-v e)
                                          (undirected-edge-u e)))
                                     (else (fuck-up))))
                              (vertex-out-edges v)))
(define (add-directed-edge-between! u v) (let ((e (make-directed-edge #f u v))) (add-edge! e) e))

(define (alist->graph alist)
 (let*
   ((vertices
     (map (lambda (label) (make-vertex label '())) (remove-duplicatese
                                               (append (map second alist) (map third alist)))))
    (edges (map (lambda (l)
                 (add-edge! ((case (first l)
                              ((->) make-directed-edge)
                              ((-) make-undirected-edge)
                              (else (fuck-up)))
                             (if (> (length l) 3) (fourth l) #f)
                             (find-if (lambda (v) (equal? (second l) (vertex-label v))) vertices)
                             (find-if (lambda (v) (equal? (third l) (vertex-label v))) vertices))))
                alist)))
  (make-graph vertices edges)))

(define (alist->directed-graph alist) (alist->graph (map (lambda (l) (cons '-> l)) alist)))
(define (alist->undirected-graph alist)   (alist->graph (map (lambda (l) (cons '-  l)) alist)))

;; (alist->graph (graph->alist g)) = g
;;   only when no duplicate labels exist
(define (graph->alist graph)
 (map (lambda (e)
       (cond ((directed-edge? e)
              (list '-> (vertex-label (directed-edge-out e)) (vertex-label (directed-edge-in e)) (directed-edge-label e)))
             ((undirected-edge? e)
              (list '- (vertex-label (undirected-edge-u e)) (vertex-label (undirected-edge-v e)) (undirected-edge-label e)))
             (else (fuck-up))))
      (graph-edges graph)))

(define (copy-graph graph)
 (let* ((vertex-alist (map (lambda (v) (cons v (make-vertex (vertex-label v) '()))) (graph-vertices graph)))
        (edges (map (lambda (e)
                     (add-edge!
                      (cond ((directed-edge? e)
                             (make-directed-edge
                              (directed-edge-label e)
                              (cdr (assoc (directed-edge-in e) vertex-alist))
                              (cdr (assoc (directed-edge-out e) vertex-alist))))
                            ((undirected-edge? e)
                             (make-undirected-edge
                              (undirected-edge-label e)
                              (cdr (assoc (undirected-edge-u e) vertex-alist))
                              (cdr (assoc (undirected-edge-v e) vertex-alist))))
                            (else (fuck-up)))))
                    (graph-edges graph))))
  (make-graph (map cdr vertex-alist) edges)))

;; the graphs will not share any nodes
;; TODO Needs a name
(define (directed-graph->graph graph)
 (let* ((graph (copy-graph graph)) (edges '()))
  (for-each (lambda (e)
             (unless (edge-between? (directed-edge-in e) (directed-edge-out e))
              (set! edges (cons (add-edge! (make-directed-edge (edge-label e) (directed-edge-in e) (directed-edge-out e)))
                                edges))))
   (graph-edges graph))
  (make-graph (graph-vertices graph) (append edges (graph-edges graph)))))

;; the graphs will not share any nodes
(define (directed-graph->undirected-graph graph)
 (let* ((graph (copy-graph graph)) (edges '()))
  (for-each (lambda (e)
             (when (directed-edge? e)
              (unless (undirected-edge-between? (directed-edge-in e) (directed-edge-out e))
               (set! edges (cons (add-edge! (make-undirected-edge (edge-label e) (directed-edge-in e) (directed-edge-out e)))
                                 edges)))
              (delete-edge! e)))
   (graph-edges graph))
  (make-graph (graph-vertices graph) (remove directed-edge? (append edges (graph-edges graph))))))

;; these take a vertex in order to give you the opposite edge of an
;; undirected edge
(define (edge-opposite-in edge vertex)
 (cond ((directed-edge? edge) (directed-edge-in edge))
       ((undirected-edge? edge)
        (if (eq? (undirected-edge-u edge) vertex)
            (undirected-edge-v edge)
            (undirected-edge-u edge)))
       (else (fuck-up))))
(define (edge-opposite-out edge vertex)
 (cond ((directed-edge? edge) (directed-edge-out edge))
       ((undirected-edge? edge)
        (if (eq? (undirected-edge-u edge) vertex)
            (undirected-edge-v edge)
            (undirected-edge-u edge)))
       (else (fuck-up))))

(define (same-edge-make e)
 (cond ((directed-edge? e) make-directed-edge)
       ((undirected-edge? e) make-undirected-edge)
       (else (fuck-up))))
(define (edge-out/u e)
 (cond ((directed-edge? e) (directed-edge-out e))
       ((undirected-edge? e) (undirected-edge-u e))
       (else (fuck-up))))
(define (edge-in/v e)
 (cond ((directed-edge? e) (directed-edge-in e))
       ((undirected-edge? e) (undirected-edge-v e))
       (else (fuck-up))))

(define (mst g edge->weight)
 (let ((root (car (graph-vertices g))))
  (let loop ((vertices (list root))
             (vertices&edges (map (lambda (edge) (cons (edge-opposite-in edge root) edge))
                                  (vertex-out-edges root)))
             (mst '()))
   (if (= (length vertices) (length (graph-vertices g)))
       mst
       (let* ((vertex&edge (minimum (lambda (v&e) (edge->weight (cdr v&e))) vertices&edges))
              (vertex (car vertex&edge)))
        (loop (cons vertex vertices)
              (append
               (remove-if (lambda (v&e) (member (car v&e) vertices))
                          (map (lambda (e) (cons (edge-opposite-in e vertex) e))
                               (vertex-out-edges vertex)))
               (remove-if (lambda (v&e) (eq? (car v&e) vertex)) vertices&edges))
              (cons (cdr vertex&edge) mst)))))))

(define (digraph-topological-sort-from-node graph roots)
 (assert-directed-graph graph)
 ;; TODO This modifies nodes
 ;; all nodes in roots must have no incoming arcs
 (let ((graph (copy-graph graph)))
  (let loop ((s roots) (l '()))
   (if (null? s)
       (reverse l)
       (let ((edges (vertex-out-edges (car s))))
        (for-each delete-edge! edges)
        (loop (append (remove vertex-incoming-edges? (map directed-edge-in edges)) (cdr s))
              (cons (car s) l)))))))

(define (digraph-topological-sort graph)
 ;; some vertex must have no incoming edges
 (let ((roots (remove vertex-incoming-edges? (graph-vertices graph))))
  (if (null? roots)
      #f
      (digraph-topological-sort-from-node
       graph
       roots))))

(define (dijkstras-algorithm graph node edge->weight)
 (assert-directed-graph graph)
 (let ((distances (alist->hash-table (map (lambda (v) (cons v +inf.0)) (graph-vertices graph)))))
  (hash-table-set! distances node 0)
  (let loop ((unvisited (removeq node (graph-vertices graph))) (node node))
   (if (null? unvisited)
       (hash-table->alist distances)
       (let ((current-distance (hash-table-ref distances node)))
        (for-each (lambda (e)
                   (when (< (+ current-distance (edge->weight e)) (hash-table-ref distances (directed-edge-in e)))
                    (hash-table-set! distances (directed-edge-in e) (+ current-distance (edge->weight e)))))
         (set-intersection (lambda (a b) (eq? (directed-edge-in a) b)) (vertex-out-edges node) unvisited))
        (let ((node (minimum (lambda (v) (hash-table-ref distances v)) unvisited)))
         (loop (removeq node unvisited) node)))))))

;; Floyd-warhsall all points shortest path (currently just the weights)
;; Constructs two |V|x|V| vectors and a hash-table
;; See http://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
(define (floyd-warshall-algorithm graph edge->weight)
 (assert-directed-graph graph)
 (letrec ((vertex-count (length (graph-vertices graph)))
          (vertex-map
           (alist->hash-table
            (zip-alist
             (map (lambda (f) (vertex-label f)) (graph-vertices graph))
             (unfold (lambda (x) (>= x vertex-count)) (lambda (x) x) (lambda (x) (+ x 1)) 0))))
          (vertex-reverse-map
           (alist->hash-table
            (zip-alist
             (unfold (lambda (x) (>= x vertex-count)) (lambda (x) x) (lambda (x) (+ x 1)) 0)
             (map (lambda (f) (vertex-label f)) (graph-vertices graph))))))
  (let ((distances
         (vector-unfold (lambda (i)
                         (cond
                          ((eq? (quotient i vertex-count) (modulo i vertex-count)) 0)
                          (else +inf.0)))
                        (* vertex-count vertex-count)))
        (next (vector-unfold (lambda (i) -1) (* vertex-count vertex-count))))
   (map (lambda (e)
         (vector-set!
          distances (+ (hash-table-ref vertex-map (vertex-label (directed-edge-in e)))
                       (* vertex-count (hash-table-ref vertex-map (vertex-label (directed-edge-out e)))))
          (edge->weight e))
         (vector-set!
          next (+ (hash-table-ref vertex-map (vertex-label (directed-edge-in e)))
                  (* vertex-count (hash-table-ref vertex-map (vertex-label (directed-edge-out e)))))
          (hash-table-ref vertex-map (vertex-label (directed-edge-in e)))))
        (graph-edges graph))
   (let loop ((k 0))
    (if (= k vertex-count)
        distances
        (begin
         (let loop ((i 0))
          (if (= i vertex-count)
              distances
              (begin
               (let loop ((j 0))
                (if (= j vertex-count)
                    distances
                    (let ((new-path-cost (+ (vector-ref distances (+ i (* vertex-count k)))
                                            (vector-ref distances (+ k (* vertex-count j))))))
                     (cond ((< new-path-cost
                               (vector-ref distances (+ i (* vertex-count j))))
                            (vector-set! next (+ i (* vertex-count j)) k)
                            (vector-set! distances (+ i (* vertex-count j)) new-path-cost)))
                     (loop (+ j 1)))))
               (loop (+ i 1)))))
         (loop (+ k 1)))))
   (list distances next vertex-map vertex-reverse-map))))

(define (floyd-warshall-extract-path start dest floydwarshall-info)
 (let*
   ((distance (car floydwarshall-info))
    (next (cadr floydwarshall-info))
    (vertex-map (caddr floydwarshall-info))
    (vertex-reverse-map (cadddr floydwarshall-info))
    (vertex-count (hash-table-size vertex-map))
    (i (hash-table-ref vertex-map start))
    (j (hash-table-ref vertex-map dest)))
  (if (= (vector-ref distance (+ j (* vertex-count i)))  +inf.0)
      #f
      (letrec
        ((find-path (lambda (x)
                     (if (= x j)
                         (cons x '())
                         (cons x (find-path (vector-ref next (+ j (* vertex-count x)))))))))
       (map (lambda (x) (hash-table-ref vertex-reverse-map x)) (find-path i))))))

(define (for-each-b/d-fs f root graph bfs? #!key (duplicate-nodes? #t))
 (assert-directed-graph graph)
 ;; default is dfs
 ;; f :: new -> parent -> r; parent is #f for the root
 ;; duplicate-nodes? never calls f with a node twice
 ;;   useful in undirected graphs
 (let loop ((explored '()) (unexplored (list (cons root #f))))
  (unless (null? unexplored)
   (let* ((p (car unexplored)))
    (f (car p) (cdr p))
    (loop (cons (car p) explored)
          (let* ((new (map (lambda (e) (cons (directed-edge-in e) (car p)))
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
      (let ((w (edge-opposite-in e v)))
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

(define (connect-graph! graph)
 (set-graph-edges!
  graph
  (append (graph-edges graph)
          (let ((l (map first (strongly-connected-components graph))))
           (if (< (length l) 2)
               l
               (map (lambda (a b) (add-directed-edge-between! a b)) (cdr l) l)))))
 graph)

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
             (let ((out (if (or (eq? (edge-out/u e) v1)
                               (eq? (edge-out/u e) v2))
                            v
                            (edge-out/u e)))
                   (in (if (or (eq? (edge-in/v e) v1)
                              (eq? (edge-in/v e) v2))
                           v
                           (edge-in/v e))))
              (unless (or (eq? in out) (edge-between? out in))
               (let ((new-edge ((same-edge-make e) (edge-label e) out in)))
                (add-edge! new-edge)
                (set-graph-edges! graph (cons new-edge (graph-edges graph)))))))
   edges)
  v))

(define (show-graph graph #!key (edge->label #f) (vertex->label #t))
 (reset-graph)
 (new-property "arrowhead" 'string #f)
 (for-each (lambda (edge)
            (let* ((n1 (register-node1 (edge-out/u edge)))
                   (n2 (register-node1 (edge-in/v edge)))
                   (e (register-edge n1 n2)))
             (when edge->label
              (set-label e
                         (if (procedure? edge->label)
                             (edge->label edge)
                             (format #f "~a" (edge-label edge)))))
             (when (undirected-edge? edge)
              (set-edge-property (lookup-property "arrowhead") e "none"))))
  (graph-edges graph))
 (when vertex->label
  (for-each (lambda (vertex)
             (set-label (register-node1 vertex)
                        (if (procedure? vertex->label)
                            (vertex->label vertex)
                            (format #f "~a" (vertex-label vertex)))))
   (graph-vertices graph)))
 (show-object-graph/dot))

(define (graph-maximum-flow graph source sink edge->capacity)
 ;; push-relabel algorithm without the gap heuristic
 ;; doesn't take advantage of sparsity
 ;; need to get rid of all list-refs
 (let ((edge->capacity (lambda (e) (if e (edge->capacity e) 0)))
       (n (length (graph-vertices graph)))
       (flow (ht)) (height (ht)) (excess (ht)) (seen (ht))
       (nodes (removeq source (removeq sink (graph-vertices graph)))))
  (for-each (lambda (u)
             (! excess u 0)
             (! height u 0)
             (! seen u 0)
             (for-each (lambda (v) (! flow (cons u v) 0)) (graph-vertices graph)))
   (graph-vertices graph))
  (define (push u v)
   (let* ((send (min (@ excess u) (- (edge->capacity (edge-between? u v)) (@ flow (cons u v))))))
    (! flow (cons u v) (+ (@ flow (cons u v)) send))
    (! flow (cons v u) (- (@ flow (cons v u)) send))
    (! excess u (- (@ excess u) send))
    (! excess v (+ (@ excess v) send))))
  (define (relabel u)
   (let ((min-height +inf.0))
    (for-each (lambda (v)
               (when (> (- (edge->capacity (edge-between? u v)) (@ flow (cons u v))) 0)
                (set! min-height (min min-height (@ height v)))
                (! height u (+ min-height 1))))
     (graph-vertices graph))))
  (define (discharge u)
   (let loop ()
    (when (> (@ excess u) 0)
     (if (< (@ seen u) n)
         (let* ((v (list-ref (graph-vertices graph) (@ seen u))))
          (if (and (> (- (edge->capacity (edge-between? u v)) (@ flow (cons u v))) 0)
                 (> (@ height u) (@ height v)))
              (push u v)
              (! seen u (+ (@ seen u) 1))))
         (begin (relabel u)
                (! seen u 0)))
     (loop))))
  (! height source n)
  (! excess source +inf.0)
  (for-each (lambda (v) (push source v)) (graph-vertices graph))
  (let loop ((p 0))
   (when (< p (length nodes))
    (let* ((u (list-ref nodes p))
           (old-height (@ height u)))
     (discharge u)
     (if (> (@ height u) old-height)
         (begin (set! nodes (cons u (list-remove nodes p)))
                (loop 0))
         (loop (+ p 1))))))
  (cons
   (let ((sum 0))
    (hash-table-walk flow (lambda (key val) (when (eq? (car key) source)
                                        (set! sum (+ sum val)))))
    sum)
   (hash-table->alist flow))))

(define (graph-laplacian-matrix graph #!key (in-degree? #f))
 ;; out degree is the default
 (define (map-n-matrix f i j)
  ;; prevents a linear-algebra dependency, for now anyway
  (map-n-vector (lambda (i) (map-n-vector (lambda (j) (f i j)) j)) i))
 (let ((vertices (graph-vertices graph))
       (vertex-edges (if in-degree?
                         vertex-in-edges
                         vertex-out-edges)))
  (map-n-matrix
   (lambda (i j)
    (cond ((= i j) (length (vertex-edges (list-ref vertices i))))
          ((edge-between? (list-ref vertices i) (list-ref vertices j)) -1)
          (else 0)))
   (length vertices)
   (length vertices))))

(define (graph-complement graph #!key (vertices->edge-label #f) (simple-graph? #f))
 (assert-directed-graph graph)
 (let ((vertices (map-vertex (lambda (v) (make-vertex (vertex-label v) '())) graph))
       (edges '()))
  (for-each
    (lambda (v-new1 v-old1)
     (for-each
       (lambda (v-new2 v-old2)
        (when (or (not (eq? v-old1 v-old2)) (not simple-graph?))
         (unless (edge-between? v-old1 v-old2)
          (let ((e (make-directed-edge (if vertices->edge-label
                                           (vertices->edge-label v-old1 v-old2)
                                           #f)
                                       v-new1 v-new2)))
           (add-edge! e)
           (push! e edges)))))
      vertices (graph-vertices graph)))
   vertices (graph-vertices graph))
  (make-graph vertices edges)))

(define (graph-maximal-cliques graph)
 ;; Bron-Kerbosch, without pivoting or vertex ordering
 (let ((max-cliques '()))
  (let loop ((r '()) (p (graph-vertices graph)) (x '()))
   (if (and (null? p) (null? x))
       (push! r max-cliques)
       (for-each (lambda (v)
                  (loop (cons v r)
                        (set-intersectionq (vertex-neighbours v) p)
                        (set-intersectionq (vertex-neighbours v) x))
                  (push! v x)
                  (set! p (removeq v p)))
        p)))
  max-cliques))
)
