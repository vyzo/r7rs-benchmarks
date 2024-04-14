;;; GRAPHS -- Obtained from Andrew Wright.

(import :std/error :std/iter)

;;;; ==== util.ss ====


;; Given limit, return the list 0, 1, ..., limit-1.
(def (giota (limit : :fixnum))
  => :list
  (let loop (((i :- :fixnum) limit) ((result :- :list) []))
    => :list
    (if (fx> i 0)
      (let (i (fx- i 1))
        (loop i (cons i result)))
      result)))

(def (integer-andmap (limit : :fixnum) (ok? : :procedure))
  (let loop (((i :- :fixnum) 0))
    (if (fx< i limit)
      (and (ok? i) (loop (fx+ i 1)))
      #t)))

(def (list-ormap lst (ok? : :procedure))
  (let loop ((rest lst))
    (if (null? rest)
      #f
      (using (rest :- :pair)
        (or (ok? (car rest))
            (loop (cdr rest)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vyzo: efficient graph representation as a flat u8vector for the incidence matrix

(defstruct Graph ((size :- :fixnum) (edges :- :u8vector))
  final: #t constructor: :init!)

(defmethod {:init! Graph}
  (lambda (self (size : :fixnum))
    (set! self.size size)
    (set! self.edges (make-u8vector (fx* size size) 0))))

(def (graph-edge? (graph : Graph) (i : :fixnum) (j : :fixnum))
  => :boolean
  (fx= 1 (graph-edge graph i j)))

(def (graph-edge (graph : Graph) (i : :fixnum) (j : :fixnum))
  => :fixnum
  (:- (##u8vector-ref graph.edges (fx+ (fx* graph.size i) j)) :fixnum))

(def (graph-edge-set! (graph : Graph) (i : :fixnum) (j : :fixnum) (edge : :fixnum))
  (##u8vector-set! graph.edges (fx+ (fx* graph.size i) j) edge))

(def (graph-degree (graph : Graph) (v : :fixnum))
  => :fixnum
  (let* ((start (fx* graph.size v))
         (end   (fx+ start graph.size)))
    (declare (fixnum) (not safe))
    (:- (for/fold (r 0) (i (in-range start end))
          (fx+ r (:- (##u8vector-ref graph.edges i) :fixnum)))
        :fixnum)))

(def (graph-copy (graph : Graph))
  => Graph
  (:- (##structure Graph::t graph.size (u8vector-copy graph.edges))
      Graph))

;;; ==== ptfold.ss ====

(defrule (Accross)
  'accross)
(def (Accross? obj)
  (eq? 'accross obj))

(defstruct Deeper  (state)
  final: #t)

;; Fold over the tree of permutations of a universe.
;; Each branch (from the root) is a permutation of universe.
;; Each node at depth d corresponds to all permutations which pick the
;; elements spelled out on the branch from the root to that node as
;; the first d elements.
;; Their are two components to the state:
;;       The b-state is only a function of the branch from the root.
;;       The t-state is a function of all nodes seen so far.
;; At each node, b-folder is called via
;;       (b-folder elem b-state t-state deeper accross)
;; where elem is the next element of the universe picked.
;; If b-folder can determine the result of the total tree fold at this stage,
;; it should simply return the result.
;; If b-folder can determine the result of folding over the sub-tree
;; rooted at the resulting node, it should call accross via
;;       (accross new-t-state)
;; where new-t-state is that result.
;; Otherwise, b-folder should call deeper via
;;       (deeper new-b-state new-t-state)
;; where new-b-state is the b-state for the new node and new-t-state is
;; the new folded t-state.
;; At the leaves of the tree, t-folder is called via
;;       (t-folder b-state t-state accross)
;; If t-folder can determine the result of the total tree fold at this stage,
;; it should simply return that result.
;; If not, it should call accross via
;;       (accross new-t-state)
;; Note, fold-over-perm-tree always calls b-folder in depth-first order.
;; I.e., when b-folder is called at depth d, the branch leading to that
;; node is the most recent calls to b-folder at all the depths less than d.
;; This is a gross efficiency hack so that b-folder can use mutation to
;; keep the current branch.
(def (fold-over-perm-tree universe
       (b-folder : :procedure)
       (t-folder : :procedure)
       state)

  (defrule (continue accross)
    (using (accross :- :procedure)
      (accross)))

  (def (fold-outer universe state accross)
    (if (null? universe)
      (let (cont (t-folder state))
        (if (Accross? cont)
          (continue accross)
          cont))
      (fold-inner universe [] state accross)))

  (def (fold-inner in out state accross)
    (using (in :- :pair)
      (let* ((first (car in))
             (rest  (cdr in)))
        (let (cont (b-folder first state))
          (cond
           ((Accross? cont)
            (if (null? rest)
              (continue accross)
              (fold-inner rest (cons first out) state accross)))
           ((Deeper? cont)
            (using (cont :- Deeper)
              (if (null? rest)
                (fold-outer (squash out rest) cont.state accross)
                (let (accross
                      (lambda ()
                        (fold-inner rest (cons first out) state accross)))
                  (fold-outer (squash out rest)
                              cont.state
                              accross)))))
           (else cont))))))

  (fold-outer universe state (lambda () #t)))

(def (squash out rest)
  (let loop ((current out) (result rest))
    (if (null? current)
      result
      (using (current :- :pair)
        (loop (cdr current) (cons (car current) result))))))

;;; ==== minimal.ss ====


;; A directed graph is stored as a connection matrix (vector-of-vectors)
;; where the first index is the `from' vertex and the second is the `to'
;; vertex.  Each entry is a bool indicating if the edge exists.
;; The diagonal of the matrix is never examined.
;; Make-minimal? returns a procedure which tests if a labelling
;; of the verticies is such that the matrix is minimal.
;; If it is, then the procedure returns the result of folding over
;; the elements of the automoriphism group.  If not, it returns #f.
;; The folding is done by calling folder via
;;       (folder perm state accross)
;; If the folder wants to continue, it should call accross via
;;       (accross new-state)
;; If it just wants the entire minimal? procedure to return something,
;; it should return that.
;; The ordering used is lexicographic (with #t > #f) and entries
;; are examined in the following order:
;;       1->0, 0->1
;;
;;       2->0, 0->2
;;       2->1, 1->2
;;
;;       3->0, 0->3
;;       3->1, 1->3
;;       3->2, 2->3
;;       ...
(def (make-minimal? (max-size : :fixnum))
  => :procedure
  (let ((iotas (make-iotas (fx+ max-size 1)))
        (perm (make-vector max-size 0)))
    (lambda ((size :- :fixnum) (graph :- Graph) (folder :- :procedure))
      (fold-over-perm-tree (vector-ref iotas size)
        (lambda ((perm-x :- :fixnum)
            (x :- :fixnum))
          (let (r (cmp-next-vertex graph perm x perm-x))
            (cond
             ((fx< r 0) #f)
             ((fx= r 0)
              (##vector-set! perm x perm-x)
              (Deeper (fx+ x 1)))
             (else
              (Accross)))))
        (lambda (leaf-depth)
          (folder perm))
        0))))

(def (make-iotas (size : :fixnum))
  (declare (fixnum) (not safe))
  (let (result (make-vector size))
    (##vector-set! result 0 [])
    (for (i (in-range 1 size))
      (##vector-set! result i (giota i)))
    result))


;; Given a graph, a partial permutation vector, the next input and the next
;; output, return less (-1), equal (0) or more (1) depending on the lexicographic
;; comparison between the permuted and un-permuted graph.
;; vyzo: this has been optimized to be a branchless constant time computation.
(def (cmp-next-vertex (graph  : Graph)
                      (perm   : :vector)
                      (x      : :fixnum)
                      (perm-x : :fixnum))
  (let loop (((y :- :fixnum) 0))
    (if (fx< y x)
      (let* ((perm-y         (:- (##vector-ref perm y) :fixnum))
             (px->py         (graph-edge graph perm-x perm-y))
             (py->px         (graph-edge graph perm-y perm-x))
             (x->y           (graph-edge graph x y))
             (y->x           (graph-edge graph y x))
             (discriminant   (fxior x->y
                                    (fxarithmetic-shift-left y->x   1)
                                    (fxarithmetic-shift-left px->py 2)
                                    (fxarithmetic-shift-left py->px 3)))
             (result         (:- (##vector-ref __cmp discriminant) :fixnum)))
        (if (fx= 0 result)
          (loop (fx+ y 1))
          result))
      0)))

(def __cmp
  (let (cmp (make-vector 16 0))
    (let loop ((i 0))
      (when (fx< i 16)
        (let ((x->y   (fxand i 1))
              (y->x   (fxarithmetic-shift-right (fxand i 2) 1))
              (px->py (fxarithmetic-shift-right (fxand i 4) 2))
              (py->px (fxarithmetic-shift-right (fxand i 8) 3)))
          (cond
           ((fx= x->y px->py)
            (cond
             ((fx= y->x py->px)
              (vector-set! cmp i 0))
             ((fx= y->x 1)
              (vector-set! cmp i -1))
             (else
              (vector-set! cmp i 1))))
           ((fx= x->y 1)
            (vector-set! cmp i -1))
           (else
            (vector-set! cmp i 1)))
          (loop (fx+ i 1)))))
    cmp))


;;; ==== rdg.ss ====


;; Fold over rooted directed graphs with bounded out-degree.
;; Size is the number of verticies (including the root).  Max-out is the
;; maximum out-degree for any vertex.  Folder is called via
;;       (folder edges state)
;; where edges is a list of length size.  The ith element of the list is
;; a list of the verticies j for which there is an edge from i to j.
;; The last vertex is the root.
(def (fold-over-rdg (size    : :fixnum)
                    (max-out : :fixnum)
                    (folder  : :procedure)
                    state)
  (let* ((root (fx- size 1))
         (graph (Graph size))
         (minimal-folder (make-minimal? root))
         (continue
          (lambda (perm)
            (Accross)))
         (non-root-minimal?
          (lambda ((size :- :fixnum))
            (minimal-folder size graph continue)))
         (continue
          (lambda ((perm :- :vector))
            (let (r (cmp-next-vertex graph perm root root))
              (if (fx< r 0)
                #f
                (Accross)))))
         (root-minimal?
          (lambda ()
            (minimal-folder root graph continue))))

    (def (finish!)
      (let (rgraph (make-reachability-graph graph))
        (let loop (((v    :- :fixnum) 0)
                   ((outs :- :fixnum) 0)
                   (efr []))
          (def (root-connected?)
            (integer-andmap
             root
             (lambda ((v :- :fixnum))
               (list-ormap
                efr
                (lambda ((r :- :fixnum))
                  (graph-edge? rgraph r v))))))
          (cond
           ((not (or (fx= v root)
                     (fx= outs max-out)))
            (graph-edge-set! graph root v 1)
            (loop (fx+ v 1) (fx+ outs 1) (cons v efr))
            (graph-edge-set! graph root v 0)
            (loop (fx+ v 1) outs efr))
           ((and (root-connected?)
                 (root-minimal?))
            (let (result (graph-copy graph))
              (set! state
                (folder result state))))))))

    (let fold (((vertex :- :fixnum) 0)
               ((sv     :- :fixnum) 0)
               ((outs   :- :fixnum) 0))
      (if (fx< sv vertex)
        (begin
          (fold vertex (fx+ sv 1) outs)
          (let (sv-out (graph-degree graph sv))
            (unless (fx= sv-out max-out)
              (let ()
                (graph-edge-set! graph sv vertex 1)
                (fold vertex (fx+ sv 1) outs)
                (unless (fx= outs max-out)
                  (let ()
                    (graph-edge-set! graph vertex sv 1)
                    (fold vertex (fx+ sv 1) (fx+ outs 1))
                    (graph-edge-set! graph vertex sv 0)))
                (graph-edge-set! graph sv vertex 0)))
            (when (fx< outs max-out)
              (let ()
                (graph-edge-set! graph vertex sv 1)
                (fold vertex (fx+ sv 1) (fx+ outs 1))
                (graph-edge-set! graph vertex sv 0)))))
        (let (vertex (fx+ vertex 1))
          (when (non-root-minimal? vertex)
            (if (fx= vertex root)
              (finish!)
              (fold vertex 0 0))))))
    state))

(def (make-reachability-graph (graph : Graph))
  => Graph
  (declare
    (fixnum)    ;; for loop arithmetic
    (not safe)) ;; no need to bounds check here
  (let (size graph.size)
    (using (result (graph-copy graph) :- Graph)
      (for (v (in-range size))
        (using (v :- :fixnum)
          (graph-edge-set! result v v 1)))
      (for (m (in-range size))
        (using (m :- :fixnum)
          (for (f (in-range size))
            (using (f :- :fixnum)
              (when (graph-edge? result f m)
                (for (t (in-range size))
                  (using (t :- :fixnum)
                    (graph-edge-set! result f t
                                     (fxior (graph-edge result f t)
                                            (graph-edge result m t))))))))))
      result)))

;;; ==== test input ====

;; Produces all directed graphs with N verticies, distinguished root,
;; and out-degree bounded by 2, upto isomorphism.

(define (run n)
  (fold-over-rdg n
                 2
                 cons
                 '()))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "graphs"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (length (run (hide count input1))))
     (lambda (result) (= result output)))))
