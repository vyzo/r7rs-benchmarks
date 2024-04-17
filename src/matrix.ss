;;; MATRIX -- Obtained from Andrew Wright.

;;; We need R6RS div and mod for this benchmark.

(def (div x y)
  (declare (fixnum) (not safe))
  (cond
   ((>= x 0)
    (quotient x y))
   ((< y 0)
    ;; x < 0, y < 0
    (let* ((q (quotient x y))
           (r (- x (* q y))))
      (if (= r 0)
        q
        (+ q 1))))
   (else
    ;; x < 0, y > 0
    (let* ((q (quotient x y))
           (r (- x (* q y))))
      (if (= r 0)
        q
        (- q 1))))))

(def (mod x y)
  (declare (fixnum) (not safe))
  (cond
   ((>= x 0)
    (remainder x y))
   ((< y 0)
    ;; x < 0, y < 0
    (let* ((q (quotient x y))
           (r (- x (* q y))))
      (if (= r 0)
        0
        (- r y))))
   (else
    ;; x < 0, y > 0
    (let* ((q (quotient x y))
           (r (- x (* q y))))
      (if (= r 0)
        0
        (+ r y))))))

;; Test that a matrix with entries in {+1, -1} is maximal among the matricies
;; obtainable by
;;       re-ordering the rows
;;       re-ordering the columns
;;       negating any subset of the columns
;;       negating any subset of the rows
;; Where we compare two matricies by lexicographically comparing the first row,
;; then the next to last, etc., and we compare a row by lexicographically
;; comparing the first entry, the second entry, etc., and we compare two
;; entries by +1 > -1.
;; Note, this scheme obeys the useful fact that if (append mat1 mat2) is
;; maximal, then so is mat1.  Thus, we can build up maximal matricies
;; row by row.
;;
;; Once you have chosen the row re-ordering so that you know which row goes
;; last, the set of columns to negate is fixed (since the last row must be
;; all +1's).
;;
;; Note, the column ordering is really totally determined as follows:
;;       all columns for which the second row is +1 must come before all
;;               columns for which the second row is -1.
;;       among columns for which the second row is +1, all columns for which
;;               the third row is +1 come before those for which the third is
;;               -1, and similarly for columns in which the second row is -1.
;;       etc
;; Thus, each succeeding row sorts columns withing refinings equivalence
;; classes.
;;
;; Maximal? assumes that mat has atleast one row, and that the first row
;; is all +1's.
(def (maximal? mat)
  (let pick-first-row ((first-row-perm (gen-perms mat)))
    (if first-row-perm
      (and (zunda first-row-perm mat)
           (pick-first-row (first-row-perm 'brother)))
      #t)))

(def (zunda first-row-perm mat)
  (let* ((first-row (first-row-perm 'now))
         (number-of-cols (length first-row))
         (make-row->func
          (lambda (if-equal if-different)
            (lambda (row)
              (let ((vec (make-vector number-of-cols)))
                (declare (not safe))
                (do (((i :- :fixnum) 0 (fx+ i 1))
                     (first first-row (cdr first))
                     (row row (cdr row)))
                    ((fx= i number-of-cols))
                  (vector-set! vec
                               i
                                 (if (fx= (car first) (car row))
                                   if-equal
                                   if-different)))
                (lambda (i)
                  (vector-ref vec i))))))
         (mat (cdr mat)))
    (zebra (first-row-perm 'child)
           (make-row->func 1 -1)
           (make-row->func -1 1)
           mat
           number-of-cols)))


(def (zebra row-perm (row->func+ : :procedure) (row->func- : :procedure) mat number-of-cols)
  (let loop ((row-perm row-perm) (mat mat) (partitions (list (miota number-of-cols))))
    (or (not row-perm)
        (using ((row-perm :- :procedure) (mat :- :pair))
          (and
            (zulu (car mat)
                  (:- (row->func+ (row-perm 'now)) :procedure)
                  partitions
                  (lambda (new-partitions)
                    (loop (row-perm 'child)
                          (cdr mat)
                          new-partitions)))
            (zulu (car mat)
                  (:- (row->func- (row-perm 'now)) :procedure)
                  partitions
                  (lambda (new-partitions)
                    (loop (row-perm 'child)
                          (cdr mat)
                          new-partitions)))
            (let ((new-row-perm (row-perm 'brother)))
              (or (not new-row-perm)
                  (loop new-row-perm
                        mat
                        partitions))))))))

(def (cons-if-not-null lhs rhs)
  (if (null? lhs)
    rhs
    (cons lhs rhs)))

(def (zulu old-row (new-row-func : :procedure) partitions (equal-cont : :procedure))
  (declare (not safe))
  (let loop ((p-in partitions) (old-row old-row) (rev-p-out []))
    (let _split ((partition (car p-in)) (old-row old-row) (plus []) (minus []))
      (if (null? partition)
        (let _minus ((old-row old-row) (m minus))
          (if (null? m)
            (let ((rev-p-out
                   (cons-if-not-null
                    minus
                    (cons-if-not-null
                     plus
                     rev-p-out)))
                  (p-in (cdr p-in)))
              (if (null? p-in)
                (equal-cont (reverse! rev-p-out))
                (loop p-in old-row rev-p-out)))
            (using ((m :- :pair) (old-row :- :pair))
              (or (fx= 1 (car old-row))
                  (_minus (cdr old-row)
                          (cdr m))))))
        (using ((partition :- :pair) (old-row :- :pair))
          (let ((next (car partition)))
            (case (new-row-func next)
              ((1)
               (and (fx= 1 (car old-row))
                    (_split (cdr partition)
                            (cdr old-row)
                            (cons next plus)
                            minus)))
              ((-1)
               (_split (cdr partition)
                       old-row
                       plus
                       (cons next minus))))))))))

(def (all? (ok? : :procedure) lst)
  (let loop ((lst lst))
    (or (null? lst)
        (using (lst :- :pair)
          (and (ok? (car lst))
               (loop (cdr lst)))))))

(def (gen-perms objects)
  (let loop ((zulu-future objects) (past []))
    (if (null? zulu-future)
      #f
      (using (zulu-future :- :pair)
        (lambda (msg)
          (case msg
            ((now)
             (car zulu-future))
            ((brother)
             (loop (cdr zulu-future)
                   (cons (car zulu-future)
                         past)))
            ((child)
             (gen-perms
              (fold past cons (cdr zulu-future))))
            ((puke)
             (cons (car zulu-future)
                   (fold past cons (cdr zulu-future))))
            (else
             (error "gen-perms: bad msg: " msg))))))))

(def (fold lst (folder : :procedure) state)
  (let loop ((lst lst) (state state))
    (if (null? lst)
      state
      (using (lst :- :pair)
        (loop (cdr lst)
              (folder (car lst) state))))))

(def (miota len)
  (declare (fixnum) (not safe))
  (let loop ((i 0))
    (if (= i len)
      []
      (cons i (loop (+ i 1))))))

(def (proc->vector size proc)
  (declare (fixnum) (not safe))
  (let ((res (make-vector size)))
    (do ((i 0 (+ i 1)))
        ((= i size))
      (vector-set! res i (proc i)))
    res))

;; Given a prime number P, return a procedure which, given a `maker' procedure,
;; calls it on the operations for the field Z/PZ.
(def (make-modular modulus)
  (let* ((reduce
          (lambda (x)
            (mod x modulus)))
         (coef-zero?
          (lambda (x)
            (zero? (reduce x))))
         (coef-+
          (lambda (x y)
            (reduce (+ x y))))
         (coef-negate
          (lambda (x)
            (reduce (- x))))
         (coef-*
          (lambda (x y)
            (reduce (* x y))))
         (coef-recip
          (let ((inverses
                 (proc->vector (- modulus 1)
                   (lambda (i)
                     (extended-gcd (+ i 1)
                                   modulus
                                   (lambda (gcd inverse ignore)
                                     inverse))))))
            ;; Coef-recip.
            (lambda (x)
              (let ((x (reduce x)))
                (vector-ref inverses (- x 1)))))))
    (lambda (maker)
      (maker 0 ;; coef-zero
             1 ;; coef-one
             coef-zero?
             coef-+
             coef-negate
             coef-*
             coef-recip))))

;; Extended Euclidean algorithm.
;; (extended-gcd a b cont) computes the gcd of a and b, and expresses it
;; as a linear combination of a and b.  It returns calling cont via
;;       (cont gcd a-coef b-coef)
;; where gcd is the GCD and is equal to a-coef * a + b-coef * b.
(def (n->sgn/abs x cont)
  (if (>= x 0)
    (cont 1 x)
    (cons -1 (- x))))

(def (extended-gcd a b cont)
  (n->sgn/abs a
              (lambda (p-a p)
                (n->sgn/abs b
                            (lambda (q-b q)
                              (let loop ((p p)
                                         (p-a p-a)
                                         (p-b 0)
                                         (q q)
                                         (q-a 0)
                                         (q-b q-b))
                                (if (zero? q)
                                  (cont p p-a p-b)
                                  (let ((mult
                                         (div p q)))
                                    (loop q
                                          q-a
                                          q-b
                                          (- p (* mult q))
                                          (- p-a (* mult q-a))
                                          (- p-b (* mult q-b)))))))))))

;; Given elements and operations on the base field, return a procedure which
;; computes the row-reduced version of a matrix over that field.  The result
;; is a list of rows where the first non-zero entry in each row is a 1 (in
;; the coefficient field) and occurs to the right of all the leading non-zero
;; entries of previous rows.  In particular, the number of rows is the rank
;; of the original matrix, and they have the same row-space.
;; The items related to the base field which are needed are:
;;       coef-zero       additive identity
;;       coef-one        multiplicative identity
;;       coef-zero?      test for additive identity
;;       coef-+          addition (two args)
;;       coef-negate     additive inverse
;;       coef-*          multiplication (two args)
;;       coef-recip      multiplicative inverse
;; Note, matricies are stored as lists of rows (i.e., lists of lists).
(def (make-row-reduce (coef-zero   : :fixnum)
                      (coef-one    : :fixnum)
                      (coef-zero?  : :procedure)
                      (coef-+      : :procedure)
                      (coef-negate : :procedure)
                      (coef-*      : :procedure)
                      (coef-recip  : :procedure))
  => :procedure
  (lambda (mat)
    (let loop ((mat mat))
      (if (or (null? mat)
              (null? (car mat)))
        []
        (let loop-inner ((in mat) (out []))
          (if (null? in)
            (map
              (lambda (x)
                (cons coef-zero x))
              (loop out))
            (using (in :- :pair)
              (let* ((prow (car in))
                     (pivot (car prow))
                     (prest (cdr prow))
                     (in (cdr in)))
                (if (coef-zero? pivot)
                  (loop-inner in (cons prest out))
                  (let ((zap-row
                         (map
                           (let ((mult (coef-recip pivot)))
                             (lambda (x) (coef-* mult x)))
                           prest)))
                    (cons (cons coef-one zap-row)
                          (map
                            (lambda (x)
                              (cons coef-zero x))
                            (loop
                             (fold in
                                   (lambda (row mat)
                                     (cons
                                      (let ((first-col (car row))
                                            (rest-row (cdr row)))
                                        (if (coef-zero? first-col)
                                          rest-row
                                          (map
                                            (let ((mult (coef-negate first-col)))
                                              (lambda (f z)
                                                (coef-+ f (coef-* mult z))))
                                            rest-row
                                            zap-row)))
                                      mat))
                                   out))))))))))))))


;; Given elements and operations on the base field, return a procedure which
;; when given a matrix and a vector tests to see if the vector is in the
;; row-space of the matrix.  This returned function is curried.
;; The items related to the base field which are needed are:
;;       coef-zero       additive identity
;;       coef-one        multiplicative identity
;;       coef-zero?      test for additive identity
;;       coef-+          addition (two args)
;;       coef-negate     additive inverse
;;       coef-*          multiplication (two args)
;;       coef-recip      multiplicative inverse
;; Note, matricies are stored as lists of rows (i.e., lists of lists).
(def (make-in-row-space? (coef-zero   : :fixnum)
                         (coef-one    : :fixnum)
                         (coef-zero?  : :procedure)
                         (coef-+      : :procedure)
                         (coef-negate : :procedure)
                         (coef-*      : :procedure)
                         (coef-recip  : :procedure))
  => :procedure
  (let ((row-reduce
         (make-row-reduce coef-zero
                          coef-one
                          coef-zero?
                          coef-+
                          coef-negate
                          coef-*
                          coef-recip)))
    (lambda (mat)
      (let ((mat (row-reduce mat)))
        (lambda (row)
          (let loop ((row row) (mat mat))
            (if (null? row)
              #t
              (using (row :- :pair)
                (let ((r-first (car row))
                      (r-rest (cdr row)))
                  (cond ((coef-zero? r-first)
                         (loop r-rest
                               (map cdr
                                    (if (or (null? mat) (coef-zero? (caar mat)))
                                      mat
                                      (cdr mat)))))
                        ((null? mat) #f)
                        (else
                         (using (mat :- :pair)
                           (let* ((zap-row (car mat))
                                  (z-first (car zap-row))
                                  (z-rest (cdr zap-row))
                                  (mat (cdr mat)))
                             (if (coef-zero? z-first)
                               #f
                               (loop
                                (map
                                  (let ((mult (coef-negate r-first)))
                                    (lambda (r z)
                                      (coef-+ r (coef-* mult z))))
                                  r-rest
                                  z-rest)
                                (map cdr mat))))))))))))))))


;; Given a prime number, return a procedure which takes integer matricies
;; and returns their row-reduced form, modulo the prime.
(def make-modular-row-reduce
  (lambda (modulus)
    ((make-modular modulus)
     make-row-reduce)))


(def make-modular-in-row-space?
  (lambda (modulus)
    ((make-modular modulus)
     make-in-row-space?)))



;; Usual utilities.

;; Given a bound, find a prime greater than the bound.
(def (find-prime bound)
  (let* ((primes (list 2))
         (last (box primes))
         (is-next-prime?
          (lambda (trial)
            (let loop ((primes primes))
              (or (null? primes)
                  (let ((p
                         (car primes)))
                    (or (< trial (* p p))
                        (and (not (zero? (mod trial p)))
                             (loop (cdr primes))))))))))
    (if (> 2 bound)
      2
      (let loop ((trial 3))
        (if (is-next-prime? trial)
          (let ((entry (list trial)))
            (set-cdr! (unbox last) entry)
            (set-box! last entry)
            (if (> trial bound)
              trial
              (loop (+ trial 2))))
          (loop (+ trial 2)))))))

;; Given the size of a square matrix consisting only of +1's and -1's,
;; return an upper bound on the determinant.
(def (det-upper-bound (size : :fixnum))
  (declare (fixnum) (not safe))
  (let (main-part (expt size (div size 2)))
    (if (even? size)
      main-part
      (* main-part
         (do ((i 0 (+ i 1)))
             ((>= (* i i) size)
              i))))))

;; Fold over all maximal matrices.
(def (go number-of-cols (inv-size : :fixnum) (folder : :procedure) state)
  (let* ((in-row-space?
          (make-modular-in-row-space?
           (find-prime (det-upper-bound inv-size))))
         (make-tester
          (lambda (mat)
            (let ((tests
                   (let ((old-mat (cdr mat))
                         (new-row (car mat)))
                     (fold-over-subs-of-size old-mat
                                             (fx- inv-size 2)
                                             (lambda (sub tests)
                                               (cons
                                                (in-row-space?
                                                 (cons new-row sub))
                                                tests))
                                             []))))
              (lambda (row)
                (let loop ((tests tests))
                  (and (not (null? tests))
                       (or ((car tests) row)
                           (loop (cdr tests)))))))))
         (all-rows ;; all rows starting with +1 in decreasing order
          (fold
           (fold-over-rows (- number-of-cols 1)
                           cons
                           [])
           (lambda (row rows)
             (cons (cons 1 row)
                   rows))
           [])))
    (let loop (((number-of-rows :- :fixnum) 1)
               (rev-mat (list (car all-rows)))
               (possible-future (cdr all-rows))
               (state state))
      (let ((zulu-future
             (remove-in-order
              (if (fx< number-of-rows inv-size)
                (in-row-space? rev-mat)
                (make-tester rev-mat))
              possible-future)))
        (if (null? zulu-future)
          (folder (reverse rev-mat) state)
          (let loop-inner ((zulu-future zulu-future)
                           (state state))
            (if (null? zulu-future)
              state
              (using (zulu-future :- :pair)
                (let ((rest-of-future (cdr zulu-future)))
                  (loop-inner rest-of-future
                    (let* ((first (car zulu-future))
                           (new-rev-mat (cons first rev-mat)))
                      (if (maximal? (reverse new-rev-mat))
                        (loop (fx+ number-of-rows 1)
                              new-rev-mat
                              rest-of-future
                              state)
                        state))))))))))))

(def (go-folder mat bsize.blen.blist)
  (let ((bsize (car bsize.blen.blist))
        (size (length mat)))
    (if (fx< size bsize)
      bsize.blen.blist
      (let ((blen (cadr bsize.blen.blist))
            (blist (cddr bsize.blen.blist)))
        (if (fx= size bsize)
          (let ((blen (fx+ blen 1)))
            (cons bsize
                  (cons blen
                        (cond
                         ((fx< blen 3000)
                          (cons mat blist))
                         ((fx= blen 3000)
                          (cons "..." blist))
                         (else blist)))))
          (list size 1 mat))))))

(def (really-go number-of-cols inv-size)
  (cddr
   (go number-of-cols
       inv-size
       go-folder
       (list -1 -1))))

(def (remove-in-order (remove? : :procedure) lst)
  (reverse!
   (fold lst
         (lambda (e lst)
           (if (remove? e)
             lst
             (cons e lst)))
         [])))

;; The first fold-over-rows is slower than the second one, but folds
;; over rows in lexical order (large to small).
(def (fold-over-rows (number-of-cols : :fixnum) (folder : :procedure) state)
  (if (fxzero? number-of-cols)
    (folder [] state)
    (fold-over-rows (fx- number-of-cols 1)
                    (lambda (tail state)
                      (folder (cons -1 tail) state))
                    (fold-over-rows (fx- number-of-cols 1)
                                    (lambda (tail state)
                                      (folder (cons 1 tail) state))
                                    state))))

;; Fold over subsets of a given size.
(def (fold-over-subs-of-size universe (size : :fixnum) (folder : :procedure) state)
  (let (usize (length universe))
    (if (fx< usize size)
      state
      (let loop (((size :- :fixnum) size)
                 (universe universe)
                 ((folder :- :procedure) folder)
                 ((csize :- :fixnum) (fx- usize size))
                 (state state))
        (cond
         ((fxzero? csize)
          (folder universe state))
         ((fxzero? size)
          (folder [] state))
         (else
          (let ((first-u (car universe))
                (rest-u (cdr universe)))
            (loop size
                  rest-u
                  folder
                  (fx- csize 1)
                  (loop (fx- size 1)
                        rest-u
                        (lambda (tail state)
                          (folder (cons first-u tail) state))
                        csize
                        state)))))))))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "matrix"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3)
     count
     (lambda () (really-go (hide count input1) (hide count input2)))
     (lambda (result) (equal? result output)))))
