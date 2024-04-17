;;; PUZZLE -- Forest Baskett's Puzzle benchmark, originally written in Pascal.

(import :std/iter)

(def board-size 512)
(def D          8)
(def classes    4)
(def piececount (u8vector))
(def pieces     (vector))
(def puzzle     (u8vector))
(def count      0)

(defrule (index-of i j k)
  (fx+ i (fx* D (fx+ j (fx* D k)))))

(defrule (u8-ref bits i)
  (:- (##u8vector-ref bits i) :fixnum))

(defrule (u8-set! bits i v)
  (##u8vector-set! bits i v))

(defstruct Piece ((class  :- :fixnum)
                  (bits   :- :u8vector)
                  (maxbit :- :fixnum))
  constructor: :init!
  final: #t)

(defmethod {:init! Piece}
  (lambda (self (class : :fixnum)
           (ii    : :fixnum)
           (jj    : :fixnum)
           (kk    : :fixnum))
    (set! self.class class)
    (let (bitmap (make-u8vector board-size 0))
      (declare (fixnum) (not safe))
      (for (i (in-range (fx+ ii 1)))
        (for (j (in-range (fx+ jj 1)))
          (for (k (in-range (fx+ kk 1)))
            (let (bit (index-of i j k))
              (u8vector-set! bitmap bit 1)
              (set! self.maxbit bit)))))
      (set! self.bits bitmap))))

(def (start (size : :fixnum))
  (set! count 0)
  (set! puzzle (make-u8vector board-size 1))
  (let ()
    (declare (fixnum) (not safe))
    (for (i (in-range 1 6))
      (for (j (in-range 1 6))
        (for (k (in-range 1 6))
          (u8vector-set! puzzle (index-of i j k) 0)))))
  (set! pieces
    (vector (Piece 0 3 1 0)
            (Piece 0 1 0 3)
            (Piece 0 0 3 1)
            (Piece 0 1 3 0)
            (Piece 0 3 0 1)
            (Piece 0 0 1 3)
            (Piece 1 2 0 0)
            (Piece 1 0 2 0)
            (Piece 1 0 0 2)
            (Piece 2 1 1 0)
            (Piece 2 1 0 1)
            (Piece 2 0 1 1)
            (Piece 3 1 1 1)))
  (set! piececount
    (u8vector 13 3 1 1))

  ;; run the puzzle solver
  (let ((m (index-of 1 1 1))
        (n 0))
    (cond
     ((fit 0 m)
      (set! n (place 0 m)))
     (else
      (error "failed initial fit")))
    (and (trial n)
         count)))

(def (trial (j : :fixnum))
  (let ((k 0))
    (call-with-current-continuation
     (lambda ((return :- :procedure))
       (for (i (in-range (vector-length pieces)))
         (using ((i :- :fixnum)
                 (p (##vector-ref pieces i) :- Piece))
           (unless (fx= 0 (u8-ref piececount p.class))
             (when (fit i j)
               (set! k (place i j))
               (cond
                ((or (trial k) (fx= k 0))
                 (set! count (fx+ count 1))
                 (return #t))
                (else
                 (puzzle-remove i j)))))))
       (set! count (fx+ count 1))
       #f))))

(def (fit (i : :fixnum) (j : :fixnum))
  (using (piece (vector-ref pieces i) :- Piece)
    (let ((bits piece.bits)
          (end  piece.maxbit))
      (do (((k :- :fixnum) 0 (fx+ k 1)))
          ((or (fx> k end)
               (and (fx= 1 (u8-ref bits k))
                    (fx= 1 (u8-ref puzzle (fx+ j k)))))
           (fx> k end))))))

(def (place (i : :fixnum) (j : :fixnum))
  => :fixnum
  (using (piece (vector-ref pieces i) :- Piece)
    (let ((bits piece.bits)
          (end  piece.maxbit))
      (do (((k :- :fixnum) 0 (fx+ k 1)))
          ((fx> k end))
        (when (fx= 1 (u8-ref bits k))
          (u8-set! puzzle (fx+ j k) 1)))
      (u8-set! piececount piece.class
               (fx- (u8-ref piececount piece.class) 1))
      (let (size (##u8vector-length puzzle))
        (:- (do (((k :- :fixnum) j (fx+ k 1)))
                ((or (fx= k size) (fx= 0 (u8-ref puzzle k)))
                 (if (fx= k size) 0 k)))
            :fixnum)))))

(def (puzzle-remove (i : :fixnum) (j : :fixnum))
  (using (piece (vector-ref pieces i) :- Piece)
    (let ((bits piece.bits)
          (end  piece.maxbit))
      (do (((k :- :fixnum) 0 (fx+ k 1)))
          ((fx> k end))
        (when (fx= 1 (u8-ref piece.bits k))
          (u8-set! puzzle (fx+ j k) 0)))
      (u8-set! piececount
               piece.class
               (fx+ (u8-ref piececount piece.class) 1)))))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 input1)
         (name "puzzle"))
    (run-r7rs-benchmark
     (string-append name ":" s2)
     count
     (lambda () (start (hide count input1)))
     (lambda (result) (equal? result output)))))
