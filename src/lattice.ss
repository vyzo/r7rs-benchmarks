;;; LATTICE -- Obtained from Andrew Wright.

;; Given a comparison routine that returns one of
;;       less
;;       more
;;       equal
;;       uncomparable
;; return a new comparison routine that applies to sequences.
(def (lexico (base : :procedure))
  => :procedure

  (def (lex-fixed fixed lhs rhs)
    (if (null? lhs)
      fixed
      (let check (((lhs :- :pair) lhs) ((rhs :- :pair) rhs))
        (let ((probe
               (base (car lhs)
                     (car rhs))))
          (if (or (eq? probe 'equal)
                  (eq? probe fixed))
            (if (null? (cdr lhs))
              fixed
              (check (cdr lhs)
                     (cdr rhs)))
            'uncomparable)))))

  (def (lex-first lhs rhs)
    (if (null? lhs)
      'equal
      (using ((lhs :- :pair) (rhs :- :pair))
        (let (probe
              (base (car lhs)
                    (car rhs)))
          (case probe
            ((less more)
             (lex-fixed probe
                        (cdr lhs)
                        (cdr rhs)))
            ((equal)
             (lex-first (cdr lhs)
                        (cdr rhs)))
            (else 'uncomparable))))))

  lex-first)

(defstruct Lattice ((elements :- :list) (cmp :- :procedure))
  final: #t)

;; Select elements of a list which pass some test.
(def (zulu-select (test : :procedure) lst)
  (let select ((ac []) (lst lst))
    (if (null? lst)
      (xreverse! ac)
      (using (lst :- :pair)
        (select
         (let ((head (car lst)))
           (if (test head)
             (cons head ac)
             ac))
         (cdr lst))))))

(def (xreverse! lst)
  (def (rotate (fo :- :pair) fum)
    (let ((next (cdr fo)))
      (set-cdr! fo fum)
      (if (null? next)
        fo
        (rotate next fo))))

  (if (null? lst) [] (rotate lst [])))

;; Select elements of a list which pass some test and map a function
;; over the result.  Note, only efficiency prevents this from being the
;; composition of select and map.
(def (select-map (test : :procedure) (func : :procedure) lst)
  (def (select ac lst)
    (if (null? lst)
      (xreverse! ac)
      (using (lst :- :pair)
        (select
         (let ((head (car lst)))
           (if (test head)
             (cons (func head)
                   ac)
             ac))
         (cdr lst)))))
  (select [] lst))

;; This version of map-and tail-recurses on the last test.
(def (map-and (proc : :procedure) lst)
  (if (null? lst)
    #t
    (let drudge (((lst :- :pair) lst))
      (let (rest (cdr lst))
        (if (null? rest)
          (proc (car lst))
          (and (proc (car lst))
               (drudge rest)))))))

(def (maps-1 (source : Lattice)
             (target : Lattice)
             pas
             new)
  (let ((scmp source.cmp)
        (tcmp target.cmp))
    (let ((less
           (select-map
            (lambda (p)
              (eq? 'less (scmp (car p) new)))
            cdr
            pas))
          (more
           (select-map
            (lambda (p)
              (eq? 'more (scmp (car p) new)))
            cdr
            pas)))
      (zulu-select
       (lambda (t)
         (and (map-and
               (lambda (t2) (memq (tcmp t2 t) '(less equal)))
               less)
          (map-and
           (lambda (t2) (memq (tcmp t2 t) '(more equal)))
           more)))
       target.elements))))

(def (maps-rest (source : Lattice)
                (target : Lattice)
                pas
                rest
                (to-1       : :procedure)
                (to-collect : :procedure))
  (if (null? rest)
    (to-1 pas)
    (using (rest :- :pair)
      (let ((next (car rest))
            (rest (cdr rest)))
        (to-collect
         (map
           (lambda (x)
             (maps-rest source target
                        (cons
                         (cons next x)
                         pas)
                        rest
                        to-1
                        to-collect))
           (maps-1 source target pas next)))))))

(def (maps (source : Lattice) (target : Lattice))
  => Lattice
  (Lattice
   (maps-rest source
              target
              []
              source.elements
              (lambda (x) (list (map cdr x)))
              (lambda (x) (concat x)))
   (lexico target.cmp)))

(def (concat lsts)
  (if (null? lsts)
    []
    (using (lsts :- :pair)
      (let ((first (car lsts)) (rest (cdr lsts)))
        (if (null? rest)
          first
          (let recur ((first first) (rest rest))
            (if (null? first)
              (concat rest)
              (using (first :- :pair)
                (cons (car first)
                      (recur (cdr first) rest))))))))))

(def (count-maps (source : Lattice) (target : Lattice))
  (maps-rest source
             target
             []
             source.elements
             (lambda (x) 1)
             sum))

(def (sum lst)
  => :fixnum
  (if (null? lst)
    0
    (using (lst :- :pair)
      (fx+ (:- (car lst) :fixnum) (sum (cdr lst))))))

(def (run k)
  (let* ((l2
          (Lattice
           '(low high)
           (lambda (lhs rhs)
             (case lhs
               ((low)
                (case rhs
                  ((low)
                   'equal)
                  ((high)
                   'less)
                  (else
                   (error "lattice: base" rhs))))
               ((high)
                (case rhs
                  ((low)
                   'more)
                  ((high)
                   'equal)
                  (else
                   (error "lattice: base" rhs))))
               (else
                (error "lattice: base" lhs))))))
         (l3 (maps l2 l2))
         (l4 (maps l3 l3)))
    (count-maps l2 l2)
    (count-maps l3 l3)
    (count-maps l2 l3)
    (count-maps l3 l2)
    (case k
      ((33) (count-maps l3 l3))
      ((44) (count-maps l4 l4))
      ((45) (let ((l5 (maps l4 l4)))
              (count-maps l4 l5)))
      ((54) (let ((l5 (maps l4 l4)))
              (count-maps l5 l4)))
      ((55) (let ((l5 (maps l4 l4)))
              (count-maps l5 l5)))
      (else (error "run: unanticipated problem size" k)))))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "lattice"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (run (hide count input1)))
     (lambda (result) (= result output)))))
