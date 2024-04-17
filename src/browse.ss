;;; BROWSE -- Benchmark to create and browse through
;;; an AI-like data base of units.
(def *properties* [])

(def (lookup key lst)
  (let loop ((rest lst))
    (if (null? rest)
      #f
      (using (rest :- :pair)
        (let (p (car rest))
          (using (p :- :pair)
            (if (eq? (car p) key)
              p
              (loop (cdr rest)))))))))

(def (getq key lst)
  (alet (p (lookup key lst))
    (using (p :- :pair)
      (cdr p))))

(def (get key1 key2)
  (alet (lst2 (getq key1 *properties*))
    (getq key2 lst2)))

(def (put key1 key2 val)
  (cond
   ((lookup key1 *properties*)
    => (lambda ((lst2 :- :pair))
         (cond
          ((lookup key2 (cdr lst2))
           => (lambda ((p :- :pair))
                (set! (cdr p) val)))
          (else
           (set! (cdr lst2) (cons (cons key2 val) (cdr lst2)))))))
   (else
    (set! *properties*
      (cons [key1 (cons key2 val)] *properties*)))))

(def *current-gensym* 0)
(def (generate-symbol)
  (set! *current-gensym* (fx+ *current-gensym* 1))
  (string->symbol (number->string *current-gensym*)))

(def (append x y)
  (if (null? x)
    y
    (using (x :- :pair)
      (cons (car x)
            (append (cdr x) y)))))

(def (append-to-tail! x y)
  (if (null? x)
    y
    (using (x :- :pair)
      (let loop (((a :- :pair) x) (b (cdr x)))
        (if (null? b)
          (begin (set-cdr! a y) x)
          (using (b :- :pair)
            (loop b (cdr b))))))))

(def (tree-copy x)
  (if (pair? x)
    (cons (tree-copy (car x))
          (tree-copy (cdr x)))
    x))

;;; n is # of symbols
;;; m is maximum amount of stuff on the plist
;;; npats is the number of basic patterns on the unit
;;; ipats is the instantiated copies of the patterns

(def *rand* 21)

(def (init (n : :fixnum) (m : :fixnum) (npats : :fixnum) (ipats : :list))
  (let (ipats (tree-copy ipats))
    (do ((p ipats (cdr p)))
        ((null? (cdr p)) (set-cdr! p ipats)))
    (do (((n :- :fixnum) n (fx- n 1))
         ((i :- :fixnum) m
          (cond
           ((fx= i 0) m)
           (else (fx- i 1))))
         (name (generate-symbol) (generate-symbol))
         (a []))
        ((fx= n 0) a)
      (set! a (cons name a))
      (do (((i :- :fixnum) i (fx- i 1)))
          ((fx= i 0))
        (put name (generate-symbol) #f))
      (put name
           'pattern
           (do (((i :- :fixnum) npats (fx- i 1))
                (ipats ipats (cdr ipats))
                (a []))
               ((fx= i 0) a)
             (set! a (cons (car ipats) a))))
      (do (((j :- :fixnum) (fx- m i) (fx- j 1)))
          ((fx= j 0))
        (put name (generate-symbol) #f)))))

(def (browse-random)
  => :fixnum
  (set! *rand* (fxremainder (* *rand* 17) 251))
  *rand*)

(def (randomize (l : :list))
  (do ((a []))
      ((null? l) a)
    (let (n (fxremainder (browse-random) (length l)))
      (cond
       ((fx= n 0)
        (set! a (cons (car (:- l :pair)) a))
        (set! l (cdr (:- l :pair)))
        l)
       (else
        (do (((n :- :fixnum) n (fx- n 1))
             ((x :- :pair) l (cdr x)))
            ((fx= n 1)
             (set! a (cons (car (cdr x)) a))
             (set-cdr! x (cdr (cdr x)))
             x)))))))

(def (my-match pat dat alist)
  (cond
   ((null? pat)
    (null? dat))
   ((null? dat) [])
   (else
    ;; NOTE: once we have union types, the following annotation will become unnecessary
    (using ((pat :- :pair) (dat :- :pair))
      (let ((pat-car (car pat))
            (pat-cdr (cdr pat))
            (dat-car (car dat))
            (dat-cdr (cdr dat)))
        (cond
         ((or (eq? pat-car '?)
              (eq? pat-car
                   dat-car))
          (my-match pat-cdr dat-cdr alist))
         ((eq? pat-car '*)
          (or (my-match pat-cdr dat alist)
              (my-match pat-cdr dat-cdr alist)
              (my-match pat dat-cdr alist)))
         ((not (pair? pat-car))
          (cond
           ((eq? (string-ref (symbol->string pat-car) 0)
                 #\?)
            (let ((val (lookup pat-car alist)))
              (cond
               (val
                ;; NOTE: annotation will become unnecessary with union types
                (using (val :- :pair)
                  (my-match (cons (cdr val)
                                  pat-cdr)
                            dat alist)))
               (else
                (my-match pat-cdr
                          dat-cdr
                          (cons (cons pat-car
                                      dat-car)
                                alist))))))
           ((eq? (string-ref (symbol->string pat-car) 0)
                 #\*)
            (let (val (lookup pat-car alist))
              (cond
               (val
                ;; NOTE: annotation will become unnecessary with union types
                (using (val :- :pair)
                  (my-match (append (cdr val)
                                    pat-cdr)
                            dat alist)))
               (else
                (do ((l []
                        (append-to-tail!
                         l
                         (cons (if (pair? d)
                                 (car d)
                                 [])
                               [])))
                     (e (cons [] dat) (cdr (:- e :pair)))
                     (d dat (if (pair? d) (cdr d) [])))
                    ((or (null? e)
                         (my-match pat-cdr
                                   d
                                   (cons
                                    (cons pat-car l)
                                    alist)))
                     (if (null? e) #f #t)))))))
           (else #f)))
         (else
          (and (pair? dat-car)
               (my-match pat-car
                         dat-car alist)
               (my-match pat-cdr
                         dat-cdr alist)))))))))



(def database
  (randomize
   (init 100 10 4 '((a a a b b b b a a a a a b b a a a)
                    (a a b b b b a a
                       (a a)(b b))
                    (a a a b (b a) b a b a)))))

(def (browse pats)
  (investigate
   database
   pats)
  (map string->number (map symbol->string database)))

(def (investigate units pats)
  (do ((units units (cdr (:- units :pair))))
      ((null? units))
    (do ((pats pats (cdr (:- pats :pair))))
        ((null? pats))
      (do ((p (get (car (:- units :pair)) 'pattern)
              (cdr (:- p :pair))))
          ((null? p))
        (my-match (car (:- pats :pair)) (car (:- p :pair)) [])))))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 "")
         (name "browse"))
    (run-r7rs-benchmark
     (string-append name ":" s2)
     count
     (lambda () (browse (hide count input1)))
     (lambda (result) (equal? result output)))))
