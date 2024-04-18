;;; RAY -- Ray-trace a simple scene with spheres, generating a ".pgm" file.
;;; Translated to Scheme from Paul Graham's book ANSI Common Lisp, Example 9.8

(defstruct Point ((x :- :flonum) (y :- :flonum) (z :- :flonum))
  constructor: :init!
  final: #t)

(defmethod {:init! Point}
  (lambda (self (x : :flonum) (y : :flonum) (z : :flonum))
    (set! self.x x)
    (set! self.y y)
    (set! self.z z)))

(defstruct Sphere (color (radius :- :flonum) (center :- Point))
  constructor: :init!
  final: #t)

(defmethod {:init! Sphere}
  (lambda (self color (radius : :flonum) (center : Point))
    (set! self.color color)
    (set! self.radius radius)
    (set! self.center center)))

(def (sq (x : :flonum))
  => :flonum
  (fl* x x))

(def (mag (x : :flonum) (y : :flonum) (z : :flonum))
  => :flonum
  (flsqrt (fl+ (sq x) (sq y) (sq z))))

(def (unit-vector (x : :flonum) (y : :flonum) (z : :flonum))
  => Point
  (let (d (mag x y z))
    (Point (fl/ x d) (fl/ y d) (fl/ z d))))

(def (distance (p1 : Point) (p2 : Point))
  => :flonum
  (mag (fl- p1.x p2.x)
       (fl- p1.y p2.y)
       (fl- p1.z p2.z)))

(def (minroot (a : :flonum) (b : :flonum) (c : :flonum))
  (if (flzero? a)
      (fl/ (fl- c) b)
      (let (disc (fl- (sq b) (fl* 4.0 a c)))
        (if (flnegative? disc)
            #f
            (let ((discrt (sqrt disc))
                  (minus-b (fl- b))
                  (two-a (fl* 2.0 a)))
              (flmin (fl/ (fl+ minus-b discrt) two-a)
                     (fl/ (fl- minus-b discrt) two-a)))))))

(def *world* [])

(def eye (Point 0.0 0.0 200.0))

(def (tracer pathname (res : :fixnum))
  (when (file-exists? pathname)
    (delete-file pathname))
  (call-with-output-file
      pathname
    (lambda (p)
      (let (extent (fx* res 100))
        (display "P2 " p)
        (write extent p)
        (display " " p)
        (write extent p)
        (display " 255" p)
        (newline p)
        (do (((y :- :fixnum) 0 (fx+ y 1)))
            ((fx= y extent))
          (do (((x :- :fixnum) 0 (fx+ x 1)))
              ((fx= x extent))
            (write (color-at
                    (+ -50.0 (fl/ (inexact x) (inexact res)))
                    (+ -50.0 (fl/ (inexact y) (inexact res))))
                   p)
            (newline p)))))))

(def (color-at (x : :flonum) (y : :flonum))
  (using (eye :- Point)
    (let (ray (unit-vector (fl- x eye.x)
                           (fl- y eye.y)
                           (fl- eye.z)))
      (exact (round (fl* (sendray eye ray) 255.0))))))

(def (sendray (pt : Point) (ray : Point))
  (let* ((x (first-hit pt ray))
         (s (vector-ref x 0))
         (int (vector-ref x 1)))
    (if s
        (* (lambert s int ray)
           (Sphere-color s))
        0.0)))

(def (first-hit (pt : Point) (ray : Point))
  (let loop ((lst *world*) (surface #f) (hit #f) ((dist :- :flonum) 1e308))
    (if (null? lst)
      (vector surface hit)
      (using ((lst :- :pair)
              (s (car lst) :- Sphere))
        (let (h (intersect s pt ray))
          (if h
            (using (h :- Point)
              (let (d (distance h pt))
                (if (fl< d dist)
                  (loop (cdr lst) s h d)
                  (loop (cdr lst) surface hit dist))))
            (loop (cdr lst) surface hit dist)))))))

(def (lambert (s : Sphere) (int : Point) (ray : Point))
  => :flonum
  (using (n (normal s int) :- Point)
    (flmax 0.0
           (fl+ (fl* ray.x n.x)
                (fl* ray.y n.y)
                (fl* ray.z n.z)))))


(def (defsphere (x : :flonum) (y : :flonum) (z : :flonum) (r : :flonum) c)
  (let (s (Sphere c r (Point x y z)))
    (set! *world* (cons s *world*))
    s))

(def (intersect (s : Sphere) (pt : Point) (ray : Point))
  (let (n (minroot
           (fl+ (sq s.center.x) (sq s.center.y) (sq s.center.z))
           (fl* 2.0
                (fl+ (fl* (fl- pt.x s.center.x) ray.x)
                     (fl* (fl- pt.y s.center.y) ray.y)
                     (fl* (fl- pt.z s.center.z) ray.z)))
           (fl+ (sq (fl- pt.x s.center.x))
                (sq (fl- pt.y s.center.y))
                (sq (fl- pt.z s.center.z))
                (fl- (sq s.radius)))))
    (if n
      (using (n :- :flonum)
        (Point (fl+ pt.x (fl* n ray.x))
               (fl+ pt.y (fl* n ray.y))
               (fl+ pt.z (fl* n ray.z))))
      #f)))

(def (normal (s : Sphere) (pt : Point))
  => Point
  (unit-vector (fl- s.center.x pt.x)
               (fl- s.center.y pt.y)
               (fl- s.center.z pt.z)))

(def (ray-test res output-file)
  (set! *world* '())
  (defsphere 0.0 -300.0 -1200.0 200.0 0.8)
  (defsphere -80.0 -150.0 -1200.0 200.0 0.7)
  (defsphere 70.0 -100.0 -1200.0 200.0 0.9)
  (do (((x :- :fixnum) -2 (fx+ x 1)))
      ((fx> x 2))
    (do (((z :- :fixnum) 2 (fx+ z 1)))
        ((fx> z 7))
      (defsphere
        (fl* (inexact x) 200.0)
        300.0
        (fl* (inexact z) -400.0)
        40.0
        0.75)))
  (tracer output-file res))

(def (run input output)
  (ray-test input output)
  'ok)

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "ray"))
    (run-r7rs-benchmark
     (string-append name ":" s2)
     count
     (lambda () (run (hide count input1) (hide count input2)))
     (lambda (result) (equal? result output)))))
