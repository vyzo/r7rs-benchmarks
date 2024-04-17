;;; TRIANGL -- Board game benchmark.

(def *board*
  (u8vector 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1))

(def *sequence*
  (u8vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(def *a*
  (u8vector 1 2 4 3 5 6 1 3 6 2 5 4 11 12
            13 7 8 4 4 7 11 8 12 13 6 10
            15 9 14 13 13 14 15 9 10
            6 6))

(def *b*
  (u8vector 2 4 7 5 8 9 3 6 10 5 9 8
            12 13 14 8 9 5 2 4 7 5 8
            9 3 6 10 5 9 8 12 13 14
            8 9 5 5))

(def *c*
  (u8vector 4 7 11 8 12 13 6 10 15 9 14 13
            13 14 15 9 10 6 1 2 4 3 5 6 1
            3 6 2 5 4 11 12 13 7 8 4 4))

(def *answer* [])

(defrule (u8-ref bits i)
  (:- (##u8vector-ref bits i) :fixnum))

(defrule (u8-set! bits i v)
  (##u8vector-set! bits i v))

(def (attempt (i : :fixnum) (depth : :fixnum))
  (cond
   ((fx= depth 14)
    (set! *answer*
      (cons (cdr (u8vector->list *sequence*)) *answer*))
    #t)
   ((and (fx= 1 (u8-ref *board* (u8-ref *a* i)))
         (fx= 1 (u8-ref *board* (u8-ref *b* i)))
         (fx= 0 (u8-ref *board* (u8-ref *c* i))))
    (u8-set! *board* (u8-ref *a* i) 0)
    (u8-set! *board* (u8-ref *b* i) 0)
    (u8-set! *board* (u8-ref *c* i) 1)
    (u8-set! *sequence* depth i)
    (do (((j :- :fixnum) 0 (fx+ j 1))
         ((depth :- :fixnum) (fx+ depth 1)))
        ((or (fx= j 36) (attempt j depth)) #f))
    (u8-set! *board* (u8-ref *a* i) 1)
    (u8-set! *board* (u8-ref *b* i) 1)
    (u8-set! *board* (u8-ref *c* i) 0)
    #f)
   (else #f)))

(def (test (i : :fixnum) (depth : :fixnum))
  (set! *answer* [])
  (attempt i depth)
  (car *answer*))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "triangl"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3)
     count
     (lambda () (test (hide count input1) (hide count input2)))
     (lambda (result) (equal? result output)))))
