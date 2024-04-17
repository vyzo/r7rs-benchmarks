#!/usr/bin/env gxi

(import :std/iter
        :std/format
        :std/sort
        :std/misc/ports)

(def (main new old)
  (let ((new-results (select-best-run (filter-result-lines (read-file-lines new))))
        (old-results (select-best-run (filter-result-lines (read-file-lines old)))))
    (print-header new old)
    (let loop ((new-rest new-results)
               (old-rest old-results))
      (match new-rest
        ([new . new-rest]
         (match old-rest
           ([old . old-rest]
            (let ((name (and new (benchmark-name new)))
                  (new-result (and new (benchmark-result new)))
                  (old-result (and old (benchmark-result old))))
              (when (and name new-result old-result)
                (print-result name new-result old-result (- new-result old-result) (/ (- new-result old-result) old-result)))
              (loop new-rest old-rest)))))
        (else (void))))
    (print-footer)))

(def (print-header new old)
  (displayln "<html><head><title>Regression Analysis</title><link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"/></head><body>")
  (printf "<h2>Regression Analysis between ~a and ~a" new old)
  (printf "<table> <th><td>~a</td><td>~a</td><td>delta</td><td>pct</td></th>" new old))
(def (print-result name new old delta rdelta)
  (let (color (colored rdelta))
    (printf "<tr><td>~a</td> <td>~a</td> <td>~a</td> <td style=\"background-color:~a\">~1,3f</td> <td style=\"background-color:~a\">~a</td>~n" name new old color delta color (pct rdelta))))
(def (print-footer)
  (displayln "</table></body></html>"))

(def (pct v)
  (format "(~a~a%)" (if (negative? v) "" "+")(/ (floor (* 1000 v)) 10)))

(def (colored delta)
  (if (> delta 0)
    (cond
     ((< (abs delta) .01) "white")
     ((< (abs delta) .1)  "lightyellow")
     ((< (abs delta) .25) "yellow")
     ((< (abs delta) .5)  "orange")
     (else "orangered"))
    (cond
     ((< (abs delta) .01) "white")
     ((< (abs delta) .1)  "lightcyan")
     ((< (abs delta) .25) "lightgreen")
     ((< (abs delta) .5)  "lawngreen")
     (else "green"))))

(def (benchmark-name line)
  (car (string-split (list-ref (string-split line #\,) 1)
                     #\:)))

(def (benchmark-result line)
  (alet (result (and line (last (string-split line #\,))))
    (string->number result)))

(def (filter-result-lines lines)
  (filter (cut string-prefix? "+!CSVLINE!+" <>) lines))

(def (select-best-run lines)
  (let lp ((rest lines) (current #f) (best-time #f) (best-line #f) (result []))
    (match rest
      ([line . rest]
       (let ((bench-name (benchmark-name line))
             (bench-time (benchmark-result line)))
         (if (equal? current bench-name)
           (if bench-time
             (if (< bench-time best-time)
               (lp rest bench-name bench-time line result)
               (lp rest bench-name best-time best-line result))
             (lp rest bench-name bench-time line result))
           (if current
             (lp rest bench-name bench-time line (cons best-line result))
             (lp rest bench-name bench-time line result)))))
      (else
       (reverse (cons best-line result))))))
