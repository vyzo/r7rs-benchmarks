#!/usr/bin/env gxi

(import :std/iter
        :std/format
        :std/sort
        :std/misc/ports)

(def contestants
  '("Gerbil-shared"
    "Gerbil-fpo"
    "Gerbil-unsafe-shared"
    "Gerbil-unsafe-fpo"
    "GambitC"
    "Racket"))

(def top #<<END
<html>
<head>
<title>R7RS Benchmarks for Gerbil</title>
<link rel="stylesheet" type="text/css" href="style.css" />
</head>
<body>
  <h2>R7RS benchmarks for Gerbil</h2>
  <p>These are the results for v0.18.2 pre-release, Gambit v4.9.5-125, and Racket v8.10</h2>
  <p>This is based on ecraven's benchmarks, which seem unmaintained.

  <p>The benchmarks were run on a Dell XPS 13-9320 laptop.
  <p>Each benchmark was run 3 times, and I kept the best value.

  <b>Note:</b> See also the results for <a href="index-v0-18.html">Gerbil v0.18</a>.

  <h2>Results</h2>
  <table>
  <tr><td> <b>Color Coding:</b> </td>
      <td style="background-color:lawngreen">best</td>
      <td style="background-color:lightgreen">within 10%</td>
      <td style="background-color:lightcyan">within 10-25%</td>
      <td style="background-color:lightyellow">within 25-50%</td>
      <td style="background-color:yellow">within 50-100%</td>
      <td style="background-color:orange">within 100-200%</td>
      <td style="background-color:orangered">over 200%</td>
  </tr>
  </table>
  <br/>
END
)

(def bottom #<<END
</body>
</html>
END
)

(def (main)
  (call-with-output-file "index.html"
    (lambda (out)
      (display top out)
      (display "<table>\n" out)
      (display "<tr>\n" out)
      (display "<th>Benchmark</th>\n" out)
      (for (c (contestant-names contestants))
        (fprintf out "<th align=\"center\">⋄⋄⋄</th>")
        (fprintf out "<th align=\"center\">~a</th>~n" c)
        )
      (display "</tr>\n" out)
      (let* ((results (map (lambda (c) (read-file-lines (string-append "results." c))) contestants))
             (results (map filter-result-lines results))
             (results (map select-best-run results)))
        (unless (apply = (map length results))
          (error "Misaligned results; length mismatch"))
        (let lp ((rest results))
          (unless (ormap null? rest)
            (let* ((next (map car rest))
                   (bench-names (map benchmark-name next))
                   (bench-results (map benchmark-result next)))
              (unless (apply string=? bench-names)
                (error "Misaligned results; benchmark name mismatch" bench-names))
              (display "<tr>\n" out)
              (fprintf out "<td><b>~a</b></td>~n"  (car bench-names))
              (for ((result bench-results)
                    (color (result-colors bench-results))
                    (delta (result-deltas bench-results)))
                (fprintf out "<td/>~n")
                (fprintf out "<td align=\"right\" style=\"background-color:~a\">~a ~a</td>~n" color result delta))
              (display "</tr>\n" out)
              (lp (map cdr rest))))))
      (display "</table>\n" out)
      (display bottom out))))

(def (filter-result-lines lines)
  (filter (cut string-prefix? "+!CSVLINE!+" <>) lines))

(def (benchmark-name line)
  (car (string-split (list-ref (string-split line #\,) 1)
                     #\:)))

(def (benchmark-result line)
  (last (string-split line #\,)))

(def (result-colors results)
  (let* ((indexed  (map (lambda (r i) (cons (string->number r) i))
                        results
                        (iota (length results))))
         (sorted  (sort indexed (lambda (a b) (< (car a) (car b)))))
         (best    (caar sorted))
         (colored (map (lambda (x)
                         (cons
                          (let (delta (/ (- (car x) best) best))
                            (cond
                             ((zero? delta) "lawngreen")
                             ((< delta .1) "lightgreen")
                             ((< delta .25)  "lightcyan")
                             ((< delta .5) "lightyellow")
                             ((< delta 1)  "yellow")
                             ((< delta 2)   "orange")
                             (else          "orangered")))
                          (cdr x)))
                       sorted))
         (sorted (sort colored (lambda (a b) (< (cdr a) (cdr b))))))
    (map car sorted)))

(def (result-deltas results)
  (let* ((timings (map string->number results))
         (best    (car (sort timings <))))
    (map (lambda (t)
           (if (> t best)
             (format "(+~a%)" (/ (floor (* 1000 (/ (- t best) best))) 10))
             "↻"))
         timings)))

(def (contestant-names contestants)
  (map (lambda (c)
         (case c
           (("Racket") "Racket")
           (("GambitC") "Gambit (baseline)")
           (("Gerbil-shared") "Gerbil safe/shared")
           (("Gerbil-fpo") "Gerbil safe/fpo")
           (("Gerbil-unsafe-shared") "Gerbil unsafe/shared")
           (("Gerbil-unsafe-fpo") "Gerbil unsafe/fpo")
           (else
            (error "unknown contestatnt" c))))
       contestants))

(def (select-best-run lines)
  (let lp ((rest lines) (current #f) (best-time #f) (best-line #f) (result []))
    (match rest
      ([line . rest]
       (let ((bench-name (benchmark-name line))
             (bench-time (string->number (benchmark-result line))))
         (if (equal? current bench-name)
           (if (< bench-time best-time)
             (lp rest bench-name bench-time line result)
             (lp rest bench-name best-time best-line result))
           (if current
             (lp rest bench-name bench-time line (cons best-line result))
             (lp rest bench-name bench-time line result)))))
      (else
       (reverse (cons best-line result))))))
