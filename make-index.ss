#!/usr/bin/env gxi

(import :std/iter
        :std/format
        :std/sort
        :std/misc/ports)

(def contestants
  '("Racket"
    "Gerbil"
    "Gerbil-unsafe-sep"
    "Gerbil-unsafe"))

(def top #<<END
<html>
  <head><title>R7RS Benchmarks</title></head>
<body>
  <h2>R7RS benchmarks for Gerbil v0.18-rc1 and Racket v8.2</h2>
  <p>This is based on ecraven's benchmarks, which seem unmatained.
  <p>So I run them for Gerbil v0.18-rc1, both safe and unsafe mode, and Racket v8.2 (this is what ubuntu installs).

  <p>The benchmarks were run on a Dell XPS 13-9320 laptop.
  <h2>Results</h2>
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
             (results (map filter-result-lines results)))
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
              (fprintf out "<td>~a</td>~n"  (car bench-names))
              (for ((result bench-results)
                    (color (result-colors bench-results)))
                (fprintf out "<td/>~n")
                (fprintf out "<td align=\"right\" style=\"background-color:~a\">~a</td>~n" color result))
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
  (let* ((indexed (map (lambda (r i) (cons (string->number r) i))
                       results
                       (iota (length results))))
         (sorted (sort indexed (lambda (a b) (< (car a) (car b)))))
         (colored (map (lambda (x c) (cons c (cdr x)))
                       sorted
                       '("lightgreen" "lightblue" "yellow" "orange")))
         (sorted (sort colored (lambda (a b) (< (cdr a) (cdr b))))))
    (map car sorted)))

(def (contestant-names contestants)
  (map (lambda (c)
         (case c
           (("Racket") "Racket v8.2")
           (("Gerbil") "Gerbil v0.18-rc1 safe/separate")
           (("Gerbil-unsafe-sep") "Gerbil v0.18-rc1 unsafe/separate")
           (("Gerbil-unsafe") "Gerbil v0.18-rc1 unsafe/fpo")
           (else
            (error "unknown contestatnt" c))))
       contestants))
