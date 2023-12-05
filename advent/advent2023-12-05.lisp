;; with libraries:
;; ciel-user
;; or
;; ppcre, str, alexandria, serapeum's dict

(defparameter *file-input* "advent2023-12-05.txt")

(defparameter input "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defun split-paragraphs (input)
  (ppcre:split "\\n\\n" input))

(defun extract-line-numbers (line)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" line)))

(defun make-range (source dest n)
  ;; a plist: then access it with getf.
  (list :source source
        :source-start source
        :source-end (+ source (1- n))
        :dest dest
        :range n))

(defun line-to-map (line)
  (let ((numbers (extract-line-numbers line)))
    (make-range (second numbers) (first numbers) (third numbers))))
#+(or)
(line-to-map (second (str:lines (second (SPLIT-PARAGRAPHS input)))))

(defun parse-paragraph (par)
  "paragraph: name on first line, then lines of numbers."
  ;; to get the name: (first (ppcre:all-matches-as-strings "[a-z]*-to-[a-z]*" (first lines)))
  (mapcar #'line-to-map (rest (str:lines par))))

(defun range-lookup (i range)
  (when (<= (getf range :source-start) i (getf range :source-end))
    (+ (getf range :dest)
       (- i (getf range :source-start)))))

(defun ranges-lookp (i ranges)
  (loop for range in ranges
        for res = (range-lookup i range)
        if res
          return res
        finally (return i)))

;; also:
(defun %ranges-lookup (i ranges)
  (or (some (lambda (range)
              (range-lookup i range))
            ranges)
      i))

#+(or)
(progn
  (assert (equal 55 (ranges-lookp 53 (parse-paragraph (second (SPLIT-PARAGRAPHS input))))))
  (assert (equal 50 (ranges-lookp 98 (parse-paragraph (second (SPLIT-PARAGRAPHS input))))))
  )

(defun parse-maps (input)
  (let* ((paragraphs (split-paragraphs input))
         ;; a list of maps,
         ;; let's suppose they follow each other in the right order.
         (maps (make-list (length (rest paragraphs)) :initial-element (dict))))
    (loop for par in (rest paragraphs)
          for i from 0
          for map = (parse-paragraph par)
          do (setf (elt maps i) map)
             finally (return maps))))

(defun seed-location (seed maps)
  (loop for map in maps
        with next = seed
        do (setf next (ranges-lookp next map))
        finally (return next)))
#+(or)
(seed-location 13 (parse-maps input))  ;; 35

(defun lowest-location (input)
  (let* ((paragraphs (split-paragraphs input))
         (seeds (extract-line-numbers (first paragraphs)))
         (maps (parse-maps input)))
    (loop for seed in seeds
          minimize (seed-location seed maps))))

#+solve-it
(lowest-location (str:from-file *file-input*))
