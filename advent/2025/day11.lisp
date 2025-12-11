
(uiop:define-package :aoc-2025-11 (:use :cl
     :ciel  ;; for libraries: str, serapeum:dict and more useful ones.
          ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
          ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
          ;; See below.)
          ;; http://ciel-lang.org/
          )
  (:documentation "part1: recursive.

   part 2: there must be cycles, we don't detect them."))

(in-package :aoc-2025-11)

(defparameter *input* "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

(defun parse-input (input)
  (let ((nodes (list))
        (paths (dict)))
    (loop for line in (str:lines input)
          for parts = (str:split ":" line)
          for in = (first parts)
          for outputs = (str:words (second parts))
          do
             (push in nodes)
             (setf (gethash in paths) outputs)
          )
    (values paths nodes)))

#++
(defparameter *paths* (parse-input *input*))

(defun end-p (s)
  (equal s "out"))

#++
(defun end-p (s)
  (equal s "dac"))

;; (function-cache:defcached follow (paths &key (start "you") current-path)
;; nah, not for us here.

(defun follow (paths &key (start "you") (end "out") current-path)
  (loop for output in (gethash start paths)
        sum
           (cond
             ((equal end output)
              1)
             ((find output current-path :test #'equal)
              0)
             (t
              (follow paths :start output :end end :current-path (push output current-path))))))

#++
(log:config :warn)

(defun part1 (input)
  (follow (parse-input input)))

#++
(part1 *input*)

#+ciel
(part1 (str:from-file "day11.txt"))
;; 724 o/
;; 15ms

;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;

(defparameter *2ndinput* "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")

(defun reverse-paths (paths)
  (loop for key being the hash-key of paths
        for outputs = (gethash key paths)
        with reversed = (dict)
        do (loop for output in outputs
                 do (push key (gethash output reversed))
                    ;; (format t "~a: ~a~&" output key)
                 )
        finally (return reversed)))

#++
(defparameter *reversed-paths* (reverse-paths (parse-input *input*)))

#++
(follow *reversed-paths* :start "out" :end "you")
;; 5 OK

(defun follow/part2 (paths &key (start "svr") (end "out") current-path visited-dac-p visited-fft-p)
  (loop for output in (gethash start paths)
        sum
           ;; (log:info "with:" output current-path)
           (cond
             ((equal end output)
              (if (and visited-fft-p visited-dac-p)
                  (progn
                    (log:info "--> out: " current-path)
                    1)
                  0))
             ((find output current-path :test #'equal)
              (log:info "nope")
              0)
             (t
              ;; (log:info "continue…")
              (when (equal "fft" output)
                (setf visited-fft-p t))
              (when (equal "dac" output)
                (setf visited-dac-p t))
              (follow/part2 paths :start output :end end
                            :current-path (push output current-path)
                            :visited-dac-p visited-dac-p
                            :visited-fft-p visited-fft-p
                      )))))

(defun part2 (input)
  (follow/part2 (parse-input input)))

#++
(part2 *2ndinput*)
;; 2

#+ciel
(part2 (str:from-file "day11.txt"))
;; oops, many seconds! I stopped after 10.
;; but… why? Is the "svr" start point nasty?

;; Would that be that faster with FSET:SET ?

(defun follow/part2/sets (paths &key (start "svr")
                                  (current-path (fset:empty-set)) ;; empty set  <---------
                                  visited-dac-p visited-fft-p)
  "current-path: a fset:set"
  (loop for output in (gethash start paths)
        sum
           ;; (log:info "with:" output current-path)
           (cond
             ((end-p output)
              (if (and visited-fft-p visited-dac-p)
                  1
                  0))
             ((fset:contains? current-path output) ;; FSET:CONTAINS? <-------------
              0)
             (t
              (when (equal "fft" output)
                (setf visited-fft-p t))
              (when (equal "dac" output)
                (setf visited-dac-p t))
              (follow/part2/sets paths
                                 :start output
                            :current-path (fset:with current-path output) ;; add to SET <-----
                            :visited-dac-p visited-dac-p
                            :visited-fft-p visited-fft-p
                      )))))

#++
(follow/part2/sets (parse-input *2ndinput*))
;; 2

#+ciel
(follow/part2/sets (parse-input (str:from-file "day11.txt")))

;; with real input: > 10s
;; so should we learn about cycles and take shortcuts?

#|

;; what if we studied the paths fft -> out and dac -> out ?

(follow (parse-input *2ndinput*) :start "fft")
;; 4

(follow (parse-input *2ndinput*) :start "dac")
;; 2

;; real input:

(follow (parse-input (str:from-file "day11.txt")) :start "dac")
;; 3263
;; a few ms

(follow (parse-input (str:from-file "day11.txt")) :start "fft")
;; too long. Is there a cycle?

|#

;; reversed?


;; (defun follow/part2 (paths &key (start "svr") (end "out") current-path visited-dac-p visited-fft-p)
(function-cache:defcached follow/part2 (paths &key (start "svr") (end "out") current-path visited-dac-p visited-fft-p)
  (if (equal start end)
      (if (and visited-fft-p visited-dac-p)
                  (progn
                    (log:info "--> out: " current-path)
                    1)
                  0)
      (loop for output in (gethash start paths)
        sum
           ;; (log:info "with:" output current-path)
           (cond
             ;; ((equal end output)
             ;;  (if (and visited-fft-p visited-dac-p)
             ;;      (progn
             ;;        (log:info "--> out: " current-path)
             ;;        1)
             ;;      0)
             ;;  )
             ((find output current-path :test #'equal)
              (log:info "nope")
              0)
             (t
              ;; (log:info "continue…")
              (when (equal "fft" output)
                (setf visited-fft-p t))
              (when (equal "dac" output)
                (setf visited-dac-p t))
              (follow/part2 paths :start output :end end
                            :current-path (push output current-path)
                            :visited-dac-p visited-dac-p
                            :visited-fft-p visited-fft-p
                      )))))
  )
