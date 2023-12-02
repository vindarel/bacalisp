

(defparameter *file-input* "2023-12-02.txt")

(defparameter input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  "test input")

(defparameter *game* (dict :id 1
                           :sets '((:blue 3 :red 4)
                                   (:red 1 :green 2 :blue 6)
                                   (:green 2))))

(defparameter max-blue 14)
(defparameter max-red 12)
(defparameter max-green 13)
(defun max-color (color)
  (case color
    (:blue max-blue)
    (:red max-red)
    (:green max-green)
    (t (error "invalid color: ~a" color))))

(defun extract-number (s)
  (parse-integer s :junk-allowed t))
#+(or)
(extract-number " 8 green")

(defun extract-color (s)
  "s: 8 green"
  (cond
    ((str:containsp "green" s)
     :green)
    ((str:containsp "blue" s)
     :blue)
    ((str:containsp "red" s)
     :red)
    (t (error "can't find a color in string: ~a" s))))

#+(or)
(extract-color " 8 green")

(defun extract-game-id (s)
  (parse-integer (str:replace-all "Game" ""
                                  (first (str:split ":" s)))))

(defun extract-sets-strings (s)
  (str:split ";"
             (last-elt (str:split ":" s))))
#+(or)
(EXTRACT-SETS-STRINGS  "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")
;; (" 8 green, 6 blue, 20 red" " 5 blue, 4 red, 13 green" " 5 green, 1 red")

(defun extract-set-data (s)
  (loop for pair in (str:split "," s)
        collect (list (extract-color pair)
                      (extract-number pair))))
#+(or)
(extract-set-data " 8 green, 6 blue, 20 red")
;; ((:GREEN 8) (:BLUE 6) (:RED 20))

(defun get-all-sets (s)
  (loop for set in (extract-sets-strings s)
        collect (flatten (extract-set-data set))))
#+(or)
(get-all-sets "1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
;; ((:BLUE 1 :GREEN 2) (:GREEN 3 :BLUE 4 :RED 1) (:GREEN 1 :BLUE 1))


(defun make-game-from-line (line)
  (let ((id (extract-game-id line))
        (sets/strings line))
    (dict :id id
          :sets (get-all-sets sets/strings))))


(defun color-nb (color plist)
  (or (getf plist color) 0))

(defun possible-color-p (color plist)
  (<= (color-nb color plist) (max-color color)))

(defun possible-set-p (plist)
  (loop for color in '(:red :blue :green)
        always (possible-color-p color plist)))

(defun possible-game-p (game)
  (loop for set in (gethash :sets game)
        always (possible-set-p set)))
#+(or)
(progn
  (possible-game-p (make-game-from-line (third (str:lines input))))
  (possible-game-p (make-game-from-line (first (str:lines input))))
  )

(defun possible-input-p (input)
  (loop for line in (str:lines input)
        for game = (make-game-from-line line)
        for foo = (print game)
        if (possible-game-p game)
          sum (gethash :id game)))

#+(or)
(possible-input-p input)
;; 8

;; Ensure the right parameters.
#+(go-for-it)
(possible-input-p (str:from-file *file-input*))

;; YEP! o/
