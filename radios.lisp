;; @ignore
;; (ql:quickload '(:lquery :access :cl-punch :cl-markup))
;; @end ignore

;; Fetch info from online radios.
;;
;; usage: `(fip-save-current-song)` in the REPL. It saves the artist, song, published year and timestamp into ~/.config/fip.txt

;; @subsubsection FIP radio

;; The @link{https://www.fip.fr/}{fip radio} is great: eclectic music without any ads.
;; We developed the following commands:
;; - fip-save-current-song to save the currently playing song to disk
;; - fip-view-saved-songs to show them all in a new buffer (with a link to a youtube search)
;; - fip-listen-saved-song to fuzzy-select a song and search it on youtube (where we can use `download-video`).

;; Save the current playing song's title into a text file.
;; Caution: it seems the website can lag to a couple minutes behind the music :S At least we also save the current time, so we could go back search for the title.

;; @ignore
;; (erudite:erudite #p"radios.md" "radios.lisp" :output-type :markdown :syntax :erudite)
;; @end ignore

(in-package :cl-user)

(defparameter *fip-home-url* "https://www.fip.fr/")

(defparameter *fip-database-file* (ensure-file-exists #p "~/.config/fip.txt")
              "Where to save our songs.")

;; Quickloading a library adds an extra startup time.
;; At some point we'll want to create a Next binary with those libraries.
;; @ignore
;; (ql:quickload '("lquery"
;;                 ;; access is a library for consistent and nested access to all data structures.
;;                 "access"
;;                 "cl-punch"
;;                 ))
;; @end ignore

(defun echo (s &key (stream t))
  (format stream s))

(defun fip-current-song ()
  "Return the artist, the song, the year (multiple values)."
  (handler-case
      (let* ((html (dex:get *fip-home-url*))
             (parsed (lquery:$ (initialize html)))
             (title (access:access ;; get first elt of vector, don't fail if out of bounds.
                     (lquery:$ parsed ".now-info-title" (text)) ; returns a vector.
                     0))
             (artist/year (access:access
                           (lquery:$ parsed ".now-info-subtitle" (text))
                           0))
             (matches (when artist/year
                        ;; returns a vector with the matched strings.
                        ;; see the Cookbook.
                        (nth-value 1 (ppcre:scan-to-strings "\(.*\) \((.*\))" artist/year))))
             (artist (when matches
                       (access:access matches
                                      0)))
             (year (when matches
                     (access:access matches
                                    1))))
        (values artist title year))
    (error (c)
      (format t "Getting FIP current song failed: ~a~&" c))))

(defun fip-save-current-song ()
  "Save the current song title, artist and year of the album playing on fip on file."
  (multiple-value-bind (artist title year)
      (fip-current-song)
    (if artist
        (progn
          (with-open-file (f *fip-database-file*
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :append)
            (let ((*print-pretty* nil))
              ;; Pretty printing will insert a line break at around 80 characters,
              ;; between :date and the date, making it un-readable with uiop:read-file-forms.
              (format f "~s~&" (list :artist artist :song title :year year
                                     :date (local-time:format-timestring nil (local-time:now))))))
          (format t "Saved: ~s~&" (list artist " - " title)))
        (echo "fip: no artist found."))))

(defun song-alist2html (alist)
  "From an alist with :artist, :title and all, return html."
  (format nil "~a, ~a ~a"
          (access:access alist :artist)
          (access:access alist :song)
          (access:access alist :year)))

(defun list2html (list)
  (cl-markup:markup
   (:head)
   (:body
    (:h1 "fip songs")
    (:ul
     (loop for elt in list
        collect (cl-markup:markup (:li (:span (song-alist2html elt))
                                       (:span "  "
                                              (:a :href (format nil "https://www.youtube.com/results?search_query=~a+~a"
                                                                (access:access elt :artist)
                                                                (access:access elt :song))
                                                  "youtube ⮱")))))))))

(defun list2txt (list)
  (loop for item in list
     do (format t "‣ ~va - ~va (~a) ~a~&"
                20
                (access:access item :artist)
                20
                (access:access item :song)
                (access:access item :year)
                (access:access item :date))
       ;TODO: escape spaces
     do (format t "~t ↪ https://www.youtube.com/results?search_query=~a+~a~&"
                (access:access item :artist)
                (access:access item :song))))

(defun fip-view-saved-songs ()
  "Print the fip songs we saved, along with a youtube search link."
  (let ((forms (uiop:read-file-forms *fip-database-file*)))
     (list2txt forms)))

(defun fip-print-saved-songs ()
  "Print the fip songs we saved."
  (handler-case
      (let ((forms (uiop:read-file-forms *fip-database-file*)))
        ;XXX: table output with vindarel/cl-ansi-term ?
        (mapcar (lambda (it)
                  (format t "~a - ~a ~a~&" (second it) (fourth it) (sixth it)))
                forms))
    (error (c)
      (format t "Error reading our fip songs file: ~a~&" c))))

;; (define-command fip-listen-saved-song ()
;;   "From my Next browser config. It uses its nice completion engine."
;;   "Pick one song that was saved on disk, and search it on Youtube."
;;   (with-result (selection (read-from-minibuffer
;;                            (make-instance 'minibuffer
;;                                           :default-modes '(minibuffer-mode)
;;                                           :input-prompt "pick a song"
;;                                           :completion-function #'songs-completion-filter)))

;;     (format t "~&choice: ~a~&" selection)
;;     (set-url-to-buffer (format nil  "https://www.youtube.com/results?search_query=~a" selection)
;;                        :new-buffer-p t)) )

;; (defun dl (&optional video-url)
;;   "Download video with youtube-dl in the currently open buffer or optional video-url."
;;   ;TODO:
;;   (media-download:download
;;    (list "youtube-dl" u
;;          "-o" (format nil "~a/%(title)s.%(ext)s" "/home/noloop/common-lisp/")
;;          "--embed-subs")))
