;; A download command that calls to youtube-dl (by default).
;;
;; (code I first wrote for the Next browser)

(declaim (type (or null string) *download-program*))
(defparameter *download-program* "youtube-dl"
  "The external program to download videos with. Defaults to youtube-dl.")

(declaim (type (or null list-of-strings) *download-args*))
(defparameter *download-args* nil
  "Default arguments for the download command as a list of strings. See also `download-arguments' which adds more.")

(declaim (type (or null trivial-types:proper-list) *preferred-download-directories*))
(defparameter *preferred-download-directories* (list download-manager::*default-download-directory*)
  "List of favorite directories to save videos to. If it contains more than one entry, we are asked for the destination.")


(defun download-arguments (url target-dir)
  "Return a list of arguments for the download command.

Appends `*download-args' and -o /target/directory/%(title)s.%(ext)s (for youtube-dl)."
  (declare (ignorable url))
  (append *download-args* (list "-o" (format nil "~a/%(title)s.%(ext)s" target-dir))))

(defun resolve-download-directory (target-dir)
  (or target-dir
      (first *preferred-download-directories*)
      download-manager::*default-download-directory*))

(defun download-command (url &optional target-dir)
  "Return a list of strings composing the full download command, ready to feed to uiop:launch-program."
  (let ((target-dir (resolve-download-directory target-dir)))
    (setf target-dir (string-right-trim (list #\/) (namestring target-dir)))
    (append (list *download-program*
                  url)
            (download-arguments url target-dir))))

(defun download (url &optional target-dir)
  "Download asynchronously and notify on success or failure."
  (handler-case
      (progn
        (unless target-dir
          (setf target-dir (resolve-download-directory target-dir)))
        (echo "Starting download of ~a to ~a." url target-dir)
        (log:info "Starting download of ~a to ~a." url target-dir)
        ;XXX notify progress.
        (utils:launch-and-notify (download-command url target-dir)
                                 :success-msg (format nil "Video downloaded to ~a." target-dir)
                                 :error-msg (format nil "Failed to download video.~&")))
    (error (c)
      (log:warn "Error downloading ~a to ~a: ~a" url target-dir c))))

(defparameter *download-function* #'download
  "Default download function.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-from-minibuffer (buffer)
  ;TODO: use fzf?
  (error 'TODO))

(defun download-video ()
  "Download the video of the current URL with an external program."
  (let* ((url (url (current-buffer))))
    (cond
      ((null *preferred-download-directories*)
       ;XXX: ask destination.
       (funcall *download-function* url))
      ((= 1 (length *preferred-download-directories*))
       (funcall *download-function* url (first *preferred-download-directories*)))
      (t
       (target-dir (read-from-minibuffer ;; ask user input with options completion.
                    (make-minibuffer
                     :input-prompt "Target directory"
                     :completion-function
                     (lambda (input)
                       (file-completion-function input *preferred-download-directories*)))))
         (funcall *download-function* url target-dir)))))
