
;; <2025-12-13> On Discord by Zyd.
;; https://github.com/sharplispers/clx
;; https://sharplispers.github.io/clx/

#+(or)
(ql:quickload :clx)

(defpackage #:w
  (:use #:cl)
  (:documentation "Example on how to list all open X windows on our desktop."))

(in-package #:w)

(defvar *noise*
  '("i3-WM_Sn" "i3-frame" "xscreensaver" "Dunst" "picom" "udiskie"
    "xdg-desktop-portal-gtk" "pavucontrol" "Qt Selection Owner for keepassxc"
    "Qt Clipboard Requestor Window"))

(defun noise-p (window)
  (or (null (xlib:wm-name window))
      (member (xlib:wm-name window) *noise* :test #'string=)
      (member (xlib:get-wm-class window) *noise* :test #'string=)))

(defun clean (windows)
  (remove-if #'noise-p windows))

(defun windows (display)
  (xlib:query-tree
   (xlib:screen-root
    (xlib:display-default-screen display))))

(defmacro with-default-display (display &body body)
  `(let ((,display (xlib:open-default-display)))
     (unwind-protect
          (progn ,@body)
       (xlib:close-display ,display))))

;;; Example:

(defun list-windows ()
  (with-default-display display
    (mapcar (lambda (window)
              `(:class ,(xlib:get-wm-class window)
                :name ,(xlib:wm-name window)
                :obj ,window))
            (clean (windows display)))))

;; (list-windows)
;; =>
;; ((:CLASS "emacs" :NAME "emacs" :OBJ #<XLIB:WINDOW :0 1800001>)
;;  (:CLASS NIL :NAME "KeePassXC" :OBJ #<XLIB:WINDOW :0 1E00008>)
;;  (:CLASS "org.mozilla.firefox" :NAME "Firefox" :OBJ #<XLIB:WINDOW :0 1A00001>)
;;  (:CLASS NIL :NAME "Firefox" :OBJ #<XLIB:WINDOW :0 1A00011>)
;;  (:CLASS "org.mozilla.firefox" :NAME "Firefox" :OBJ #<XLIB:WINDOW :0 1A0002E>))
