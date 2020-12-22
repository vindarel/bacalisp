;;; http://40ants.com/lisp-project-of-the-day/2020/05/0059-cl-ncurses.html
;;; Run with
;;; sbcl --load ncurses.lisp

(ql:quickload :cl-ncurses)

(defpackage :curse
  (:use :cl
        :cl-ncurses)
  (:shadowing-import-from :cl-ncurses
                          :timeout))

(in-package :curse)

(format t "loaded libs: ~a" uffi::*loaded-libraries*)
(force-output)

(defun main (&rest argv)
  "This example is based on:
   https://www.tldp.org/HOWTO/NCURSES-Programming-HOWTO/windows.html"
  (declare (ignorable argv))
  (uffi:load-foreign-library "/lib/x86_64-linux-gnu/libncurses.so.5.9"
    :module "cl-ncurses")
  (initscr)
  (start-color)
  (init-pair 1
             color_red
             color_black)

  (let ((text "Hello Lisp World!"))
    (multiple-value-bind (screen-height screen-width)
        (get-maxyx *stdscr*)
      (let* ((text-len (length text))
             (box-width (+ text-len 4))
             (box-height 5)
             (box-x (round (/ (- screen-width box-width)
                              2)))
             (box-y (round (/ (- screen-height box-height)
                              2)))
             (plus (char-code #\+))
             (h-line (char-code #\-))
             (v-line (char-code #\|)))

        (attron (color-pair 1))

        ;; It's time to print a text in the center of the screen:
        (mvprintw (+ box-y 2) (+ box-x 2) text)

        ;; Now we'll draw a border around the box.
        ;; Horizontal lines:
        (mvhline box-y box-x
                 h-line box-width)
        (mvhline (+ box-y (1- box-height)) box-x
                 h-line box-width)
        ;; Vertical lines:
        (mvvline box-y box-x
                 v-line box-height)
        (mvvline box-y (+ box-x (1- box-width))
                 v-line box-height)
        ;; And corners
        (mvaddch box-y box-x
                 plus)
        (mvaddch box-y (+ box-x (1- box-width))
                 plus)
        (mvaddch (+ box-y (1- box-height))
                 (+ box-x (1- box-width))
                 plus)
        (mvaddch (+ box-y (1- box-height)) box-x
                 plus)
        (move 0 0))))

  ;; Wait for key press
  (getch)
  (endwin))

(main)
