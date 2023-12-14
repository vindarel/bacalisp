
(defpackage :pstest
  (:use :cl :parenscript :paren6))

(in-package :pstest)


;; const numberButtons = document.querySelectorAll("[data-number]");
;; const operationButtons = document.querySelectorAll("[data-operation]");
;; const equalsButton = document.querySelector("[data-equals]");
;; const deleteButton = document.querySelector("[data-delete]");
;; const allClearButton = document.querySelector("[data-all-clear]");
;; const previousOperandTextElement = document.querySelector("[data-previous-operand]");
;; const currentOperandTextElement = document.querySelector("[data-current-operand]");

;; OK this works.
;; But now, how can we write our logic with all CL libraries?
;; looks like we need JS -> lisp process communication…
;; or bindings to C++.
;; OR with their webview
;; https://gitlab.com/sciter-engine/sciter-webview
;; But how can we run it? Not with scapp.

(defun main ()
  (ps

    (console.log "loading…")

    (defmacro select (elt)
      `(|document.querySelector| ,elt))

    (defmacro select-all (elt)
      `(|document.querySelectorAll| ,elt))

    (defparameter number-buttons (select-all "[data-number]"))
    (defparameter operation-buttons (select-all "[data-operation]"))

    ;; watch out, sometimes All sometimes not ;)
    (defparameter equals-button (select "[data-equals]"))

    (equals-button.add-event-listener
     "click"
     (lambda ()
       (console.log "-- equal clicked")
       ;; (calculator.clear)
       ;; (calculator.display)
       (values)))

    (console.log "started!")

    ))

#+(or)
;; C-c C-J on it:
(main)

#+(or)
(str:to-file "script.js" (main))
