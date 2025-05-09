;;;;
;;;; Utilisation:
;;;; - télécharger https://github.com/ciel-lang/CIEL/
;;;; - mettre les fichiers .csv à côté du programme et le lancer:
;;;;
;;;; Run this script on many CSV files:
;;;;
;;;; $ ciel sumuputils.lisp Rapports*
;;;;
;;;;

(in-package :ciel-user)

;; Needs the latest CIEL, or this library.
;; (ql:quickload "data-table"  :silent t)

;;; Download the CSV file from Sum Up
(defvar *file*  #p"/path/to/Rapport-ventes-2024-11-01_2024-11-30.csv"
  "Only for testing.")

(defvar *dt* nil "devel only")

(defun parse-csv (file)
  "Parse CSV, return a data-table object with column names and rows.

  file: a pathname (not just a string)."
  ;; This takes headers as the first row
  ;; and guesses the columns' types (string, int, float).
  (csv:get-data-table-from-csv (pathname file)))

(defun get-all-days (dt)
  (remove-duplicates
   (loop for row in (data-table:rows dt)
         for date/time = (data-table:data-table-value dt :row row :col-name "Date")
         for day = (str:unwords
                    (split-sequence #\Space date/time :count 3))
         collect day)
   :test #'equal))

;; (get-all-days (parse-csv *FILE*))
;; ("1 nov. 2024" "2 nov. 2024" "5 nov. 2024" "7 nov. 2024" "8 nov. 2024" …)

(defun get-all-conso-types (dt)
  (sort
   (remove-duplicates
    (loop for row in (data-table:rows dt)
          for description = (data-table:data-table-value dt :row row :col-name "Description")
          when (str:non-blank-string-p description)
            collect description)
    :test #'equal)
   #'string<))

(defun get-all-offert-types (types)
  (filter (^ (it) (str:containsp "offert" it)) types))
#++
(get-all-offert-types (get-all-conso-types (parse-csv *FILE*)))

(defun rows-offerts-for-day (day dt)
  "day: string, like '23 nov"
  (loop for row in (data-table:rows dt)
        when (and (str:containsp day (data-table:data-table-value dt :row row :col-name "Date"))
                  (str:containsp "offert" (data-table:data-table-value dt :row row :col-name "Description")))
          collect row))

(defun sum-quantities-offerts-for-day (day dt)
  ;; optionnel: pour chq jour, combien de tartines, alcool, soft… d'offerts? (en nombre)
  (loop for row in (rows-offerts-for-day day dt)
        for qty = (data-table:data-table-value dt :row row :col-name "Quantité")
        when qty
          sum qty))

;; (sum-quantities-offerts-for-day "23 nov" *dt*)
;; 15

(defun sum-quantities-offerts-for-day/by-type (day dt desc)
  ;; optionnel: pour chq jour, combien de tartines, alcool, soft… d'offerts? (en nombre)
  (loop for row in (rows-offerts-for-day day dt)
        for qty = (data-table:data-table-value dt :row row :col-name "Quantité")
        when (and qty
                  (str:containsp desc (data-table:data-table-value dt :row row :col-name "Description")))
          sum qty))

(defun report-sum-quantities-offerts-for-day/by-type (file &key (stream t) &aux dt)
  (setf dt (parse-csv file))
  (loop for day in (get-all-days dt)
        do
           (format stream "~&~%Nombre de consos offertes le ~a~&~%" day)
           (loop for desc in (get-all-offert-types (get-all-conso-types dt))
                 do
                    (format stream "~a: ~a~&" desc (sum-quantities-offerts-for-day/by-type day dt desc)))))


(defun sum-total-offerts-for-day (day dt)
  ;; le + important
  (loop for row in (rows-offerts-for-day day dt)
        for qty = (data-table:data-table-value dt :row row :col-name "Prix (TTC)")
        when qty
          sum qty))

;; (sum-total-offerts-for-day "23 nov" *dt*)
;; 49.0d0

(defun report-totals-offert-for-days (file &key (stream t))
  (let ((dt (parse-csv file)))
    (format stream "~&~%Total TTC des offerts~&~%")
    (format stream "~&(somme des colonnes Prix (TTC) pour toutes les lignes du jour comportant 'conso offerte')~&")
    (loop for day in (get-all-days dt)
         do (format stream "~20a: ~4f~&" day (sum-total-offerts-for-day day dt)))))

(defun report-number-offert-for-days (file &key (stream t))
  (let ((dt (parse-csv file)))
    (format stream "~&Nombre des consos offertes par jour~&")
    (format stream "~&(somme des colonnes 'Quantité' pour toutes les lignes du jour comportant 'conso offerte')~&")
    (loop for day in (get-all-days dt)
         do (format stream "~20a: ~4a~&" day (sum-quantities-offerts-for-day day dt)))))

#+ciel
(let ((files (mapcar #'pathname (rest ciel-user:*script-args*))))
  (mapcar #'report-totals-offert-for-days files)
  (mapcar #'report-sum-quantities-offerts-for-day/by-type files))
