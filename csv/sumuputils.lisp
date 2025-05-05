;;;;
;;;; Utilisation:
;;;; - télécharger https://github.com/ciel-lang/CIEL/
;;;; - mettre les fichiers .csv à côté du programme et le lancer:
;;;;
;;;; ./ciel Rapports*
;;;;
;;;;

(in-package :ciel-user)

(ql:quickload "data-table"  :silent t)

;;; Download the CSV file from Sum Up

(defvar *file*  "/path/to/Rapport-ventes-2024-11-01_2024-11-30.csv")

(defvar *DT* nil)

(defun parse-csv (file)
  "Parse CSV, return a data-table object with column names and rows."
  (let ((rows (csv:read-csv (str:from-file file))))
    (make-instance 'data-table:data-table :column-names (first rows) :rows (rest rows))))

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
          sum (parse-integer qty)))

(defun sum-quantities-offerts-for-day/by-type (day dt desc)
  ;; optionnel: pour chq jour, combien de tartines, alcool, soft… d'offerts? (en nombre)
  (loop for row in (rows-offerts-for-day day dt)
        for qty = (data-table:data-table-value dt :row row :col-name "Quantité")
        when (and qty
                  (str:containsp desc (data-table:data-table-value dt :row row :col-name "Description")))
          sum (parse-integer qty)))

;; (sum-quantities-offerts-for-day "23 nov")
;; 15

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
          sum (parse-float qty)))

;; (sum-total-offerts-for-day "23 nov")
;; 49

(defun report-totals-offert-for-days (file &key (stream t))
  (let ((dt (parse-csv file)))
    (format stream "~&~%Total TTC des offerts~&~%")
    (format stream "~&(somme des colonnes Prix (TTC) pour toutes les lignes du jour comportant 'conso offerte')~&")
    (loop for day in (get-all-days dt)
         do (format stream "~20a: ~4a~&" day (sum-total-offerts-for-day day dt)))))

(defun report-number-offert-for-days (file &key (stream t))
  (let ((dt (parse-csv file)))
    (format stream "~&Nombre des consos offertes par jour~&")
    (format stream "~&(somme des colonnes 'Quantité' pour toutes les lignes du jour comportant 'conso offerte')~&")
    (loop for day in (get-all-days dt)
         do (format stream "~20a: ~4a~&" day (sum-quantities-offerts-for-day day dt)))))

#+ciel
(progn
  (mapcar #'report-totals-offert-for-days (rest ciel-user:*script-args*))
  (mapcar #'report-sum-quantities-offerts-for-day/by-type (rest ciel-user:*script-args*))
)
