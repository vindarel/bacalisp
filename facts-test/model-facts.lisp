#+nil
(ql:quickload :facts)

;; Try facts:
;; - add 3000 facts
;; - get all ISBNs, get all cards whose quantity is > 0,
;; - compare speed with SQL or raw loop.
;;
;;;TODO: try complicated queries.

;; In ABStock project.

#|
(length *cards*)
3062
|#

(defun load-cards-to-facts ()
  ;; discard output. facts:add only prints one symbol, but still too much.
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (time (loop for card in *cards*
             do (facts:add (?card :is-a :card
                                  :title (getf card :|title|)
                                  :id (getf card :|id|)
                                  :quantity (getf card :|quantity|)
                                  :repr (getf card :|repr|)
                                  :repr2 (getf card :|repr2|)
                                  :isbn (getf card :|isbn|)
                                  :price (getf card :|price|)
                                  :author (getf card :|author|)
                                  :publisher (getf card :|publisher|)
                                  :shelf (getf card :|shelf|)
                                  :shelf_id (getf card :|shelf_id|)))))))
#|
Evaluation took:
  110.031 seconds of real time => longer on the server
  109.842534 seconds of total run time (109.815457 user, 0.027077 system)
  [ Run times consist of 0.046 seconds GC time, and 109.797 seconds non-GC time. ]
  99.83% CPU
  285,430,506,254 processor cycles
  24 page faults
  20,639,664 bytes consed

NIL
|#

;; Required:
#+nil
(setf *print-length* 20)

;;
;; Collect and return all ISBNs.
;;

;; Facts
(time
 (facts:collect ((?card :is-a :card :isbn ?isbn))
   ?isbn))

#|
Evaluation took:
  2.434 seconds of real time
  2.433631 seconds of total run time (2.433631 user, 0.000000 system)
  100.00% CPU
  6,313,020,261 processor cycles
  720,704 bytes consed

("9791090724846" "9782353481811" "9782353481859" "9782362200625" "9782352894599" "9782352901952" "9788831312103" "9782362663376"
 "9782352902379" "9782889085057" "9782362663659" "9782367342139"
 "9782361392161" "9791092011944" "9791092011760" "9782379410130"
 "9782377560639" "9782897195588" "9788831312134" "9788831312127" ...)

|#

;; SQL

(time (let* ((query (dbi:prepare *connection*
                                 "SELECT isbn FROM search_card;"))
             (query (dbi:execute query)))
        (dbi:fetch-all query)))

#|
Evaluation took:
  0.007 seconds of real time
  0.007371 seconds of total run time (0.007371 user, 0.000000 system)
  100.00% CPU
  19,142,089 processor cycles
  687,648 bytes consed

((:|isbn| "9782021418019") (:|isbn| "9782072862106") (:|isbn| "9782072729331")
 (:|isbn| "9782072753800") (:|isbn| "9782072866586") (:|isbn| "9782226443885")
 (:|isbn| "9782889277445") (:|isbn| "9782889277391") (:|isbn| "9782072849787")
 (:|isbn| "9782710389668") (:|isbn| "9782882502414") (:|isbn| "9791095066316")
 (:|isbn| "9782364683976") (:|isbn| "9782757877128") (:|isbn| "9782253906568")
 (:|isbn| "9782221240861") (:|isbn| "9782374251981") (:|isbn| "9782917141496")
 (:|isbn| "9782917817506") (:|isbn| "9782369141150") ...)

|#

;;
;; Print length of result.
;;

;; Facts
(time
 (let (res)
   (facts:with ((?card :is-a :card :isbn ?isbn))
     (push ?isbn res))
   (print (length res))))

#|
4960  uhoh??

Evaluation took:
  2.483 seconds of real time
  2.482953 seconds of total run time (2.482953 user, 0.000000 system)
  100.00% CPU
  6,441,169,442 processor cycles
  720,704 bytes consed
|#

;; Loop
(time (loop for card in *cards*
         collect (getf card :|isbn|) into res
         finally (print (length res))))

#|
3062

Evaluation took:
  0.000 seconds of real time
|#



;;
;; All cards.quantity > 0 ?
;;

(time
 (let (res)
   (facts:with ((?card :is-a :card :quantity ?quantity))
     (when (plusp ?quantity)
       (push ?quantity res)))
   (length res)))

#|
Evaluation took:
  2.413 seconds of real time
  2.412363 seconds of total run time (2.408628 user, 0.003735 system)
  99.96% CPU
  6,260,721,280 processor cycles
  786,224 bytes consed

4958  ;; uhoh cannot be that much.
|#

(time (loop for card in *cards*
         when (plusp (getf card :|quantity|))
         collect card into res
         finally (print (length res))))

#|
3062 ;; correct

Evaluation took:
  0.000 seconds of real time
  0.000699 seconds of total run time (0.000689 user, 0.000010 system)
  100.00% CPU
  1,805,407 processor cycles
  65,536 bytes consed
|#
