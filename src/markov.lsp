;; ** markov
;;;; markov-list-object

(in-package :layers)

;; *** markov-list
;;; list of pairs of soundfile-ids and allocated relative odds
(defclass markov-list (list-object)
  ())

;; *** make-markov-list
;;; make an instance of markov-list
(defun make-markov-list (id ml)
  (make-instance 'markov-list :data ml :id id))

;;; example:
#|(make-markov-list 'test
		  '(("/E/test.wav" 5)
		    ("/E/test1.wav" 2)
		    ("/E/test2.wav" 7)))|#

;; *** decide-for-snd-file
;;; returns the car of the chosen sublist of a markov-list object
;;; (the id of the next soundfile)
(defmethod decide-for-snd-file ((ml markov-list) random-number)
  (car (nth (decider
	     random-number
	     (loop for i in (data ml) collect (cadr i)))
	    (data ml))))

;;;; EOF markov.lsp
