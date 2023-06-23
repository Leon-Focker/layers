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
  (unless (data ml) (error 'markov-list-is-nil
			   :text "~&when determinig the new soundfile, ~
                 the markov-list of the current soundfile ~a is nil"))
  (let* ((ls (loop for i in (data ml) collect (cadr i))))
    (handler-bind ((no-value
		    #'(lambda (c) (error (text c)
			"decide-for-snd-file with markov-list" ls))))
      (car (nth (decider
		 random-number
		 ls)
		(data ml))))))

;; *** make-load-file
(defmethod make-load-file ((ml markov-list) &optional environment)
    (declare (ignore environment))
    `(make-instance 'markov-list
		    :id ',(id ml)
		    :data ',(data ml)))

;;;; EOF markov.lsp
