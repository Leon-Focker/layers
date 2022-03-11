;; ** length-dependant-list
;;;; length-dependant-list object

(in-package :layers)

;; *** length-dependant-list
;;; list of pairs of soundfile-ids and allocated lengths
;;; -> the length of the currently played soundfile decides the next file
(defclass length-dependant-list (list-object)
  ())

;; *** make-length-dependant-list
;;; make an instance of length-dependant-list
(defun make-length-dependant-list (id ldl)
  (make-instance 'length-dependant-list :data ldl :id id))

;;; example:
#|(make-length-dependant-list 'test
		  '(("/E/test.wav" 0.5)
		    ("/E/test1.wav" 2)
		    ("/E/test2.wav" 10)))|#

;; *** decide-for-snd-file
;;; returns the car of the chosen sublist of a length-dependant-list object
;;; (the id of the next soundfile)
(defmethod decide-for-snd-file ((ldl length-dependant-list) current-length)
  (loop for ls in ldl do
       (when (> current-length (cadr ls)) (return (car ls)))))

;;;; EOF length-dependant-list.lsp
