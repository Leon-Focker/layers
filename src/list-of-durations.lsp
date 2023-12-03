;; ** list-of-durations
;;;; list-of-durations class

(in-package :layers)

;; *** class
;;; a structure is a list of list-of-durations, for the l-o-d, we choose one
;;; of those lists
(defclass list-of-durations (list-object)
  ((structure :accessor structure :initarg :structure :initform nil)))

;; *** make-list-of-durations
;;; initialize a list-of-durations object

(defmethod make-list-of-durations ((st structure) &optional (n 0) (current 0))
  (make-instance 'list-of-durations
		 :data (nth n (data st))
		 :structure st
		 :current current))

;; *** get-next-by-time
;;; don't just get the next value in the list, use decider and the current time
(defmethod get-next-by-time (current-time (ll list-of-durations))
  (when (data ll)
    (let* ((data (data ll)))
      (progn
	(setf (current ll)
	      (decider (mod (/ current-time *total-duration*) 1.0) data))
	(when (= (current ll) (length data))
	  (setf (current ll) 0))
	;;(format t "~&index: ~a~&current: ~a" (current ll) current-time)
	(nth (current ll) data)))))

;; EOF list-of-durations.lsp
