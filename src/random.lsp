;; ** random
;;;; some methods for fixed seed random numbers

(in-package :layers)

;; *** random-number
;;; stores current pseudo random number and can get the next one
(defclass random-number (base-object)
  ((data :accessor data :initarg :data :type float :initform *seed*)
   (pcg :accessor pcg :initarg :pcg :initform nil)))

;; *** random-number
;;; make an instance of random-number
(defun random-number (&optional seed)
  (make-instance 'random-number
		 :data (if seed seed *seed*)
		 :pcg (pcg::make-pcg :seed (if seed seed *seed*))))

;;; set the global variable *random-number* to an instance of random-number
(setf *random-number* (random-number))

;;; get current value of a random-numer object
(defmethod get-number ((rn random-number))
  (data rn))

;; *** get-next
;;; get a random-number object to the next pseudo random number
(defmethod get-next ((rn random-number))
  (setf (data rn)
	(pcg::pcg-random (pcg rn) 1.0)))

;; *** set-state
;;; set the random-state of the prng
(defmethod set-state ((rn random-number) state)
  (prog1 (setf (pcg::pcg-state (pcg rn))
	       state)
    (get-next rn)))

;;;; EOF random.lsp
