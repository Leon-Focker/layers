;; ** structure
;;;; stucture and list-of-durations class

(in-package :layers)

;; *** structure
;;; stores the structure as a list of durations and some information as to how
;;; it was generated
(defclass structure (list-object)
  ((seed :accessor seed :initarg :seed)
   (rules :accessor rules :initarg :rules)
   (ratios :accessor ratios :initarg :ratios)))

;; *** make-structure
;;; generate a structure-object
;;; takes several arguments to generate a structure using the lindenmayer fun.
;;; total-length: length of the structure (and piece) in seconds
;;; the higher the n, the lower the recursion
(defun make-structure (seed rules ratios
		       &key id
			    (duration *total-length*)
		            (type 'lindenmayer)
		            (smallest *max-smallest-sample-length*))
  (make-instance 'structure
		 :id id
		 :data (funcall type duration seed rules ratios smallest)
		 :seed seed
		 :rules rules
		 :ratios ratios))

;; *** re-gen-structure
;;; in case the *total-length* changed,
;;; this function will generate a new structure, based on the initial arguments
(defmethod re-gen-structure ((st structure))
  (setf (data st)
	(lindenmayer *total-length* (seed st) (rules st) (ratios st))))

;; example:
#+nil(defparameter *structure*
       (make-structure '(2)
		       '((1 ((2 1)))
			 (2 ((3 1 3)))
			 (3 ((2))))
		       '((1 1)
			 (2 5)
			 (3 .2))))

;; *** list-of-durations
;;; a structure is a list of list-of-durations, for the length list, we choose one
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
	      (decider (mod (/ current-time *total-length*) 1.0) data))
	(when (= (current ll) (length data))
	  (setf (current ll) 0))
	;;(format t "~&index: ~a~&current: ~a" (current ll) current-time)
	(nth (current ll) data)))))

;; *** visualize-structure
;;; use the sketch library to visualize the structure. bit crooked
(when *load-risky-files*
  (load-from-same-dir "show-structure.lsp")
  (defmethod visualize-structure ((st structure))
    (show-structure (reverse (cdr (reverse (data st)))))))

;;;; EOF structure.lsp
