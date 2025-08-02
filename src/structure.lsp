;; ** structure
;;;; stucture and fractal-structure class
;;;; Most functionality was moved to layers-utils: duration-layers and
;;;; lindenmayer-layers. This is mostly here for backwards compatability

(in-package :layers)

;; *** structure class and fractal-structure class
;;; a list of lists of durations and some information about it - for example
;;; how it was generated.
(defclass structure (lindenmayer-layers)
  ;; when this is t, re-generate the structure everytime the *total-duration*
  ;; is changed - for simple structures this means, that all sublists will
  ;; be of length *total-duration* after that.
  ((depends-on-total-length :accessor depends-on-total-length
			    :initarg :depends-on-total-length
			    :initform nil)))

;; *** re-gen-structure
;;; in case the *total-duration* changed,
;;; this function will generate a new structure, based on the initial arguments
(defmethod re-gen-structure ((st structure))
  (when (depends-on-total-length st)
    (regenerate st)
    (format t "structure ~a was regenerated" (id st))))

;; *** make-structure and make-fractal-structure
;;; takes several arguments to generate a structure using the lindenmayer fun.
;;; total-length: length of the structure (and piece) in seconds.
;;; EXAMPLE
#|
(make-structure '(2)
		'((1 ((2 1)))
		  (2 ((3 1 3)))
		  (3 ((2))))
		'((1 1)
		  (2 5)
		  (3 .2)))
|#
(defun make-structure (seed rules ratios
		       &key id
			 (duration *total-duration*)
			 (generation-method 'lindenmayer)
			 (smallest *max-smallest-sample-length*)
			 fixed-duration)
  (make-instance 'structure
		 :id (or id seed)
		 :data (funcall generation-method duration seed rules ratios smallest)
		 :seed seed
		 :generation-method generation-method
		 :rules rules
		 :ratios ratios
		 :depends-on-total-length
		 (unless fixed-duration
		   (when (= duration *total-duration*)
		     t))))

;; *** *compatability*

(defmethod depth-of-structure ((st structure))
  (how-many st))

(defmethod type ((st structure))
  (generation-method st))

(defun make-fractal-structure (seed rules ratios
			       &key id
				 (duration *total-duration*)
				 (generation-method 'lindenmayer)
				 (smallest *max-smallest-sample-length*)
				 fixed-duration)
  (make-structure seed rules ratios
		  :id id
		  :duration duration
		  :generation-method generation-method
		  :smallest smallest
		  :fixed-duration fixed-duration))

;;;; EOF structure.lsp
