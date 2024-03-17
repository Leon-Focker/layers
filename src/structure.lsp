;; ** structure
;;;; stucture and fractal-structure class

(in-package :layers)

;; *** structure class and fractal-structure class
;;; a list of lists of durations and some information about it - for example
;;; how it was generated.
(defclass structure (list-object)
  ;; when this is t, re-generate the structure everytime the *total-duration*
  ;; is changed - for simple structures this means, that all sublists will
  ;; be of length *total-duration* after that.
  ((depends-on-total-length :accessor depends-on-total-length
			    :initarg :depends-on-total-length
			    :initform nil)
   (depth-of-structure :accessor depth-of-structure
		       :initarg :depth-of-structure
		       :type integer :initform 0)
   (type :accessor type :initarg :type :initform 'simple)))

(defclass fractal-structure (structure)
  ((seed :accessor seed :initarg :seed)
   (rules :accessor rules :initarg :rules)
   (ratios :accessor ratios :initarg :ratios)))

(defmethod initialize-instance :after ((st structure) &rest initargs)
  (declare (ignore initargs))
  (let* (new-data)
    (unless (listp (data st))
      (error "data in make-structure must be a list: ~a" (data st)))
    (setf new-data (loop for ls in (data st) when (listp ls) collect ls))
    (unless (and (car new-data)
		 (loop for ls in new-data
		       always (loop for i in ls always (numberp i))))
      (error "data in make-structure seems to be faulty: ~a" (data st)))
    (setf (data st)
	  new-data
	  (depth-of-structure st)
	  (length new-data))))

;; *** print-object
(defmethod print-object ((st structure) stream)
  (format stream "~&Structure ID:  ~a ~
                  ~&depth:         ~a"
	  (id st)
	  (depth-of-structure st)))

;; *** scale-smallest-value-to
(defmethod scale-smallest-value-to ((st structure) new-smallest-value)
  (let* ((data (data st))
	 (minimum (apply #'min (first data)))
	 (scaler (/ new-smallest-value minimum)))
    (setf (data st)
	  (loop for ls in data collect
	       (if (atom ls) (* ls scaler 1.0)
		   (loop for i in ls collect (* i scaler)))))
    st))

;; *** scale-biggest-value-to
(defmethod scale-biggest-value-to ((st structure) new-biggest-value)
  (let* ((data (data st))
	 (maximum (apply #'max (first data)))
	 (scaler (/ new-biggest-value maximum)))
    (setf (data st)
	  (loop for ls in data collect
	       (if (atom ls) (* ls scaler 1.0)
		   (loop for i in ls collect (* i scaler)))))
    st))

;; *** scale-structure
;;; loop through all lists in the data of a structure and scale the duration
;;; of each to the target-duration
(defmethod scale-structure ((st structure) target-duration)
  (setf (data st)
	(loop for ls in (data st)
	      collect (scale-list-to-sum ls target-duration))))

;; *** re-gen-structure
;;; in case the *total-duration* changed,
;;; this function will generate a new structure, based on the initial arguments
(defmethod re-gen-structure ((st structure))
  (print st)
  (when (depends-on-total-length st)
    (setf (data st)
	  (case (type st)
	    (lindenmayer
	     (lindenmayer *total-duration* (seed st) (rules st) (ratios st)))
	    (compartmentalise
	     (compartmentalise *total-duration* (seed st) (rules st) (ratios st)))
	    (simple (scale-structure st *total-duration*))
	    (t (error "re-gen-structure does not know how to gen structure of~
                       type ~a" (type st)))))
    (format t "structure ~a was regenerated" (id st))))

;; *** make-structure and make-fractal-structure

;;; generate a structure-object
(defun make-structure (id data &optional depends-on-total-length)
  (make-instance 'structure
		 :id id
		 :data data
		 :depends-on-total-length (when depends-on-total-length)))

;;; takes several arguments to generate a structure using the lindenmayer fun.
;;; total-length: length of the structure (and piece) in seconds.
;;; EXAMPLE
#|
(make-fractal-structure '(2)
		'((1 ((2 1)))
		  (2 ((3 1 3)))
		  (3 ((2))))
		'((1 1)
		  (2 5)
		  (3 .2)))
|#
(defun make-fractal-structure (seed rules ratios
			       &key id
				 (duration *total-duration*)
				 (type 'lindenmayer)
				 (smallest *max-smallest-sample-length*)
				 fixed-duration)
  ;; sanity
  (unless (listp seed) (error "malformed seed ~a, must be a list." seed))
  (unless (when (listp rules)
	    (loop for i in rules always (listp i)))
    (error "malformed rules ~a, must be of type '((1 (2 1)) (2 (1 2)))"
	   rules))
  (unless (when (listp ratios)
	    (loop for i in ratios always (listp i)))
    (error "malformed ratios ~a, must be of type '((1 .2) (2 .6))"
	   ratios))
  (unless (= (length rules) (length ratios))
    (error "malformed rules and ratios, should be the same length not ~a and ~a"
	   (length rules) (length ratios)))
  (make-instance 'fractal-structure
		 :id (or id seed)
		 :data (funcall type duration seed rules ratios smallest)
		 :seed seed
		 :type type
		 :rules rules
		 :ratios ratios
		 :depends-on-total-length
		 (unless fixed-duration
		   (when (= duration *total-duration*)
		     t))))

;; *** visualize-structure
;;; use the imago library to visualize the structure.
(load (probe-file (format nil "~a~a" *src-dir* "show-structure.lsp")))

;;;; EOF structure.lsp
