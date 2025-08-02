;; ** utilities
;;;; utility functions used in layers

(in-package :layers)

(defgeneric data (base-object)
  (:documentation "returns the data of any object"))

(defgeneric id (base-object)
  (:documentation "returns the id of any object"))

(defgeneric get-id (base-object)
  (:documentation "returns the id of any object"))

(defgeneric next-trigger (layers-object &key current-time trigger-all)
  (:documentation "called for triggering new sounds"))

#+nil(when *load-risky-files*
  (load (format nil "~a~a" *src-dir* "export-with-clm.lsp")))

;; *** conditions
(define-condition markov-list-is-nil (error)
  ((text :initarg :text :reader text)))

(define-condition markov-list-is-empty (error)
  ((text :initarg :text :reader text)))

(define-condition no-value (error)
  ((text :initarg :text :reader text
	 :initform "~&when calling ~a some data was missing: ~&~a")))

(define-condition weird-values (error)
  ((text :initarg :text :reader text
	 :initform "~&when calling ~a it noticed some weird values in: ~&~a")))

(define-condition id-not-found (error)
  ((text :initarg :text :reader text)))

;; *** start-osc
;;; simple function to open the osc-call
(defun start-osc (&optional ip port)
  (when ip (unless (vectorp ip)
	     (error "ip must be a vector of form #(192 168 56 1) but is: ~a" ip)))
  (osc-call :send-ip (or ip #(192 168 178 20)) :listen-port (or port 5000)
	    :send-port (or port 5000)))

;; *** layers-has-been-loaded
;;; function with no features whatsoever, other files can check wheter this one
;;; has been loaded by checking wheter this function is defined.
(defun layers-has-been-loaded ())

;; *** set-start-stop
;;; sets the global *start-stop* variable to t or nil (1 or 0 in pd)
(defun set-start-stop (val &optional (time-left 0.02))
  (declare (special *layers*))
  (unless *layers* (error "in set-start-stop, *layers* is nil"))
  (prog1 (cond ((= val 0) (setf *start-stop* nil)
		(setf *next-trigger* (- *next-trigger* time-left)))
	       ((= val 1)
		(setf *start-stop* t)
		(next-trigger *layers* :trigger-all t))
	       (t (error "~&set-start-stop got value ~a but needs either a 0 or 1"
			 val)))
    (format t "~& *start-stop* has been set to ~a" *start-stop*)))

;; *** set-loop
;;; sets the global *loop* variable to t or nil (1 or 0 in pd)
(defun set-loop (val)
  (cond ((= val 0) (setf *loop* nil))
	((= val 1) (setf *loop* t))
	(t (error "~&set-loop got value ~a but needs either a 0 or 1"
		  val)))
  (format t "~& *loop* has been set to ~a" *loop*))

;; *** set-seed-to
;;; sets seed to new value
(defun set-seed-to (seed)
  (setf *seed* seed)
  (format t "~& *seed* has been set to ~a" *seed*))

;; *** set-cloud-radius-to
;;; sets *cloud-radius* to new value
(defun set-cloud-radius-to (val)
  (setf *cloud-radius* val)
  (when *print-to-console*
    (format t "~& *cloud-radius* has been set to ~a" *cloud-radius*)))

;; *** get-current-times
;;; get the current-time and timer values from pd
(defun get-current-times ()
  (list 'TIMES 1))

;; *** set-current-times
;;; a way for pd to send the times
(defun set-current-times (time timer)
  (setf *current-time* time
	*current-timer* timer))

;; *** set-total-length
;;; sets the global *total-duration* variable to value in seconds
(defun set-total-length (len)
  (setf *total-duration* len)
  (let ((all-st '()))
    (loop for lys in *all-layers*
	  do (loop for ly in (data lys)
		   do (pushnew (structure ly) all-st)))
    (loop for st in all-st do (re-gen-structure st)))
  ;; ask pd for current time:
  (list 'TIME-U 1))

;; *** set-current-times-for-update
;;; send time and then use them for the update
(defun set-current-times-for-update (time timer)
  (setf *current-time* time
	*current-timer* timer)
  (update-times *layers* time timer)
  (format t "~& *total-duration* has been set to ~a" *total-duration*))

;; *** set-print-to-console
;;; sets the globals *print-to-console* variable
(defun set-print-to-console (val)
  (cond ((= val 0) (setf *print-to-console* nil))
	((= val 1) (setf *print-to-console* t))
	(t (error "~&set-print-to-console got value ~a~
                     but needs either a 0 or 1"
		  val)))
  (format t "~& *print-to-console* has been set to ~a" *print-to-console*))

;; *** set-use-sample-clouds
;;; sets the global *total-duration* variable to value in seconds
(defun set-use-sample-clouds (val)
  (cond ((= val 0) (setf *use-sample-clouds* nil))
	((= val 1) (setf *use-sample-clouds* t))
	(t (error "~&set-use-sample-clouds got value ~a~
                     but needs either a 0 or 1"
		  val)))
  (format t "~& *use-sample-clouds* has been set to ~a" *use-sample-clouds*))

;; *** set-x-y-z
;;; sets the global *total-duration* variable to value in seconds
(defun set-x-y-z (x y z)
  (setf *x-y-z-position* (vector x y z))
  (when *print-to-console*
    (format t "~& *x-y-z-position* has been set to ~a" *x-y-z-position*)))

;; *** set-timer
;;; sets timer within pure data to value in ms,
;;; usually next-trigger is used to do this.
(defun set-timer (time)
  (unless (numberp time) (error "time in set-timer must be a number"))
  (list 'timer time))

;; *** find-with-id
;;; loop through list of objects, looking for object with specific id
(defun find-with-id (id ls)
  (let* ((res (loop for el in ls do
		   (when (eq id (get-id el))
		     (return el)))))
    (unless res (error 'id-not-found
		       :text (format nil "~&id ~a not found in given list" id)))
    res))

;;;; EOF utilities.lsp
