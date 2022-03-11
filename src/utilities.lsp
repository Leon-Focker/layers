;; ** utilities
;;;; utility functions used in layers

(in-package :layers)

(defgeneric data (base-object)
  (:documentation "returns the data of any object"))

(defgeneric id (base-object)
  (:documentation "returns the id of any object"))

(when *load-risky-files*
  (load (format nil "~a~a" *src-dir* "export-with-clm.lsp")))

;; *** os-path
;;; converts device-names ("/E/", "E:/") according to current platfrom
(defun os-path (path)
  (let* ((new-path (substitute #\/ #\: path))
	 (device (or (pathname-device path)
		     (second (pathname-directory path))))
	 (rest (subseq new-path (position #\/ new-path :start 1))))
    (format nil "~a~a"
	    #+(or win32 win64) (format nil "~a:" device)
	    #-(or win32 win64) (format nil "/~a" device)
	    rest)))

;; *** start-osc
;;; simple function to open the osc-call
(defun start-osc ()
  (sc::osc-call :send-ip #(192 168 56 1) :listen-port 5000 :send-port 5000))

;; *** layers-has-been-loaded
;;; function with no features whatsoever, other files can check wheter this one
;;; (Layers.lsp) has been loaded by checking wheter this function is defined.
(defun layers-has-been-loaded ())

;; *** set-start-stop
;;; sets the global *start-stop* variable to t or nil (1 or 0 in pd)
(defun set-start-stop (val)
  (cond ((= val 0) (setf *start-stop* nil))
	((= val 1) (setf *start-stop* t))
	(t (error "~&set-start-stop got value ~a but needs either a 0 or 1"
		  val)))
  (format t "~& *start-stop* has been set to ~a" *start-stop*))

;; *** set-loop
;;; sets the global *loop* variable to t or nil (1 or 0 in pd)
(defun set-loop (val)
  (cond ((= val 0) (setf *loop* nil))
	((= val 1) (setf *loop* t))
	(t (error "~&set-loop got value ~a but needs either a 0 or 1"
		  val)))
  (format t "~& *loop* has been set to ~a" *loop*))

;; *** set-seed-to
;;; sets see to new value
(defun set-seed-to (seed)
  (setf *seed* seed)
  (format t "~& *seed* has been set to ~a" *seed*))

;; *** set-total-length
;;; sets the global *total-length* variable to value in seconds
(defun set-total-length (len)
  (setf *total-length* len)
  (format t "~& *total-length* has been set to ~a" *total-length*))

;; *** set-use-sample-clouds
;;; sets the global *total-length* variable to value in seconds
(defun set-use-sample-clouds (val)
  (cond ((= val 0) (setf *use-sample-clouds* nil))
	((= val 1) (setf *use-sample-clouds* t))
	(t (error "~&set-use-sample-clouds got value ~a~
                     but needs either a 0 or 1"
		  val)))
  (format t "~& *use-sample-clouds* has been set to ~a" *use-sample-clouds*))

;; *** set-x-y-z
;;; sets the global *total-length* variable to value in seconds
(defun set-x-y-z (x y z)
  (setf *x-y-z-position* (vector x y z))
  (format t "~& *x-y-z-position* has been set to ~a" *x-y-z-position*))

;; *** decider
;;; gets (random) value as chooser between 0 and 1 and a list
;;; of odds. Will return index of chosen element
(defun decider (chooser ls)
  (labels ((helper (chooser ls1 index sum)
	     (cond ((null ls1) (1- (length ls)))
		   ((< chooser sum) index)
		   (t (helper chooser
			      (cdr ls1)
			      (+ index 1)
			      (+ sum (car ls1)))))))
    (helper (sc::rescale chooser 0 1 0 (loop for i in ls sum i))
	    (cdr ls) 0 (car ls))))

;; *** remove-nth
;;; remove nth element from list
(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

;; *** alternating-modulo
;;; mirrors input value between min and max value
(defun alternating-modulo (value min max)
  (labels ((helper (val)
	     (cond ((> val max) (- max (- val max)))
		   ((< val min) (+ min (- min val)))
		   (t val))))
    (helper value)))

;; *** get-start-times
(defun get-start-times (durations)
  (loop for i in durations
     and sum = 0 then (+ sum i)
     collect sum))

;; *** soundfile-duration
;;; get the duration of a soundfile in seconds
(defun soundfile-duration (path)
  (clm::sound-duration path))

;; *** lists-to-midi
;;; write pitch duration and start-times into a midi-file
;;; longest list determindes length of the file, other lists get modulo
(defun lists-to-midi (pitch-list duration-list start-time-list
		      &key
			velocity-list
			(tempo 60)
			(name "test.mid")
			dir)
  (when (or (null pitch-list) (null duration-list))
    (error "please provide at least one value in the pitch and the duration ~
            lists in lists-to-midi"))
  (let* ((start start-time-list)
	 (pitch-len (length pitch-list))
	 (duration-len (length duration-list))
	 (start-len (length start-time-list))
	 (velo (or velocity-list '(0.7)))
	 (velo-len (length velo))
	 (how-many (apply #'max (list pitch-len duration-len start-len)))
	 (events (loop for i below how-many collect
		      (let* ((start-time (when start
					   (nth (mod i start-len)
						start-time-list)))
			     (pitch (nth (mod i pitch-len) pitch-list))
			     (duration (nth (mod i duration-len) duration-list))
			     (velocity (nth (mod i velo-len) velo)))
			(sc::make-event pitch
					duration
					:start-time start-time
					:duration t
					:amplitude velocity)))))
    (unless start (sc::events-update-time events))
    (sc::event-list-to-midi-file
     events
     :start-tempo tempo
     :midi-file (format nil "~a~a~a" (or dir *default-dir*) "/" name))))

;; *** structure-to-midi
(defun structure-to-midi (structure &key n dir)
  (let* ((rhythm-blocks (data structure))
	 (len (- (length rhythm-blocks) 1))
	 (pitches (if n
		      (loop repeat (length (nth n rhythm-blocks)) collect
			   (sc::midi-to-note 60))
		      (loop for i below len append
			   (loop repeat (length (nth i rhythm-blocks)) collect
				(sc::midi-to-note (- 60 i))))))
	 (start-times (if n
			  (get-start-times (nth n rhythm-blocks))
			  (loop for i below len append
			       (get-start-times (nth i rhythm-blocks)))))
	 (durations (if n
			(nth n rhythm-blocks)
			(loop for i below len append (nth i rhythm-blocks)))))
    (lists-to-midi pitches durations start-times
		   :dir (or dir *default-dir*)
		   :name (if n
			     (format nil "~a~a~a~a~a" "structure-"
				     (id structure) "-n-" n ".mid")
			     (format nil "~a~a~a" "structure-"
				     (id structure) ".mid")))))

;; *** distance-between-points
;;; distance between two points p1 and p2,
;;; both need to be vectors with the same length
(defun distance-between-points (p1 p2)
  (unless (and (vectorp p1) (vectorp p2) (= (length p1) (length p2)))
    (error "~&both arguments need to be same-length vectors in function~
             distance-between-points, ~%args: ~a ~a" p1 p2))
  (sqrt (loop for q across p1 and p across p2 sum (expt (- q p) 2.0))))

;;;; EOF utilities.lsp
