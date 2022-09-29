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

(when *load-risky-files*
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

;; *** unix-path
;;; converts device-names ("/E/", "E:/") to unix format
(defun unix-path (path)
  (let* ((new-path (substitute #\/ #\: path))
	 (device (or (pathname-device path)
		     (second (pathname-directory path))))
	 (rest (subseq new-path (position #\/ new-path :start 1))))
    (format nil "~a~a"
	    (format nil "/~a" device)
	    rest)))

;; *** windows-path
;;; converts device-names ("/E/", "E:/") to unix format
(defun windows-path (path)
  (let* ((new-path (substitute #\/ #\: path))
	 (device (or (pathname-device path)
		     (second (pathname-directory path))))
	 (rest (subseq new-path (position #\/ new-path :start 1))))
    (format nil "~a~a"
	    (format nil "~a:" device)
	    rest)))

;; *** start-osc
;;; simple function to open the osc-call
(defun start-osc ()
  (sc::osc-call :send-ip #(192 168 56 1) :listen-port 5000 :send-port 5000))

;; *** layers-has-been-loaded
;;; function with no features whatsoever, other files can check wheter this one
;;; has been loaded by checking wheter this function is defined.
(defun layers-has-been-loaded ())

;; *** set-start-stop
;;; sets the global *start-stop* variable to t or nil (1 or 0 in pd)
(defun set-start-stop (val &optional (time-left 0.02))
  (unless *layers* (error "in set-start-stop, *layers* is nil"))
  (prog1 (cond ((= val 0) (setf *start-stop* nil)
		(setf *next-trigger* (- *next-trigger* time-left))
		(format t "~&*next-trigger was set to ~a" time-left))
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
(defun set-x-y-z (x y z &key (printing nil))
  (setf *x-y-z-position* (vector x y z))
  (when printing
    (format t "~& *x-y-z-position* has been set to ~a" *x-y-z-position*)))

;; *** set-timer
;;; sets timer within pure data to value in ms, usually next-trigger is used to do this.
(defun set-timer (time)
  (unless (numberp time) (error "time in set-timer must be a number"))
  (list 'timer time))

;; *** current-timer
;;; can probably be deleted:
;;; when offsetting the timer, this function is called to check, wheter triggers
;;; need to be skipped (because they already should have happened) and another
;;; offset needs to be added to the timer, because it currently is negative
#+nil(defun current-timer (current-timer)
  (unless (> current-timer 0)
    (;; skip next trigger points and get new offset
     (list 'timer-offset offset!!!!!!!!!!!!))))

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
    (helper (sc::rescale chooser 0 1 0 (loop for i in ls
					  do (unless i (error 'no-value))
					  sum i))
	    (cdr ls) 0 (car ls))))

;; *** index-of-element
(defun index-of-element (element ls)
  (unless (and ls (listp ls)) (error 'no-value))
  (let* ((len (length ls)))
    (- len (length (member element ls)))))

;; *** remove-nth
;;; remove nth element from list
(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

;; *** mirrors
;;; formerly calles alternating-modulo
;;; mirrors input value between min and max value
(defun mirrors (value min max)
  (labels ((helper (val)
	     (cond ((> val max) (helper (- max (- val max))))
		   ((< val min) (helper (+ min (- min val))))
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

;; *** biggest-jump
;;; discrete derivation maximum i guess?
(defun biggest-jump (ls)
  (loop for i in (cdr ls)
     with last = (car ls)
     maximize (abs (- last i))
     do (setf last i)))

;; *** biggest-jump-up
(defun biggest-jump-up (ls)
  (loop for i in (cdr ls)
     with last = (car ls)
     maximize (- i last)
     do (setf last i)))

;; *** biggest-jump-down
(defun biggest-jump-down (ls)
  (loop for i in (cdr ls)
     with last = (car ls)
     maximize (- last i)
     do (setf last i)))

;; *** reduce-by
;;; reduces a list by factor (100 elements to 10 elements, factor 10)
;;; using average
(defun reduce-by (ls &optional (factor 10))
  (unless (integerp factor)
    (error "reduce-by needs a factor that is an integer, not ~a" factor))
  (let* ((new '()))
    (loop for i from 1 and e in ls with sum = 0 do
	 (incf sum (/ e factor))
	 (when (= (mod i factor) 0)
	   (push sum new)
	   (setf sum 0)))
    (reverse new)))

;; *** flatness
;;; this might not be useful at all for anything but spectra
(defun list-flatness (ls)
  (let ((arithmetic-mean 0)
	(geometric-mean 0)
	(len (length ls)))
    (loop for i in ls do
	 (when (= i 0) (setf i 0.000001))
	 (incf arithmetic-mean i)
	 (incf geometric-mean (log i)))
    (setf arithmetic-mean (/ arithmetic-mean len))
    (setf geometric-mean (exp (/ geometric-mean len)))
    (/ geometric-mean arithmetic-mean)))

;; *** find-with-id
;;; loop through list of objects, looking for object with specific id
(defun find-with-id (id ls)
  (let* ((res (loop for el in ls do
		   (when (eq id (get-id el))
		     (return el)))))
    (unless res (error 'id-not-found
		       :text (format nil "~&id ~a not found in given list" id)))
    res))

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
     :midi-file (format nil "~a~a~a" (or dir *src-dir*) "/" name))))

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
		   :dir (or dir *src-dir*)
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
    (error "~&both arguments need to be same-length vectors in function ~
             distance-between-points, ~%args: ~a ~a" p1 p2))
  (sqrt (loop for q across p1 and p across p2 sum (expt (- q p) 2.0))))

;; *** max-of-array
;;; max of an array
(defun max-of-array (array)
  (loop for i below (length array) maximize
       (abs (aref array i))))

;; *** get-spectral-centroid
;;; calulate the spectral centroid out of a list of frequency-magnitude pairs
;;; eq.: '((440 0.5) (630 0.46) (880 0.25))
(defun get-spectral-centroid (list-of-pairs)
  (let ((centroid 1))
    (loop for pair in list-of-pairs
       sum (* (car pair) (cadr pair)) into sum1
       sum (cadr pair) into sum2
       do (setf centroid (/ sum1 sum2)))
    centroid))

;; *** visualize stuff :)
;;; aray or list as input
(defun visualize (ls &key scaler (start 0) abs scale)
  (when (arrayp ls)
    (setf ls (loop for i across ls collect i)))
  (when abs (setf ls (loop for i in ls collect (abs i))))
  (let* ((matrix (make-array '(64 17) :initial-element 0.0))
	 (maxi (apply #'max (mapcar #'abs ls)))
	 (scaler (if scaler scaler
		     (if (= maxi 0) 1 maxi)))
	 (len (length ls))
	 (size (if (or scale (>= len 64)) 64 len)))
    (loop for i from start below (+ size start) do
	 (loop for j below 17 do
	      (if (= (round (+ (* (/ (nth (mod (floor
						(+ start
						   (* (/ i size)
						      (- len start))))
					       len)
					  ls)
				     scaler)
				  8 (if abs 2 1))
			       (* 8 (if abs 0 1))))
		     j)
		  (setf (aref matrix (- i start) j) 1)
		  (setf (aref matrix (- i start) j) 0))))
    (loop for j downfrom 16 to 0 do
	 (print  (apply 'concatenate 'string
			(loop for i below 64 collect
			     (if (= (aref matrix i j)  1)
				 "_"
				 " ")))))
    "=)"))

;; *** taylor-polynom
;;; get taylor polynomial, pls check this again, probably false
(defun second-order-taylor (ls x)
  (let* ((len (length ls))
	 (n (floor (* x len)))
	 ;; list as function
	 (f1 #'(lambda (n) (nth n ls)))
	 ;; crooked derivative function
	 (f2 #'(lambda (f n) (/ (- (funcall f (1+ n)) (funcall f (1- n)))
				(/ 2 len))))
	 (a (funcall f1 n))
	 (b (funcall f2 f1 n))
	 (c (funcall f2 #'(lambda (n) (funcall f2 f1 n)) n))
	 )
  (lambda (x) (+ a
		 (* b x)
		 (* c (expt x 2))))))

;; *** second-order-taylor-rotated
;;; bs function, not needed for now
#+nil(defun second-order-taylor-rotated (ls x)
  (let* ((len (length ls))
	 (n (floor (* x len)))
	 ;; list as function
	 (f1 #'(lambda (x) (decider (/ x len) ls)))
	 ;; crooked derivative function
	 (f2 #'(lambda (f n) (/ (- (funcall f (1+ n)) (funcall f (1- n)))
				(/ 2 len))))
	 (a (funcall f1 n))
	 (b (funcall f2 f1 n))
	 (c (funcall f2 #'(lambda (n) (funcall f2 f1 n)) n))
	 )
  (lambda (x) (+ a
		 (* b x)
		 (* c (expt x 2))))))

;;;; EOF utilities.lsp
