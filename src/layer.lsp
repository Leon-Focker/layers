;; ** layer
;;;; layer class - a lot of the magic is happening here

(in-package :layers)

;; *** layer
;;; a layer with distinct soundfiles and properties
;;; one layer will send audio information to one mixer channel
(defclass layer (stored-file-list)
  ((stored-file-list :accessor stored-file-list
		     :initarg :stored-file-list :initform nil)
   (current-stored-file :accessor current-stored-file
			:initarg :current-stored-file :initform nil)
   (last-stored-file :accessor last-stored-file
		     :initarg :last-stored-file :initform nil)
   (this-length :accessor this-length :initarg :this-length :initform 1)
   (play-length :accessor play-length :initarg :play-length :initform 1)
   (last-length :accessor last-length :initarg :last-length :initform 1)
   (remaining-duration :accessor remaining-duration :initarg :remaining-duration
		       :initform 0)
   (structure :accessor structure :initarg :structure)
   (n-for-list-of-durations :accessor n-for-list-of-durations
		      :initarg :n-for-list-of-durations :type integer)
   (list-of-durations :accessor list-of-durations :initarg :list-of-durations
		      :initform nil)
   ;; alternative way to determine new rhythm:
   (use-rhythm-function :accessor use-rhythm-function
			:initarg :use-rhythm-function :initform nil)
   ;; the only argument when called is always the layer-object itself.
   (rhythm-function :accessor rhythm-function :initarg :rhythm-function
		    :initform #'(lambda (ly) (declare (ignore ly)) 1))
   ;; if nil, the layer will stop sending new information.
   (play :accessor play :initarg :play :initform t)
   (current-time :accessor current-time :initarg :current-time :initform 0)
   (panorama :accessor panorama :initarg :panorama :initform 45)
   (use-pan-of-layer :accessor use-pan-of-layer :initarg :use-pan-of-layer
		     :initform t)))

;; *** initialize-instance
;;; called automatically, sets last-stored-file to current-s-f when initializing
(defmethod initialize-instance :after ((ly layer) &rest initargs)
  (declare (ignore initargs))
  (setf (current-stored-file ly) (first (data (stored-file-list ly)))
	(last-stored-file ly) (current-stored-file ly)
	(list-of-durations ly)
	(if (use-rhythm-function ly)
	    (make-instance 'list-of-durations
			   :data (list (funcall (rhythm-function ly) ly))
			   :current 0)
	    (make-list-of-durations (structure ly)(n-for-list-of-durations ly)))
	(this-length ly)
	(see-current (list-of-durations ly))
	(play-length ly)
	(this-length ly)
	(remaining-duration ly)
	(this-length ly)))

;; *** make-layer
;;; create a layer-object
(defun make-layer (id stored-file-list structure &optional (n 0) (panorama 45)
						   (use-pan-of-layer t)
						   (error-fun #'warn)
						   (rhythm-function
						    #'(lambda (ly)
							(declare (ignore ly))
							1))
						   use-rhythm-function)
  (unless (equal (type-of stored-file-list) 'stored-file-list)
    (error "sfl in #'make-layer was not of type stored-file-list but: ~a"
	   stored-file-list))
  (check-sanity stored-file-list error-fun)
  (unless (subtypep (type-of structure) 'structure)
    (warn "sfl in #'make-layer was not of type structure: ~a, ~
           using rhythm-funtion instead." structure)
    (setf structure nil use-rhythm-function 1))
  (make-instance 'layer
		 :id id
		 :stored-file-list stored-file-list
		 :structure structure
		 :n-for-list-of-durations n
		 :rhythm-function rhythm-function
		 :use-rhythm-function use-rhythm-function
		 :panorama panorama
		 :use-pan-of-layer use-pan-of-layer))

;; *** get-id-current-file
;;; id of the current stored-file
(defmethod get-id-current-file ((ly layer))
  (when (current-stored-file ly) (get-id (current-stored-file ly))))

;; *** get-id-last-file
;;; id of the last stored-file
(defmethod get-id-last-file ((ly layer))
  (get-id (last-stored-file ly)))

;; *** print-object
;;; print the layer object
(defmethod print-object ((ly layer) stream)
  (format stream "~%Layer ID:          ~a ~
                  ~&current soundfile: ~a ~
                  ~&duration:          ~a ~
                  ~&start:             ~a"
	  (id ly)
	  (get-id-current-file ly)
	  ;;(get-id-last-file ly)
	  (play-length ly)
	  (when (current-stored-file ly) (start (current-stored-file ly)))
	  ;;(play ly)
	  ;; (get-id (last-played (stored-file-list ly)))
	  ))

;; *** print-layer
;;; print the layer object - deprecated
;;; (setf (symbol-function 'print-layer) #'print)

;; *** swap-stored-file-list
;;; swap the stored-file-list of a layer, even while playing
(defmethod swap-stored-file-list ((ly layer) new-stored-file-list)
  (unless (equal (type-of new-stored-file-list) 'stored-file-list)
    (error "sfl in #'swap-stored-file-list was not of type stored-file-list ~
            but: ~a"
	   new-stored-file-list))
  ;; don't do anything when both sfls are the same
  (unless (eq (stored-file-list ly) new-stored-file-list)
    (setf (stored-file-list ly)
	  new-stored-file-list
    ;; after the stored-file-list has been swapped, set the current-stored-file
    ;; to the last played of the new stored-file-list
	  (current-stored-file ly)
	  (last-played (stored-file-list ly)))
    (format t "~& Layer ~a now plays ~a"
	    (get-id ly)
	    (get-id new-stored-file-list))))

;; *** determine-new-stored-file
;;; will be called in 'get-next', after possibly changing the sfl etc.
;;; determines next sound file in current-stored-file-list depending on either:
;;; - length-dependant-list
;;; - markov list with fixed-seed-randomness
;;; - position in coordinate space and fixed-seed-randomness
;;; can receive a sub-sfl, containing only some stored-files
;;; then checks wheter the preferred-length matches, if no: repeat.
(defmethod determine-new-stored-file ((ly layer))
  (let ((data (data (stored-file-list ly))))
    (loop for snd in data until break
       with break
       with new-id =
       ;; when current file has no length-dependant-list
       ;; a random number and the markov-list will decide.
       ;; else we use the length-dependant-list
	 (handler-bind ((markov-list-is-nil
			 #'(lambda (c)
			     (error (text c)
				    (get-id
				     (current-stored-file ly)))))
			(no-value
			 #'(lambda (c)
			     (error (text c)
				    'get-sub-list-of-closest
				    (id (stored-file-list ly)))))
			(weird-values
			 #'(lambda (c)
			     (error (text c)
				    'get-sub-list-of-closest
				    (id (stored-file-list ly))))))
	   (cond (*use-sample-clouds*
		(decide-for-snd-file
		 (get-sub-list-of-closest (stored-file-list ly)
					  *x-y-z-position*
					  :max-distance *cloud-radius*)
		 (get-next *random-number*)))
	       ((length-dependant-list (current-stored-file ly))
		(decide-for-snd-file
		 (length-dependant-list (current-stored-file ly))
		 (this-length ly)))
	       (t (decide-for-snd-file
		   (markov-list
		    (current-stored-file ly))
		   (get-next *random-number*)))))
       do
	 (when (eq (id snd)
		   new-id)
	   (setf break t)
	   ;; check wheter chosen stored-file likes the length it would get
	   (if (and *use-preferred-lengths* (preferred-length snd))
	       (if (and (> (this-length ly) (first (preferred-length snd)))
			(< (this-length ly) (second (preferred-length snd))))
		   (return snd)
		   ;; if not, get a file that wants to be played
		   (let ((ls '()))
		     (loop for snd1 in data do
			  (if (preferred-length snd1)
			      ;; check whether file likes the current length
			      (when (and (> (this-length ly)
					    (first (preferred-length snd1)))
					 (< (this-length ly)
					    (second (preferred-length snd1))))
				(push snd1 ls))
			      ;; if it has ne preferrences, add it as well
			      (push snd1 ls)))
		     (if (null ls)
			 ;; when nobody wants to be played, play original one
			 (return snd)
			 ;; else choose at random :o
			 (return (nth (floor (* (get-next *random-number*)
						(length ls)))
				      ls)))))
	       (return snd)))
	 (when (and (eq snd (car (last data)))
		    (not break))
	   (error "there is no file with ID: ~a in current-stored-file ~
                         list: ~a" new-id (get-id (stored-file-list ly)))))))

;; *** get-next
;;; moves on to the next stored sound file
(defmethod get-next ((ly layer))
  (unless ly (error "in get-next, the layer object is nil"))
  (unless (equal (type-of (stored-file-list ly)) 'stored-file-list)
    (error "get-next did not find a proper sfl in layer ~a: ~a"
	   (id ly) (stored-file-list ly)))
  ;;(check-sanity (stored-file-list ly))
  (unless (data (stored-file-list ly))
    (error "stored-file-list of layer ~a seems to be empty" (id ly)))
  ;; get next duration:
  (let ((next-len 1))
    (setf (last-stored-file ly)
	  (current-stored-file ly)
	  (current-time ly)
	  (+ (current-time ly) (this-length ly))
	  next-len
	  (if (use-rhythm-function ly)
	      (progn (add-to-list (list-of-durations ly)
				  (funcall (rhythm-function ly) ly))
		     (see-current (list-of-durations ly)))
	      (get-next-by-time (current-time ly) (list-of-durations ly))))
    ;; change sample-bank (sfl) of current layer depending on the
    ;; length of the next played soundfile -> next-len
    (cond ((< next-len (length-min (stored-file-list ly)))
	   (swap-stored-file-list
	    ly
	    (if (sfl-when-shorter (stored-file-list ly))
		(sfl-when-shorter (stored-file-list ly))
		(stored-file-list ly))))
	  ((> next-len (length-max (stored-file-list ly)))
	   (swap-stored-file-list
	    ly
	    (if (sfl-when-longer (stored-file-list ly))
		(sfl-when-longer (stored-file-list ly))
		(stored-file-list ly))))
	  (t ))
    ;; check wheter there is at least one file in the sfl that's not a rest
    (unless (or (data (stored-file-list ly))
		(not (eq 'rest
			 (get-id (first (data (stored-file-list ly)))))))
      (error "~&there are no sound files stored in sfl ~a"
	     (get-id (stored-file-list ly))))
    ;; when the (new) current event is actually a rest,
    ;; copy and use the last played markov list
    (when (eq 'rest (get-id (current-stored-file ly)))
      (setf-markov (current-stored-file ly)
		   ;; if the last played is also a rest, use second file in list
		   (cond ((data
			   (markov-list (last-played (stored-file-list ly))))
			  (markov-list (last-played (stored-file-list ly))))
			 (t (loop for i in (data (stored-file-list ly))
				  when (markov-list i)
				    do (return (markov-list i)))))))
    ;; error, if current file still has no markov-list
    (unless (data (markov-list (current-stored-file ly)))
      (error "~&No markov-list found in current-stored-file ~a in Layer~a"
	     (get-id (current-stored-file ly)) (get-id ly)))
    ;; make the playback stop when the structure has ended (and *loop* is nil)
    (let ((ll (list-of-durations ly)))
      (when (and (= (current ll) (1- (length (data ll))))
		 (not *loop*)
		 (not (use-rhythm-function ly)))
	(setf (play ly) nil)))
    ;; set time for the next sample and then actually determine the new sample
    (setf
     ;; sample-lengths
     (last-length ly)
     (this-length ly)
     ;; using pop, instead of #'get-next when using the rhythm-function
     (this-length ly)
     (if (use-rhythm-function ly) (pop (data (list-of-durations ly))) next-len)
     (play-length ly)
     (this-length ly)
     (remaining-duration ly)
     (this-length ly)
     ;; new-sample
     (current-stored-file ly)
     (determine-new-stored-file ly)
     )))

;; *** update-times
;;; after chanigng a structure, similar to set-n, the timings for the next
;;; trigger etc might have shiftet. This updates those times.
(defmethod update-times ((ly layer) current-time current-timer)
  (let* ((lod (data (list-of-durations ly)))
	 (len (length lod))
	 ;; look for next trigger time and sum
	 (new-next-trigger
	   (loop for n from 0 and i = (nth (mod n len) lod)
		 sum i into sum
		 until (> sum current-time)
		 finally (return sum)))
	 (passed-timer (- *next-trigger* current-timer)))
    (setf (current-time ly)
	  new-next-trigger
	  ;; set this-length according to current position in list
	  (this-length ly)
	  (see-current (list-of-durations ly))
	  (remaining-duration ly)
	  ;; time between last and next general trigger
	  (+ passed-timer		       ;last-trigger until now
	     (- new-next-trigger current-time))) ;now until end of sample
    ;; warn when negative remainder
    (when (< (remaining-duration ly) 0)
      (warn "something is off, remaining-duration for ~a is negative: ~a"
	    (id ly) (remaining-duration ly)))
    (when *print-to-console* (format t "~&updated for layer ~a" (id ly)))
    (list 'update
	  ;; time until next trigger
	  (float (- *next-trigger* current-timer))
	  ;; reset-timer in pd?
	  0
	  ;; layer ID
	  (id ly)
	  ;; remaining time for currently played file
	  (float (- new-next-trigger current-time))
	  ;; new decay 
	  (float (see-next (list-of-durations ly))))))

;; *** reset
;;; updates the slots of a layer, when needed
;;; could be named #'reset...
(defmethod reset-layer ((ly layer))
  (setf (current-stored-file ly)
	(first (data (stored-file-list ly)))
	(last-stored-file ly)
	(current-stored-file ly)
	(list-of-durations ly)
	(make-list-of-durations (structure ly) (n-for-list-of-durations ly))
	(this-length ly)
	(see-current (list-of-durations ly))
	(play-length ly)
	(this-length ly)
	(remaining-duration ly)
	(this-length ly)))

;; *** reset-index
;;; sets current slot of the list-of-durations of a layer back to 0 (start of loop)
(defmethod reset-index ((ly layer))
  (setf (current (list-of-durations ly)) 0)
  (format t "~& current timing index of layer ~a set to ~a"
	  (get-id ly) (current (list-of-durations ly))))

;; *** play-this
;;; sends list with all necessary information to pd, so to play
;;; the current stored-file
(defmethod play-this ((ly layer) &key
				   (offset-start 0)
				   (printing t) ;t
				   (output-for-unix nil)
				   (change-sampler t)
				   (get-next t))
  (unless (data (stored-file-list ly))
    (error "stored-file-list of layer ~a seems to be empty" (id ly)))
  (if (and (or *loop* (play ly)) *start-stop*)
      (prog1
	  (list
	   'layer
	   ;; layer id (which voice in PD to send to)
	   (get-id ly)
	   ;; soundfile
	   (os-format-path (path (if get-next (current-stored-file ly)
				     (last-stored-file ly)))
			   (if (and *pd-on-windows*
				    (not output-for-unix))
			       'windows
			       'unix))
	   ;; soundfile-length in seconds, pd needs floats
	   (float (play-length ly))
	   ;; start in seconds
	   (float (mod
		   (+ (if (eq (start (current-stored-file ly)) 'random)
			  (if (> (duration (current-stored-file ly)) (this-length ly))
			      (random (- (duration (current-stored-file ly)) (this-length ly)))
			      0)
			  (or (start (current-stored-file ly)) 0))
		      offset-start)
		   (duration (current-stored-file ly))))
	   ;; attack in milliseconds
	   10
	   ;; decay in miliseconds
	   (float (* 1000 ;; from seconds to miliseconds
		     (let ((max-decay (see-next (list-of-durations ly)))
			   (decay (decay (current-stored-file ly))))
		       (if (>= decay max-decay)
			   max-decay
			   decay))))
	   ;; amplitude
	   (float (amplitude (current-stored-file ly)))
	   ;; loop-flag
	   (if (loop-flag (current-stored-file ly))
	       1 0)
	   ;; soundfile-id (displayed in PD)
	   (if get-next
	       (get-id-current-file ly)
	       (get-id-last-file ly))
	   ;; panning
	   (float (if (use-pan-of-layer ly)
		      (panorama ly)
		      (if (eq (panorama (current-stored-file ly)) 'random)
			  (* 90 (get-next *random-number*))
			  (panorama (current-stored-file ly)))))
	   ;; change sampler or use same as last?
	   (if change-sampler 1 0))
	(when printing (print ly))
	(when get-next (get-next ly)))
      (prog1
	  (list
	   'layer
	   ;; layer id (which voice in PD to send to)
	   (get-id ly)
	   ;; soundfile
	   (os-format-path (path (if get-next (current-stored-file ly)
				     (last-stored-file ly)))
			   (if (and *pd-on-windows*
				    (not output-for-unix))
			       'windows
			       'unix))
	   ;; soundfile-length in seconds
	   0
	   ;; start in seconds
	   0
	   ;; attack in milliseconds
	   0
	   ;; decay in miliseconds
	   0
	   ;; amplitude
	   0
	   ;; loop-flag
	   0
	   ;; soundfile-id (displayed in PD)
	   "stopped"
	   ;; panning
	   45
	   ;; change sampler or use same as last?
	   0)
	;; (setf (play ly) t)
	(format t "~&Playback for layer ~a ends now, last sound was ~a seconds"
		(get-id ly)
		(this-length ly)))))

;; *** update-list-of-durations
(defmethod update-list-of-durations ((ly layer) &optional (current 0))
  (setf (list-of-durations ly)
	(make-list-of-durations (structure ly) (n-for-list-of-durations ly) current)))

;;;; EOF layer.lsp
