;; ** layer
;;;; layer class - a lot of the magic is happening here

(in-package :layers)

;; *** layer
;;; a layer with distinct soundfile and properties
;;; one layer will send audio to one mixer channel
;;; play slot is responsible to stop layer after structure ended
(defclass layer (stored-file-list)
  ((stored-file-list :accessor stored-file-list
		     :initarg :stored-file-list :initform nil)
   (current-stored-file :accessor current-stored-file
			:initarg :current-stored-file :initform nil)
   (last-stored-file :accessor last-stored-file
		     :initarg :last-stored-file :initform nil)
   (this-length :accessor this-length :initarg :this-length :initform 1)
   (last-length :accessor last-length :initarg :last-length :initform 1)
   (structure :accessor structure :initarg :structure)
   (n-for-list-of-durations :accessor n-for-list-of-durations
		      :initarg :n-for-list-of-durations :type integer)
   (list-of-durations :accessor list-of-durations :initarg :list-of-durations :initform nil)
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
	(make-list-of-durations (structure ly) (n-for-list-of-durations ly))))

;; *** make-layer
;;; create a layer-object
(defun make-layer (id stored-file-list structure &optional (n 0) (panorama 45)
						   (use-pan-of-layer t))
  (make-instance 'layer
		 :id id
		 :stored-file-list stored-file-list
		 :structure structure
		 :n-for-list-of-durations n
		 :this-length (first (nth n (data structure)))
		 :panorama panorama
		 :use-pan-of-layer use-pan-of-layer))

;; *** get-id-current-file
;;; id of the current stored-file
(defmethod get-id-current-file ((ly layer))
  (get-id (current-stored-file ly)))

;; *** get-id-last-file
;;; id of the last stored-file
(defmethod get-id-last-file ((ly layer))
  (get-id (last-stored-file ly)))

;; *** print-layer
;;; print the layer object
(defmethod print-layer ((ly layer))
  (format t "~&Layer ID:          ~a ~
             ~&current soundfile: ~a ~
             ~&last soundfile:    ~a ~
             ~&this-length:       ~a ~
             ~&start:             ~a ~
             ~&play:              ~a"
          ;; ~&last in sfl:       ~a"
	  (id ly)
	  (get-id-current-file ly)
	  (get-id-last-file ly)
	  (this-length ly)
	  (start (current-stored-file ly))
	  (play ly)
	  ;; (get-id (last-played (stored-file-list ly)))
	  ))

;; *** swap-stored-file-list
;;; swap the stored-file-list of a layer, even while playing
(defmethod swap-stored-file-list ((ly layer) new-stored-file-list)
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
;;; will be called in 'get-next' after possibly changing the sfl etc.
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
			     (error (text c) 'get-sub-list-of-closest
				    (id (stored-file-list ly))))))
	   (cond (*use-sample-clouds*
		(decide-for-snd-file
		 (get-sub-list-of-closest (stored-file-list ly)
					  *x-y-z-position*)
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
  (let ((next-len (see-next (list-of-durations ly))))
    (setf (last-stored-file ly)
	  (current-stored-file ly)
	  (current-time ly)
	  (+ (current-time ly) (this-length ly)))
    ;; change sample-bank (sfl) of current layer depending on the
    ;; length of the next played soundfile (see-next)
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
		   (if (data (markov-list (last-played (stored-file-list ly))))
		       (markov-list (last-played (stored-file-list ly)))
		       (markov-list (second (data (stored-file-list ly)))))))
    ;; error, if current file still has no markov-list
    (unless (data (markov-list (current-stored-file ly)))
      (error "~&No markov-list found in current-stored-file ~a in Layer~a"
	     (get-id (current-stored-file ly)) (get-id ly)))
    ;; make the playback stop when the structure has ended (and *loop* is nil)
    (let ((ll (list-of-durations ly)))
      (when (and (= (current ll) (1- (length (data ll)))) (not *loop*))
	(setf (play ly) nil)))
    ;; set time for the next sample and then actually determine the new sample
    (setf
     ;; sample-lengths
     (last-length ly)
     (this-length ly)
     (this-length ly)
     (get-next-by-time (current-time ly) (list-of-durations ly))
     ;; new-sample
     (current-stored-file ly)
     (determine-new-stored-file ly)
     )))

;; *** update-layer
;;; updates the slots of a layer, when needed
;;; should maybe be called reset-layer?!
(defmethod update-layer ((ly layer))
  (setf (current-stored-file ly)
	(first (data (stored-file-list ly)))
	(last-stored-file ly)
	(current-stored-file ly)
	(list-of-durations ly)
	(make-list-of-durations (structure ly) (n-for-list-of-durations ly))
	(this-length ly)
	(see-current (list-of-durations ly))))

;; *** reset-index
;;; sets current slot of the list-of-durations of a layer back to 0 (start of loop)
(defmethod reset-index ((ly layer))
  (setf (current (list-of-durations ly)) 0)
  (format t "~& current timing index of layer ~a set to ~a"
	  (get-id ly) (current (list-of-durations ly))))

;; *** play-this
;;; sends list with all necessary information to pd, tsouo play
;;; the current stored-file
(defmethod play-this ((ly layer) &key
				   (printing t)
				   (output-for-unix nil)
				   (change-sampler t)
				   (get-next t))
  (if (and (play ly) *start-stop*)
      (prog1
	  (list
	   'trigger
	   ;; layer id (which voice in PD to send to)
	   (get-id ly)
	   ;; soundfile
	   (if (and *pd-on-windows*
		    (not output-for-unix))
	       (windows-path
		(path (if get-next (current-stored-file ly)
			  (last-stored-file ly))))
	       (unix-path
		(path (if get-next (current-stored-file ly)
			  (last-stored-file ly)))))
	   ;; soundfile-length in seconds
	   (this-length ly)
	   ;; start in seconds
	   (mod
	    (if (eq (start (current-stored-file ly)) 'random)
		(if (> (duration (current-stored-file ly)) (this-length ly))
		    (random (- (duration (current-stored-file ly)) (this-length ly)))
		    0)
		(or (start (current-stored-file ly)) 0))
	    (duration (current-stored-file ly)))
	   ;; attack in milliseconds
	   10
	   ;; decay in miliseconds
	   (* 1000 ;; from seconds to miliseconds
	      (let ((max-decay (see-next (list-of-durations ly)))
		    (decay (decay (current-stored-file ly))))
		(if (>= decay max-decay)
		    max-decay
		    decay)))
	   ;; amplitude
	   (amplitude (current-stored-file ly))
	   ;; loop-flag
	   (if (loop-flag (current-stored-file ly))
	       1 0)
	   ;; soundfile-id (displayed in PD)
	   (if get-next
	       (get-id-current-file ly)
	       (get-id-last-file ly))
	   ;; panning
	   (if (use-pan-of-layer ly)
	       (panorama ly)
	       (if (eq (panorama (current-stored-file ly)) 'random)
		   (* 90 (get-next *random-number*))
		   (panorama (current-stored-file ly))))
	   ;; change sampler or use same as last?
	   (if change-sampler 1 0))
	(when printing (print-layer ly))
	(when get-next (get-next ly)))
      (progn
	(setf (play ly) t)
	(format t "~&Playback for layer ~a ends now, last sound was ~a seconds"
		(get-id ly)
		(this-length ly)))))

;; *** update-list-of-durations
(defmethod update-list-of-durations ((ly layer) &optional (current 0))
  (setf (list-of-durations ly)
	(make-list-of-durations (structure ly) (n-for-list-of-durations ly) current)))

;;;; EOF layer.lsp
