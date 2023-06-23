;; ** layers
;;;; the class to rule them all, a layers object basically represents a piece

(in-package :layers)

;; *** layers
;;; new class layers, which represents the whole piece
(defclass layers (list-object)
  ())

;;; *layers* can only be an object of type layers
(declaim (type layers *layers*))

;; *** make-layers
;;; create a layers-object
(defun make-layers (id list-of-layers)
  (make-instance 'layers
		 :id id
		 :data list-of-layers))

(defparameter *layers* (make-layers nil nil))

;; *** add-layer-to-layers
;;; add a layer-object to the piece (layers-object)
(defmethod add-layer-to-layers ((ly layer) (lys layers)
				&key (warn-if-double t))
  (if (null (data lys))
      (setf (data lys) (list ly))
      (let* ((double (member (id ly)
			     (loop for i in (data lys) collect (get-id i)))))
	(if double
	    (progn
	      (when warn-if-double
		(warn (format nil "the id ~a is already in the lys ~a ~
                                   and will be replaced"
			      (get-id ly) (get-id lys))))
	      (setf (data lys)
		    (remove-nth (- (length (data lys))
				   (length double))
				(data lys)))
	      (push ly (data lys)))
	    (push ly (data lys))))))

;; *** play-layer
;;; play a layer from a layers-object
(defmethod play-layer (layer-id (lys layers))
  (let* ((ly (loop for layer in (data lys) do
		  (when (eq layer-id (get-id layer))
		    (return layer)))))
    (if ly
        (play-this ly)
	(error "~&there is no Layer with ID ~a in layers ~a"
	       layer-id (get-id lys)))))

;; *** next-trigger-for-layer
;;; helper function for next-trigger and set-n
(defun next-trigger-for-layer (remaining ly &key (index 1) (threshold 0.01))
  (setf remaining (+ remaining (see-with-relative-index (list-of-durations ly) index)))
  (if (> remaining threshold) remaining (next-trigger-for-layer remaining ly :index (+ index 1))))

;; *** next-trigger
;;; this function is called, when the timer within pure data reaches 0
(defmethod next-trigger ((lys layers) &key current-time trigger-all)
  (unless lys (error "in next-trigger, the layers object is nil"))
  ;; subtract current next-trigger from time until next trigger of each layer
  ;; and look for the times until the next layer will be triggered
  (let* ((next-triggers (loop for ly in (data lys)
			   do (setf (remaining-duration ly)
				    (- (remaining-duration ly) *next-trigger*))
			   collect (let* ((remaining (remaining-duration ly)))
				     (if (> remaining 0.01)
					 remaining
					 (next-trigger-for-layer remaining ly)))))
	 (next-layers-triggered '()))
    (format t "~&next-triggers: ~a" next-triggers)
    ;; here we decide which layers are to be triggered
    ;; usually layers with remaining time < 0.01 are chosen.
    ;; when playback is started, all layers must be triggered
    (setf next-layers-triggered
	  (loop for ly in (data lys)
	     ;; somehow the following doesn't work:
	     ;do (when (> (remaining-duration ly) 0.01)
	     ;	  (setf (play-length ly) (- (play-length ly) *next-trigger*)))
	     when (or trigger-all (<= (remaining-duration ly) 0.01))
	     collect ly))
    ;; set *next-trigger* to the minimal time until the next layer needs one
    (setf *next-trigger* (* 0.01 (round (* 100 (apply #'min next-triggers)))))
    ;; set timer to next-trigger and send list of layers that will be triggered
    (when current-time (format t "~&current-time: ~a" current-time))
    (append
     (list 'trigger *next-trigger*)
     (loop for ly in next-layers-triggered append
	  (play-this ly :offset-start (print (if (> (remaining-duration ly) 0.01)
					  (- (this-length ly) (remaining-duration ly))
					  0)))))))

;; *** set-n
;;; change the n-for-list-of-durations value even while playing
(defmethod set-n (n layer-id (lys layers) current-time current-timer)
  ;; look for the layer we want to change n for
  (let* ((ly (handler-case (find-with-id layer-id (data lys))
	       ;; if no layer with id is found, just take first one in list
	       ;; to prevent this function crashing
	       (id-not-found (c)
		 (warn (text c))
		 (first (data lys)))))
	 ;; check and see if n is too big, then choose list-of-durations (ls)
	 (ls (progn (when (>= n (length (data (structure ly))))
		      (warn "~&n ~a is too big for structure of layer ~a"
			    n layer-id)
		      (setf n (- (length (data (structure ly))) 1)))
		    (nth n (data (structure ly)))))
	 ;; some neat variables
	 (len (length ls))
	 (current-duration-index 0)
	 ;; in new list, look for next trigger time and sum
	 (new-next-trigger (loop for n from 0 and i = (nth (mod n len) ls)
			      sum i into sum
			      until (> sum current-time)
			      finally (progn (setf current-duration-index
						   (mod n len))
					     (return sum))))
	 ;; (old-next-trigger (current-time ly))
	 (passed-timer (- *next-trigger* current-timer))
	 (reset-timer-flag 0))
    ;; store new n in layer
    (setf (n-for-list-of-durations ly) n)
    ;; setting "current-duration-index" should be obsolete, since the next length
    ;; will be chosen by using the current-time anyways. but for good measure:
    (update-list-of-durations ly current-duration-index)
    ;; function get-next sets current time of layer to the time
    ;; when the next sample will be triggered. (old-next-trigger)
    ;; reset to new-next-trigger
    (setf (current-time ly)
	  new-next-trigger
	  ;; set this-length according to current-time
	  (this-length ly)
	  (see-current (list-of-durations ly))
	  ;; (get-next-by-time (print (current-time ly)) (list-of-durations ly))	  
	  ;; set new play-length (?) and remaining time (time between last and next general trigger)
	  ;; (play-length ly)
	  ;; (- new-next-trigger current-time)
	  (remaining-duration ly)
	  (+ passed-timer		; last-trigger until now
	     (- new-next-trigger current-time)) ; now until end of sample
	  ;; choose soundfile again... maybe we don't want this here?
	  (current-stored-file ly)
	  (determine-new-stored-file ly))
    (when (< (remaining-duration ly) 0)
      (warn "something is off, remaining-duration is negative: ~a" (remaining-duration ly)))
    ;; find new *next-trigger*, calling this function solves all my troubles,
    ;; idk why
    (next-trigger *layers* :trigger-all t)
    ;; information what has happened:
    (format t "~&n for Layer ~a has been set to ~a" layer-id n)
    (format t "~&SET TIMER TO: ~a ~a" (- *next-trigger* current-timer) reset-timer-flag)
    ;; finally tell PD what to do:
    (list 'set-n
		 ;; time until next trigger
		 (float (- *next-trigger* current-timer))
		 ;; reset-timer in pd?
		 reset-timer-flag
		 ;; layer ID
		 layer-id
		 ;; remaining time for currently played file
		 (float (- new-next-trigger current-time))
		 ;; new decay
		 (float (see-next (list-of-durations ly))))
      ))


;; old code, dirty :c , kept for now
#+nil(defmethod set-n (n layer-id (lys layers) current-time current-timer)
  ;; look for the layer we want to change n for
  (let* ((ly (handler-case (find-with-id layer-id (data lys))
	       ;; if no layer with id is found, just take first one in list
	       ;; to prevent this function crashing
	       (id-not-found (c)
		 (warn (text c))
		 (first (data lys)))))
	 ;; check and see if n is too big, then choose list-of-durations (ls)
	 (ls (progn (when (>= n (length (data (structure ly))))
		      (warn "~&n ~a is too big for structure of layer ~a"
			    n layer-id)
		      (setf n (- (length (data (structure ly))) 1)))
		    (nth n (data (structure ly)))))
	 ;; some neat variables
	 (len (length ls))
	 (current-duration-index 0)
	 (new-next-trigger (loop for n from 0 and i = (nth (mod n len) ls)
			      sum i into sum
			      until (> sum current-time)
			      finally (progn (setf current-duration-index
						   (mod n len))
					     (return sum))))
	 ;; (old-next-trigger (current-time ly))
	 (passed-timer (- *next-trigger* current-timer))
	 (reset-timer-flag 0))
    ;; store new n in layer
    (setf (n-for-list-of-durations ly) n)
    ;; setting "current-duration-index" should be obsolete, since the next length
    ;; will be chosen by using the current-time anyways. but for good measure:
    (update-list-of-durations ly current-duration-index)
    ;; function get-next sets current time of layer to the time
    ;; when the next sample will be triggered. (old-next-trigger)
    ;; reset to new-next-trigger
    (setf (current-time ly)
	  new-next-trigger
	  ;; set this-length according to current-time
	  (this-length ly)
	  (see-current (list-of-durations ly))
	  ;; (get-next-by-time (print (current-time ly)) (list-of-durations ly))	  
	  ;; set new play-length and remaining time (time between last and next general trigger)
	  (play-length ly)
	  (- new-next-trigger current-time)
	  (remaining-duration ly)
	  (+ passed-timer		; last-trigger until now
	     (play-length ly))		; now until end of sample
	  ;; choose soundfile again... maybe we don't want this here?
	  (current-stored-file ly)
	  (determine-new-stored-file ly))
    (when (< (remaining-duration ly) 0)
      (warn "something is off, remaining-duration is negative: ~a" (remaining-duration ly)))
    ;; find new *next-trigger*
    ;; next-triggers should use the next-trigger-for-layer function
    (let* ((next-triggers (loop for ly in (data lys)
			     collect (let* ((remaining (remaining-duration ly)))
				       (if (> remaining passed-timer)
					   remaining
					   (next-trigger-for-layer
					    remaining
					    ly
					    :threshold passed-timer)))))
	   ;; when (> (remaining-duration ly)
	   ;; passed-timer)
	   ;; collect (remaining-duration ly)))
	   (new (+ (* 0.01 (round (* 100 (apply #'min next-triggers))))
		   passed-timer)))
      (unless (= *next-trigger* new)
	(incf reset-timer-flag 1)
	(setf *next-trigger* new)))
    ;; information what has happened:
    (format t "~&n for Layer ~a has been set to ~a" layer-id n)
    ;; finally tell PD what to do:
    (format t "~&SET TIMER TO: ~a ~a" (- *next-trigger* current-timer) reset-timer-flag)
    (prog1 (list 'set-n
		 ;; time until next trigger
		 (- *next-trigger* current-timer)
		 ;; reset-timer in pd?
		 reset-timer-flag
		 ;; layer ID
		 layer-id
		 ;; remaining time for currently played file
		 (- new-next-trigger current-time)
		 ;; new decay
		 (see-next (list-of-durations ly)))
      (next-trigger *layers* :trigger-all t))))

;; *** reset-layers
;;; resets everything to the start of the piece and re-read structure
(defun reset-layers (&optional layers-object)
  (setf (data *random-number*) *seed*)	; reset *random-number*
  (unless layers-object (setf layers-object *layers*))
  (let ((sts '()))
    ;; re-generate structures
    (loop for layer in (data layers-object) do
	 (unless (member (structure layer) sts)
	   (push (structure layer) sts)))
    (loop for st in sts do
	 (re-gen-structure st))
    ;; update each layer, reset some init values
    (loop for layer in (data layers-object) do
	 (update-layer layer)
	 (reset-index layer)
	 (setf (play layer) t)
	 (setf (current-time layer) 0)))
  (setf *next-trigger* 0)
  (format t "~&Layers have been reset"))

;; *** reload-layers
;;; even better than a reset. reloads everything
(defun reload-layers (&optional load-all)
  (let ((score *score-file*))
    (when load-all (load-all))
    (load score)
    nil))

;; *** get-ids
;;; get ids of all stored-files in stored-file-list
(defmethod get-ids ((lys layers))
  (loop for ly in (data lys) collect
       (get-id ly)))

;; would be cool but obviously doesn't work for now:

#|
;; *** layers-to-txt
;;; saves a layers object into a txt file, so you don't have to reload and
;;; re-analyse all the samples when restarting the software.
(defmethod layers-to-txt ((lys layers) &key (dir *src-dir*) (name "*layers*.txt"))
  (with-open-file (stream (format nil "~a~a" dir name)
			  :direction :output)
    (format stream (write-to-string lys))))

;; *** layers-from-txt
;;; see above
(defun layers-from-txt (path)
  )|#

;;;; EOF layers.lsp
