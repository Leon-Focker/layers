;; ** layers
;;;; the class to rule them all, a layers object basically represents a piece

(in-package :layers)

;; *** layers
;;; new class layers, which represents the whole piece
(defclass layers (base-object)
  ())

;; *** make-layers
;;; create a layers-object
(defun make-layers (id list-of-layers)
  (make-instance 'layers
		 :id id
		 :data list-of-layers))

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

;; *** set-n
;;; change the n-for-list-of-durations value even while playing
(defmethod set-n (n layer-id (lys layers) current-time)
  (let* ((ly (handler-case (find-with-id layer-id (data lys))
	       ;; if no layer with id is found, just take first one in list
	       ;; to prevent this function crashing
	       (id-not-found (c)
		 (warn (text c))
		 (first (data lys)))))
	 ;; check and see if n is too big, then choose list
	 (ls (progn (when (>= n (length (data (structure ly))))
		      (progn (warn "~&n ~a is too big for structure of layer ~a"
				   n layer-id)
			     (setf n (- (length (data (structure ly))) 1))))
		    (nth n (data (structure ly)))))
	 (len (length ls))
	 (current 0) ;; current for new list-of-durations
	 (new-next-trigger (loop for n from 0 and i = (nth (mod n len) ls)
			  sum i into sum
			  until (> sum current-time)
			  finally (progn (setf current (mod (- n 1) len))
					 (return sum))))
	 (old-next-trigger (current-time ly)))
    ;; when resetting n, we need to tell lisp and the layer that we reset n:
    (setf (n-for-list-of-durations ly) n)
    ;; setting "current" should be obsolete, since the next length
    ;; will be chosen by using the current-time anyways. but for good measure:
    (update-list-of-durations ly current)
    ;; function get-next sets current time of layer to the time
    ;; when the next sample will be triggered. (old-next-trigger)
    ;; reset to new-next-trigger
    (setf (current-time ly)
	  new-next-trigger
	  ;; reset this-length (for next sample)
	  (this-length ly)
	  (get-next-by-time (current-time ly) (list-of-durations ly))
	  ;; maybe we don't want this here? choose soundfile again...
	  (current-stored-file ly)
	  (determine-new-stored-file ly))    
    ;; information what has happened:
    (format t "~&n for Layer ~a has been set to ~a" layer-id n)
    ;; finally tell PD what to do:
    ;; !!!!!!! the decay of the currently playing sound could be too long
    (list 'set-n layer-id (- new-next-trigger old-next-trigger))))

;; old code, not working. kept for now:
#|(defmethod set-n (n layer-id (lys layers) current-time sample-run-time)
  (let* ((ly (loop for layer in (data lys) do
		  (when (eq layer-id (get-id layer))
		    (return layer))))
	 ;; check and see if n is too big, then choose list
	 (ls (progn (when (>= n (length (data (structure ly))))
		      (progn (warn "~&n ~a is too big for structure of layer ~a"
				   n layer-id)
			     (setf n (- (length (data (structure ly)) 1)))))
		    (nth n (data (structure ly)))))
	 (st (start (current-stored-file ly)))
	 (len (length ls))
	 (current 0) ;; current for new list-of-durations
	 (last-trigger (loop for n from 0 and i = (nth (mod n len) ls)
			  sum i into sum
			  until (> sum current-time)
			  finally (progn (setf current (mod (- n 1) len))
					 (return (- sum i))))))
    (prog2
	(progn (setf (n-for-list-of-durations ly) n)
	       (update-list-of-durations ly current)
	       (setf (this-length ly) (see-current (list-of-durations ly)))
	       (setf (this-length ly)
		     (- (this-length ly)
			(- current-time
			   last-trigger)))
	       ;; to neatlessly transition we need to know how long the sample
	       ;; has been playing yet (sample-run-time) and set that as a start value now:
	       (setf (start (current-stored-file ly))
		     (+ (if (numberp (start (current-stored-file ly)))
			    (start (current-stored-file ly))
			    0)
			sample-run-time)))
	(play-this ly :get-next nil :change-sampler nil)
      (setf (start (current-stored-file ly)) st)
      ;; also there needs to be a way to shut of the other sample layer
      ;; (smooth the sound even when a new one is triggered and the old one hasn't faded yet)
      ;; implemented, check if working
(format t "~&n for Layer ~a has been set to ~a" layer-id n))))|#

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
  (format t "~&Layers have been reset"))

;; *** reload-layers
;;; even better than a reset. reloads everything
(defun reload-layers (&optional load-all)
  (load *score-file*)
  (when load-all (load-all))
  nil)

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
