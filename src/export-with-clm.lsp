(in-package :ly)	   

(defun swap-nth (n value ls)
  (loop for i from 0 and j in ls collect
	(if (= i n) value j)))

;; *** get-list-of-clm-calls
;;; get a list of lists, containing all information needed to feed
;;; Michael Edwards' samp1 and render layers as a .wav file
(defun get-list-of-clm-calls (layers &key (reset-layers nil))
  (when reset-layers (reset-layers layers))
  (let* ((hm-layers (length (data layers)))
	 (times (loop repeat hm-layers collect 0))) ; list of times the respective layers are at
    (prog1
	(loop
	   until break
	   with break
	   ;; check which layer has the minimal current time value
	   for min-val = (apply #'min times)
	   for index = (- hm-layers
			  (length (member min-val times)))
	   ;; get next sample for that layer
	   for sample = (play-this (nth index (data layers))
				   :printing nil
				   :output-for-unix t)
	   collect (unless break
		     (when sample
		       (list (nth 1 sample)    ; layer-id
			     (nth index times) ; time
			     (nth 2 sample)    ; file
			     (nth 3 sample)    ; duration
			     (nth 4 sample)    ; start
			     (nth 5 sample)    ; attack
			     (nth 6 sample)    ; decay
			     (nth 7 sample)    ; amplitude
			     (nth 8 sample)    ; loop-flag
			     (nth 10 sample)))) ; panorama
	   ;; set new time value for the last played layer
	   do (setf times (swap-nth index (+ (nth index times)
					     (nth 3 sample))
				    times))
	     (when (>= min-val (* *total-duration* 0.995)) (setf break t)))
      (format t "~&getting calls for clm...")
      (when reset-layers (reset-layers layers)))))

(in-package :clm)

;; *** calls-to-samp0
;;; this is a helper function for layers-to-clm
;;; it can call itself recursively, which is why it needed to be seperate
(defun calls-to-samp0 (calls ids)
    (loop for call in calls and i from 0 do
	 (when (and (member (first call) ids)
		    (not (equal (pathname-name (nth 2 call)) "rest")))
	     ;; get all parameters and name them
	     (let* ((id (nth 0 call))
		    (time (nth 1 call))
		    (file (nth 2 call))
		    (dur (nth 3 call))
		    (start (nth 4 call))
		    (attack (/ (nth 5 call) 1000))
		    (decay (/ (nth 6 call) 1000))
		    (max-dur (- (clm::sound-duration file) start))
		    (duration (if (> dur max-dur) max-dur (+ dur decay 0.01)))
		    (amplitude (nth 7 call))
		    (loop-flag (nth 8 call))
		    (call-again (and loop-flag (> dur max-dur)))
		    (panorama (nth 9 call)))
	       ;; samp0 would reverse the sound, not just loop it - that's why
	       ;; sounds that are supposed to loop have to be called again:
	       (when call-again
		 (calls-to-samp0 (list (list id (+ time max-dur) file (- dur max-dur) 0
					     0.01 decay amplitude loop-flag panorama))
				 ids))
	       ;; call to samp0:
	       (samp0 file
		      time
		      :duration duration
		      :start start
		      :amp-env (if call-again
				   '(0 0  0.1 1  99.9 1  100 0)
				   `(0 0  ,(* (/ attack (+ dur decay) 100)) 1
				       ,(* (/ dur (+ dur decay)) 100) 1
				       100.1 0))
		      :amp amplitude
		      :degree panorama
		      ;; to cope with samples of any samplerate:
		      :srt (/ (clm::sound-srate file) 48000)
		      ;;loop has to happen as a separate call
		      )))))

;; *** layers-to-clm
;;; gets a layers-object and renders out a soundfile (constant faders)
(defun layers-to-clm (layers &optional list-of-layer-ids)
  (let* ((calls (ly::get-list-of-clm-calls layers))
	 (ids (if list-of-layer-ids list-of-layer-ids
		  (ly::get-ids layers))))
    (with-sound (:header-type mus-riff
	         :sampling-rate 48000
	         :channels 2
	         :play nil
		 :scaled-to 0.95
	         :output (concatenate 'string
				      ly::*default-sample-dir*
				      "layers.wav"))
      (calls-to-samp0 calls ids))))

(in-package :ly)

(defun layers-to-clm (layers &optional list-of-layer-ids)
  (clm::layers-to-clm layers list-of-layer-ids))
