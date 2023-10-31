;; ** Midi
;;;; Collection of some useful midi input, output and transformation functions

;; *** lists-to-midi
;;; generate a midi file from lists of starting points, length, pitch...
;;; if one list is shorter than others it will be wrapped (mod).
;;; Has a lot less features than slippery chickens event-list-to-midi-file, but
;;; in return it's a lot easier and skipps the generation of an event, which
;;; might be favorable in some cases.
(defun lists-to-midi (pitch-list duration-list start-time-list
			  &key
			    velocity-list
			    (tempo 60)
			    (channel 0)
			    (file (format nil "~a~a" *src-dir*
					  "midi-output.mid")))
  (when (or (null pitch-list) (null duration-list))
    (error "please provide at least one value in the pitch and the duration ~
            lists in lists-to-midi"))
  (unless start-time-list (setf start-time-list
				(get-start-times duration-list)))
  (let* ((pitch-len (length pitch-list))
	 (duration-len (length duration-list))
	 (start-len (length start-time-list))
	 (velo (or velocity-list '(0.7)))
	 (velo-len (length velo))
	 (total (apply #'max `(,pitch-len ,duration-len ,start-len ,velo-len)))
	 (events (sort
		  (loop for i below total collect
		       `(,(let ((pitch (nth (mod i pitch-len) pitch-list)))
			    (if (numberp pitch) pitch (note-to-midi pitch)))
			  ,(nth (mod i start-len) start-time-list)
			  ,(nth (mod i velo-len) velo)
			  ,(nth (mod i duration-len) duration-list)))
		  #'(lambda (x y) (< (second x) (second y))))))
    (setf events (loop for event in events appending
		      (cm::output-midi-note
		       (pop event)
		       0
		       (pop event)
		       (pop event)
		       (pop event)
		       channel)))
    (cm::events
     (cm::new cm::seq :name (gensym) :time 0.0 :subobjects events)
     file
     :tempo tempo)))

;;; old version using event-list-to-midi-file
;;; if the start-time-list is empty, (events-update-time) will be called
#|
(defun lists-to-midi (pitch-list duration-list start-time-list
		      &key
			velocity-list
			(tempo 60)
			;;(channel 0)
			(name "test.mid")
			dir)
  (when (or (null pitch-list) (null duration-list))
    (error "please provide at least one value in the pitch and the duration ~
            lists in lists-to-midi"))
  (let* ((pitch-len (length pitch-list))
	 (duration-len (length duration-list))
	 (start-len (length start-time-list))
	 (velo (or velocity-list '(0.7)))
	 (velo-len (length velo))
	 (how-many (apply #'max (list pitch-len duration-len start-len)))
	 (events (loop for i below how-many
		    for start-time = (when start-time-list
                                       (nth (mod i start-len)
                                            start-time-list))
                    for pitch = (nth (mod i pitch-len) pitch-list)
                    for duration = (nth (mod i duration-len) duration-list)
                    for velocity = (nth (mod i velo-len) velo)
                    collect
                      (sc::make-event (if (numberp pitch)
                                          (sc::midi-to-note pitch)
                                          pitch)
                                      duration
                                      :start-time start-time
                                      :duration t
                                      :amplitude velocity))))
    (unless start-time-list (sc::events-update-time events))
    (sc::event-list-to-midi-file
     events
     :start-tempo tempo
     :midi-file (print (format nil "~a~a" (or dir *src-dir*) name)))))
|#

;; *** structure-to-midi
(defun structure-to-midi (structure &key n dir)
  (let* ((rhythm-blocks (data structure))
	 (len (- (length rhythm-blocks) 1))
	 (pitches (if n
		      (loop repeat (length (nth n rhythm-blocks)) collect
			   (midi-to-note 60))
		      (loop for i below len append
			   (loop repeat (length (nth i rhythm-blocks)) collect
				(midi-to-note (- 60 i))))))
	 (start-times (if n
			  (get-start-times
			   (loop for i in
				(nth n rhythm-blocks)
				collect (float i)))
			  (loop for i below len append
			       (get-start-times
				(loop for i in
				     (nth i rhythm-blocks)
				     collect (float i))))))
	 (durations (if n
			(nth n rhythm-blocks)
			(loop for i below len append (nth i rhythm-blocks)))))
    (lists-to-midi pitches durations start-times
		   :file (if n
			     (format nil "~a~a~a~a~a~a" (or dir *src-dir*)
				     "structure-" (id structure) "-n-" n ".mid")
			     (format nil "~a~a~a~a" "structure-"
				     (or dir *src-dir*) (id structure)
				     ".mid")))))

;; *** midi-file-to-list
;;; slippery chickens midi-file-to-events seems to make things more complicated
;;; than need be, so this function just reads the file without making events
;;; !!! notice, that currently there is no tempo argument - so be careful,
;;; that the imported file is already in the right tempo
(defun midi-file-to-list (file &optional track)
  (let* ((cm-midi (cm::parse-midi-file file track)))
    (remove nil
	    (loop for m in cm-midi
	       collect (typecase m
			 (cm::midi (list (cm::object-time m)
					 (cm::midi-keynum m)
					 (cm::midi-duration m)
					 (cm::midi-amplitude m)
					 (cm::midi-channel m)
					 (+ (cm::object-time m)
					    (cm::midi-duration m)))))))))

(setf (symbol-function 'midi-to-list) #'midi-file-to-list)

;; *** harmony-filter
;;; takes a midi file, which will then be filtered by a second midi file.
;;; you can define the probability of a note being changed and the detection
;;; range for notes, ie. how close a note in the filter file has to be,
;;; to consider changing the original note to it.
;;; probability can be an envelope of style '(0 1  [...]  1 1)
;;; as can be the detection range.
;;; TODO fixed seed please!;;; 
(defun harmony-filter (midi-file midi-filter probability detection-range
		       &key track1 track2
			 (name (format nil "~afiltered_file.mid" fu::*futils-load-dir*))
			 (seed 1))
  (let* ((file1 (midi-file-to-list midi-file track1))
	 (file2 (midi-file-to-list midi-filter track2))
	 (probability-env (if (atom probability)
			      (list 0 probability 1 probability)
			      probability))
	 (detection-range-env (if (atom detection-range)
				  (list 0 detection-range 1 detection-range)
				  detection-range))
	 (ran-nb (random-number seed))
	 duration
	 (harmony '())
	 (result '())
	 (events '()))
    ;; make sure both files are sorted by start-time
    ;; then loop through each note1 in file1
    (setf duration (first (car (last file1))))
    (loop for note in file1
	  for start = (nth 0 note)
	  for keyname = (nth 1 note)
	  with last-start2 = 0
	  for prob = (fu::envelope-interp (/ start duration) probability-env)
	  for detection = (fu::envelope-interp (/ start duration) detection-range-env)
	  ;; collect all notes in file2 that are played at notes start time
	  do (loop for i from 0 and note2 in (subseq file2 last-start2)
		   for start2 = (first note2)
		   for end2 = (nth 5 note2)
		   while (< start2 start)
		   do
		      (when (> end2 start)
			(push note2 harmony))
		   finally (incf last-start2 i))
	     (setf harmony (remove start harmony :test (lambda (x y) (>= x (nth 5 y)))))
	     ;; decide wheter note is changed, then collect
	     ;; collect
	     (push (if (and harmony
			    (< (get-next ran-nb) prob))
		       ;; select closest pitch from harmony
		       (let* ((harm (loop for i in harmony collect (nth 1 i)))
			      (intervals (mapcar (lambda (x) (abs (- x keyname))) harm))
			      (min (apply #'min intervals))
			      (closest (nth 1 (nth (position min intervals) harmony))))
			 ;; if closest is in range, change
			 (if (<= min detection)
			     (replace note (list closest) :start1 1)
			     note))
		       ;; use original pitch
		       note)
		   result)
	    ;; (format t "~&prob: ~a" prob)
	  )
    ;; sort for time (for cm), then write into midi
    (setf result
	  (sort result #'(lambda (x y) (< (first x) (first y))))
	  events
	  (loop for note in result appending
				   (cm::output-midi-note
				    (nth 1 note)
				    0
				    (nth 0 note)
				    (nth 3 note)
				    (nth 2 note)
				    (nth 4 note))))
    (cm::events
     (cm::new cm::seq :name (gensym) :time 0.0 :subobjects events)
     name
     :tempo 60)))

;; EOF midi.lsp
