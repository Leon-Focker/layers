;; ** Midi
;;;; Collection of some useful midi input, output and transformation functions

(in-package :layers)

;; *** lists-to-midi
;;; generate a midi file from lists of starting points, length, pitch...
;;; if one list is shorter than others it will be wrapped (mod).
;;; Has a lot less features than slippery chickens event-list-to-midi-file, but
;;; in return it's a lot easier and skipps the generation of an event, which
;;; might be favorable in some cases.
;;; pitch-list - A list of either sc-pitches or midi-key-numbers. Can also be a
;;;  list of chords, ie. lists of pitches.

(defmacro lists-to-midi-aux (pitch)
  ``(,(if (numberp ,pitch) ,pitch (note-to-midi ,pitch))
     ,(nth (mod i start-len) start-time-list)
     ,(nth (mod i velo-len) velo)
     ,(nth (mod i duration-len) duration-list)
     ,(nth (mod i chan-len) channel)))

(defun lists-to-midi (pitch-list duration-list start-time-list
		      &key
			velocity-list
			(tempo 60)
			(channel '(0))
			(file (format nil "~a~a" *src-dir*
				      "midi-output.mid")))
  (when (or (null pitch-list) (null duration-list))
    (error "please provide at least one value in the pitch and the duration ~
            lists in lists-to-midi"))
  (unless start-time-list (setf start-time-list
				(get-start-times duration-list)))
  (setf channel (force-list channel))
  (let* ((pitch-len (length pitch-list))
	 (duration-len (length duration-list))
	 (start-len (length start-time-list))
	 (velo (or velocity-list '(0.7)))
	 (velo-len (length velo))
	 (chan-len (length channel))
	 (total (apply #'max `(,pitch-len ,duration-len ,start-len ,velo-len)))
	 (events '()))
    (setf events
	  (sort (loop for i below total
		      for pitch = (nth (mod i pitch-len) pitch-list)
		      collect (lists-to-midi-aux
			       (if (listp pitch) (car pitch) pitch))
		      when (listp pitch)
			append (loop for p in (cdr pitch)
				     collect (lists-to-midi-aux p)))
		#'(lambda (x y) (< (second x) (second y))))
	  events (loop for event in events
		       appending
		       (cm::output-midi-note
			(pop event)
			0
			(pop event)
			(pop event)
			(pop event)
			(pop event))))
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
;;; Also with longer midi files there seems to be some minor timing errors that
;;; build up? Probably stems from the cm::import-events funktion
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


;; *** find-notes-with-cursor
;;; return all notes (lists with (start key duration...),
;;; that play during the time of cursor.
(defun find-notes-with-cursor (cursor list-of-notes)
  (loop for note in list-of-notes
	when (<= (first note) cursor (+ (first note) (nth 2 note)))
	  collect note))

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
			 (file (format nil "~afiltered_file.mid" *src-dir*))
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
	  for prob = (interpolate (/ start duration) probability-env)
	  for detection = (interpolate (/ start duration) detection-range-env)
	  ;; collect all notes in file2 that areplayed at notes start time
	  do (loop for i from 0 and note2 in (subseq file2 last-start2)
		   for start2 = (first note2)
		   for end2 = (nth 5 note2)
		   while (< start2 start)
		   do
		      (when (> end2 start)
			(push note2 harmony))
		   finally (incf last-start2 i))
	     (setf harmony (remove start harmony
				   :test (lambda (x y) (>= x (nth 5 y)))))
	     ;; decide wheter note is changed, then collect
	     (push (if (and harmony
			    (< (get-next ran-nb) prob))
		       ;; select closest pitch from harmony
		       (let* ((harm (loop for i in harmony collect (nth 1 i)))
			      (intervals (mapcar (lambda (x) (abs (- x keyname)))
						 harm))
			      (min (apply #'min intervals))
			      (closest (nth 1 (nth (position min intervals)
						   harmony))))
			 ;; if closest is in range, change
			 (if (<= min detection)
			     (replace note (list closest) :start1 1)
			     note))
		       ;; use original pitch
		       note)
		   result))
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
     file
     :tempo 60)))

;; *** deconstruct-chords
;;; Takes a midi file as input, which should consist of chords or some kind of
;;; polyphonie. This function then deconstructs that file, using the
;;; rhythmic-pattern.
;;; file - midi file with chords to deconstruct
;;; rhythmic-pattern - list of durations als rhythms, ie. '(2 1 (2)). Durations
;;;  in parens are rests.
;;; longest-duration - if any duration in the final file would be longer that
;;;   this, it will be replaced with a nested version of the pattern.
;;;   (see #'nested-pattern)
;;; density-env - an envelope with x and y values from 0 to 1. 0 Meaning only
;;;   one note will be chose, 1 meaning, all notes will be chosen.
;;; stay-on-note - when true, try to use notes that appeared in the last chord.
;;; TODO: chosing the pitches is a bit trivial and might need a way to vary it.
(defun deconstruct-chords
    (midi-file rhythmic-pattern longest-duration density-env stay-on-note
     &key midi-track)
  (let* ((chords (midi-file-to-list midi-file midi-track))
	 first-note
	 last-note
	 (duration 1)
	 (new-notes '()))
    ;; sort and set first and last note
    (setf chords (sort chords #'(lambda (x y) (< (car x) (car y))))
	  first-note (first chords)
	  last-note (car (last chords))
	  duration (- (+ (car last-note) (nth 2 last-note)) (car first-note))
	  rhythmic-pattern (scale-pattern rhythmic-pattern duration)
	  rhythmic-pattern (nested-pattern rhythmic-pattern longest-duration))
    
    ;; divide chords into the rhythmic pattern and look which notes are available
    (flet ((pred (last)
	     "This sorts items to the front, if they are found in 'last' "
	     (lambda (x y) (or (find x last)
			  (and (not (find x last)) (not (find y last))))))
	   (density-aux (d len) (1+ (round (* (max (min d 1) 0) (1- len))))))
      (loop for rhythm in rhythmic-pattern
	    with time = (car first-note)
	    for duration = (if (listp rhythm) (car rhythm) rhythm)
	    for i from 0
	    for notes = (find-notes-with-cursor time chords)
	    for how-many-pitches = (density-aux (interpolate
						 (/ i (length rhythmic-pattern))
						 density-env)
						(length notes))
	    do (when (and notes (atom rhythm))
		 ;; sort notes for priorities:
		 (setf notes (sort notes (pred (third (first new-notes)))))
		 (when stay-on-note (setf notes (reverse notes)))
		 ;; collect notes
		 (push `(,time ,duration ,(loop repeat how-many-pitches
						collect (second (pop notes))))
		       new-notes))
	       (setf time (+ time duration))
	    finally (setf new-notes (reverse new-notes))))
    ;; write to midi fil
    (lists-to-midi (loop for i in new-notes collect (nth 2 i))
		   (loop for i in new-notes collect (second i))
		   (loop for i in new-notes collect (first i))
		   :file (format nil "~a~a~a"
				 (directory-namestring midi-file)
				 (pathname-name midi-file)
				 "-deconstructed.mid"))))

;; *** midi-file-to-env

;;; convert a midi file into a pair of x-y breakpoints. The start-time of the
;;; first note corresponds to the first x-value (0). The start-time of the last
;;; note corresponds to the last x-value (100).
;;; The midi keynum values correspond to the y-values. The lowest one will be 0,
;;; the highest 1.
(defun midi-file-to-env (file &optional track)
  (let ((lists (midi-file-to-list file track))
	first
	last
	lowest
	highest
	(x-values '())
	(y-values '()))
    ;; sort by start-time
    (setf lists (sort lists #'(lambda (x y) (< (first x) (first y)))))
    ;; init all missing variables
    (loop for event in lists
	  for time = (first event)
	  for key = (second event)
	  minimize time into t-min
	  maximize time into t-max
	  minimize key into k-min
	  maximize key into k-max
	  do (push time x-values)
	     (push key y-values)
	  finally (setf first t-min
			last t-max
			lowest k-min
			highest k-max))
    (reverse
     (loop for x in x-values and y in y-values
	   ;; in reverse, because we reverse again in the end
	   collect (rescale y lowest highest 0 1)
	   collect (rescale x first last 0 100)))))

(setf (symbol-function 'midi-to-env) #'midi-file-to-env)

;; EOF midi.lsp
