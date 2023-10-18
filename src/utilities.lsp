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

;; *** load-from-file
(defmethod load-from-file (file)
  (eval (read-from-file file)))

;; *** start-osc
;;; simple function to open the osc-call
(defun start-osc (&optional ip port)
  (when ip (unless (vectorp ip)
	     (error "ip must be a vector of form #(192 168 56 1) but is: ~a" ip)))
  (osc-call :send-ip (or ip #(192 168 56 1)) :listen-port (or port 5000) :send-port (or port 5000)))

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
;;; this was added to slippery chicken!
#|
(defun decider (chooser ls)
  (labels ((helper (chooser ls1 index sum)
	     (cond ((null ls1) (1- (length ls)))
		   ((< chooser sum) index)
		   (t (helper chooser
			      (cdr ls1)
			      (+ index 1)
			      (+ sum (rationalize (car ls1))))))))
    (helper (sc::rescale (rationalize chooser) 0 1 0 (loop for i in ls sum
							  (rationalize i)))
	    (cdr ls) 0 (rationalize (car ls)))))
|#

;; *** remove-nth
;;; remove nth element from list
(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

;; *** rotate
;;; rotate a list so that it starts with the specified index.
(defun rotate (ls &optional (new-start-index 1))
  (let* ((len (length ls))
         (effective-index (mod new-start-index len)))
    (append (subseq ls effective-index)
            (subseq ls 0 effective-index))))

;; *** re-order
;;; take a list and a new-order as input. The latter should be a list of
;;; indices, which will then be used to re-order the first list. If the 
;;; new-order list is not long enough, all elements that have not been used will
;;; be appended. If some index in new-order is too big, the mod will be taken.
;;; If the same index appears twice, the element following the already chosen 
;;; one will be taken.
;;; (re-order '(a b c d e) '(4 1 3 2 0)) => '(e b d c a)
;;; (re-order '(a b c d e) '(3 4 1)) => '(d e b a c)
;;; (re-order '(a b c d e) '(2 3 4 6 0 1)) => '(c d e b a)
;;; (re-order '(a b c d e) '(0 3 7 0 7 3 5)) => '(a d c b e)
;;; (re-order '(a b c d e) '(0 0 0)) => '(a b c d e)
(defun re-order (ls new-order)
  (let* ((len (length ls))
	 (all-i (loop for i from 0 below len collect i)))
    (append
     (loop repeat len for index in new-order for j from 0 collect
	  (do ((n (mod index len) (mod (+ n 1) len)))
	      ((member n all-i)
	       (progn (when (member n all-i) (setf all-i (remove n all-i)))
		      (nth n ls)))))
     (loop for n in all-i collect (nth n ls)))))

;; *** depth
;;; the depth of a (possibly) nested list
(defun depth (ls)
  (labels ((iter (ls level)
		 (if (listp ls)
                     (let ((sublevels '()))
                       (dolist (s ls)
			 (push (iter s (1+ level)) sublevels))
                       (apply #'max (cons level sublevels)))
                     level)))
    (iter ls 0)))

;; *** rqq-depth
(defun rqq-depth (ls)
  (apply #'max (labels ((iter (ls level)
			      (loop for div in ls append
				   (if (listp div)
				       (iter (second div) (1+ level))
				       (list level)))))
		 (iter (second ls) 0))))

;; *** normalize-depth
;;; Modifies a list, wrapping parts of it in new lists,
;;; so that every element is at uniform depth.
(defun normalize-depth (ls &optional (in-place t))
  (unless in-place (setq ls (copy-seq ls)))
  (let ((max-depth (loop for element in ls maximize (depth element))))
    (loop for i below (length ls)
       do (loop while (< (depth (nth i ls)) max-depth)
             do (setf (nth i ls) (list (nth i ls)))))
    (loop for element in ls
       do (when (listp element)
            (normalize-depth element)))
    ls))

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
(defun get-start-times (list-of-durations)
  (loop for i in list-of-durations
     and sum = 0 then (+ sum i)
     collect sum))

;; *** get-durations
;;; collect the durations in a certain section of a certain layer
(defun get-durations (list-of-start-times)
  (let ((ls (avoid-repetition (sort list-of-start-times #'<) t)))
    (loop for time in (cdr ls)
       with last = (car ls)
       collect (- time last)
       do (setf last time))))

;; *** get-durations-within
;;; collect the durations in a certain section of a certain layer
(defun get-durations-within (durations min max)
  (loop for i in durations sum i into sum while (< sum max)
     when (> sum min) collect i))

;; *** get-start-times-within
;;; collect the start-times in a certain section of a certain layer
(defun get-start-times-within (durations min max)
  (loop for i in durations sum i into sum while (< sum max)
     when (> sum min) collect sum))

;; *** scale-list-to-sum
;;; scale the elements in a list so that the sum of that list is *argument*
(defun scale-list-to-sum (ls &optional (target-sum 1))
  (let* ((sum (loop for i in ls sum (rationalize i)))
	 (scaler (/ target-sum sum)))
    (loop for i in ls collect (* i scaler))))

;; *** avoid-repetition
;;; in a list of elements, when an element appears twice in a row it is cut
(defun avoid-repetition (ls &optional within-tolerance?)
  (loop for i in ls with last unless (if within-tolerance?
					 (equal-within-tolerance last i 1.0d-5)
					 (equal last i))
     collect i do (setf last i)))

;; *** insert
;;; insert an element into list at index
(defun insert (ls index newelt)
  (if (= 0 index) (push newelt ls) (push newelt (cdr (nthcdr (1- index) ls))))
  ls)

;; *** insert-multiple
;;; insert multiple elements on multiple indices in a list
(defun insert-multiple (ls index-list newelt-list)
  (let* ((len (length newelt-list)))
    (unless (= (length index-list) len)
      (error "both index-list and newelt-list in insert-multiple should have ~
              the same length"))
    (loop for i from 0 and el in ls
       when (member i index-list) append
	 (loop for j in index-list and new in newelt-list
	    when (= i j) collect new)
       collect el)))

;; *** dry-wet
(defun dry-wet (dry wet mix)
  (let ((mix (min 1 (max mix 0))))
    (+ (* wet mix)
       (* dry (- 1 mix)))))

;; *** soundfile-duration
;;; get the duration of a soundfile in seconds
(defun soundfile-duration (path)
  (clm::sound-duration path))

;; *** soundfile-framples
;;; get the number of framples (number of samples / channels)
(defun soundfile-framples (path)
  (clm::sound-framples path))

;; *** soundfile-samplerate
;;; samplerate of the soundfile
(defun soundfile-samplerate (path)
  (clm::sound-srate path))

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
;;; using averages
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

;; *** get-beat-prox
;;; check how close a number is to a "beat" aka an even subdivision of 1
;;; => As I now know, this is basically a variation on Thomae's function,
;;; but with a set amount of levels and inverted weighting (ON the beat = 0)
;;; (loop for i from 0 to 1 by .125 collect (get-beat-prox i))
;;; => (0 3 2 3 1 3 2 3 0)
;;;
;;; (visualize (loop for i from 0 to 1 by .01 collect (get-beat-prox i 7)))
;;; " _ _   _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _   _ _" 
;;; "  _   _   _   _   _   _   _   _   _   _   _   _   _   _   _   _ " 
;;; "                                                                " 
;;; "    __      _       _       _       _       _       _      __   " 
;;; "        _               _               _               _       " 
;;; "                _                               _               " 
;;; "                                                                " 
;;; "                                _                               " 
;;; "_                                                               "
(defun get-beat-prox (i &optional (how-many-levels 4))
  ;;(unless (<= 0 i 1) (error "i must be between 0 and 1, not ~a" i))
  (unless (and (integerp how-many-levels) (< 1 how-many-levels 100))
    (error "silly value for how-many-levels: ~a" how-many-levels))
  (round (log (denominator
	       (rational
		(/ (round (* (mod i 1) (expt 2 (1- how-many-levels))))
		   (expt 2 (1- how-many-levels)))))
	      2)))

;; alias
(setf (symbol-function 'beat-proximity) #'get-beat-prox)
(setf (symbol-function 'thomaes-function) #'get-beat-prox)


;; *** rqq-to-indispensability and rqq-to-indispensability-function
;;; This is baiscally a really flexible and complicated version of
;;; get-beat-prox, inspired by Clarence Barlows rhythm theory of
;;; indispensibility and Marc Evans implementation of it.
;;; Instead of repeatedly splitting a bar into halfs, as get-beat-prox does
;;; it, you can specify any division of a bar by using rqq notation.
;;; Also, every beat (or division) has a unique weight.
;;; Compare this:
;;;
;;; (loop for i from 0 to 1 by .125 collect (get-beat-prox i))
;;; => (0 3 2 3 1 3 2 3 0)
;;;
;;; (loop for i from 0 to 1 by 0.125 collect
;;;      (funcall (rqq-to-indispensability-function
;;;		'(2 ((2 ((2 (1 1)) (2 (1 1)))) (2 ((2 (1 1)) (2 (1 1))))))) i))
;;; => (0 7 3 5 1 6 2 4 0)
;;;
;;; (loop for i from 0 to 1 by 0.125 collect 
;;;	 (funcall (rqq-to-indispensability-function 
;;;		   '(8 (1 1 1 1 1 1 1 1))) i))
;;; => (0 7 6 5 4 3 2 1 0)
;;;
;;; when step-function? is t, don't interpolate between values
;;;
;;; RETURNS a function, whose input should be a number
(defun rqq-to-indispensability-function (rqq &optional step-function?)
  (let* ((dur-list (mapcar #'(lambda (x) (/ 1 x))
			   (mapcar #'parse-rhythm-symbol
				   (remove 'sc::{
					   (remove 'sc::}
						   (remove '- (rqq-divide rqq)))))))
	 (sum  (loop for i in dur-list sum i))
	 (indis (rqq-to-indispensability rqq))
	 (env '()))
    (setf dur-list (mapcar #'(lambda (x) (/ x sum)) dur-list))
    (setf env (loop for dur in (append dur-list '(0))
		 and y in (append indis (first (list indis))) 
		 collect x collect y sum dur into x))
    (if step-function?
	(lambda (x) (nth (decider (mod x 1) dur-list) indis))
	(lambda (x) (envelope-interp (mod x 1) env)))))

(defun indispensability-enumerate (ls)
  (let* ((fls (flatten ls))
	 (new-ls (copy-list fls))
	 (len (length fls))
	 (mx (apply #'max fls)))
    (loop for i from 0 to mx
       with backwards
       with cnt
       with n = 0 do
	 (setf cnt (count i fls))
	 ;; Stellschraube: wann backwards t
	 (setf backwards (> i 0))
	 (if (<= cnt 2)
	     (loop for el in (if backwards (reverse fls) fls)
		for j from 0 do
		  (when (= i el)
		    (setf (nth (if backwards (- (1- len) j) j) new-ls) n)
		    (incf n)))
	     (loop for el in (reverse fls)
		for j downfrom (1- len)
		with k = 0
		with m = n do
		  (when (= i el)
		    (setf (nth j new-ls)
			  (if (and (not backwards)
				   (= k (1- cnt)))
			      m
			      (if backwards n (1+ n))))
		    (incf n)
		    (incf k)))))
    new-ls))

(defun rqq-to-indispensability (rqq)
  (labels ((iter (ls)
		 (let* ((len (length ls))
			(ls (if (<= len 2)
				(loop for el in ls with i = 0 collect
				     (if (listp el)
					 (iter (second el))
					 (prog1 i (incf i)))) ; incf always?
				(loop for el in ls
				   for k from 0
				   for i = 0 then (- len k)
				   collect
				     (if (listp el)
					 (iter (second el))
					 i)))))
		   (indispensability-enumerate ls))))
    (iter (second rqq))))

;; *** list-interp
;;; looks at a list as a series of y values, spaced equally from 0 to max-x
;;; give any rational x to return interpolated result between two y values
(defun list-interp (x list &optional (max-x (1- (length list))))
  (let* ((len (1- (length list)))
	 last-y
	 next-y
	 inter)
    (cond ((<= x 0) (first list))
	  ((>= x max-x) (car (last list)))
	  (t (setf last-y (nth (floor (* (/ x max-x) len)) list)
		   next-y (nth (ceiling (* (/ x max-x) len )) list)
		   inter (mod (* (/ x max-x) len) 1))
	     (+ (* inter next-y)
		(* (- 1 inter) last-y))))))

;; *** make-list-into-function
;;; curried list-interp
(defun make-list-into-function (list duration) ; &optional last-at-what-x)
  (lambda (x) (list-interp x list duration)))

;; alias
(setf (symbol-function 'list-to-function) #'make-list-into-function)

;; *** lists-to-envelopes
;;; combine a list of x-values and one of y-values into an envelope
(defun lists-to-envelopes (x-list y-list)
  (unless (= (length x-list) (Length y-list))
    (error "the lists given to lists-to-envelopes should be of the same length ~
           but are of length: ~a and ~a" (length x-list) (length y-list)))
  (let* ((envelopes '()))
    (loop for x in x-list and y in y-list with new-env with last-x = 0 do
	 (when (< x last-x)
	   (push (reverse new-env) envelopes)
	   (setf new-env '()))
	 (push x new-env)
	 (push y new-env)
	 (setf last-x x)
       finally (push (reverse new-env) envelopes))
    (reverse envelopes)))

;; *** make-function-into-env
(defun make-function-into-env (function &optional (from 0) (to 1) (step .1))
  (unless (and (numberp from) (numberp to) (numberp step))
    (error "from, to and step in #'make-function-into-env must be a number"))
  (unless (<= from to)
    (error "'from' should be <= than 'to' but they are: ~a ~a" from to))
  (loop for x from from to to by step collect x collect (funcall function x)))

;; alias
(setf (symbol-function 'function-to-env) #'make-function-into-env)

;; *** flatness-of-list
;;; this might not be useful at all for anything but spectra (spectral flatnes)
(defun flatness-of-list (ls)
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
;;; generate a midi file from lists of starting points, length, pitch...
;;; if one list is shorter than others it will be wrapped (mod)
;;; if the start-time-list is empty, (events-update-time) will be called
#+nil(defun lists-to-midi (pitch-list duration-list start-time-list
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

;;; a lot less features than slippery chickens event-list-to-midi-file, but in
;;; return is a lot easier and skipps the generation of an event, which might
;;; be favorable in some cases.
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
		   :dir (or dir *src-dir*)
		   :name (if n
			     (format nil "~a~a~a~a~a" "structure-"
				     (id structure) "-n-" n ".mid")
			     (format nil "~a~a~a" "structure-"
				     (id structure) ".mid")))))

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
(defun max-of-array (array &optional abs?)
  (loop for i across array maximize (if abs? (abs i) i)))

;; *** max-of-array-with-index
;;; max of an array and the index at which it first occurs
(defun max-of-array-with-index (array &optional abs?)
  (loop for i from 0 and el across array with max = 0 with max-i
     when (> (if abs? (abs el) el) max) do (setf max el max-i i)
     finally (return `(,max ,max-i))))

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
;;; array or list as input
(defun visualize (ls &key y-range (start 0) abs (scale t))
  (when (arrayp ls)
    (setf ls (loop for i across ls collect i)))
  (when abs (setf ls (loop for i in ls collect (abs i))))
  (let* ((matrix (make-array '(64 17) :initial-element 0.0))
	 (maxi (apply #'max (mapcar #'abs ls)))
	 (y-range (if y-range y-range
		      (if (= maxi 0) 1 maxi)))
	 (len (length ls))
	 (size (if (or scale (>= (- len start) 64)) 64 (- len start))))
    (loop for i from start below (+ size start) do
	 (loop for j below 17 do
	      (if (= (round (+ (* (/ (nth (mod (floor
						(+ start
						   (if scale
						       (* (/ i size)
							  (- len start))
						       i)))
					       len)
					  ls)
				     y-range)
				  8 (if abs 2 1))
			       (* 8 (if abs 0 1))))
		     j)
		  (setf (aref matrix (- i start) j) 1)
		  (setf (aref matrix (- i start) j) 0))))
    (loop for j downfrom 16 to 0 do
	 (print (apply 'concatenate 'string
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
