  #|(defpackage :layers
  (:use :cl))
(in-package :layers)|#
(in-package :sc)
(auto-set-default-dir)

;;;; To do:
;;;; check reset (?) - layer-objects are updated - layers-ojects are not
;;;; variable rest probability multiplier?
;;;; create template
;;;; change probabilities depending on Formteil
;;;; multichannel?
;;;; one layer could influence another (for example by appending markov lists)
;;;; consider binding structure - n to sfl, not a layer - or leave it be?
;;;; build "sets" for songs, smoothly switch between sets, live transistions
;;;; one-time-use of samples? (eg. sample can only be played once every 3 mins)
;;;; dynamic panning
;;;; reseting should reset n of a layer as well
;;;; set-n looks for the layer in layers by id. abstract that function and
;;;; build in an error case (not found)
;;;; when markov-chain has no weight there is no error message
;;;; when changing n of a layer whilst playing (set-n). each time a slight delay
;;;; is added to the layer (something aroung 20 ms). Probably happens in PD. where?
;;;; PD needs to be restarted after every reload. else some timings seem to be
;;;; getting stuck. pls fix :c

;; * Layers
#|||||||||||||||||||||||||||||||||||#
;; ** global parameters
#|||||||||||||||||||||||||||||||||||#

(defparameter *total-length* 300)
(defparameter *seed* 5)
(defparameter *random-number* nil)
(defparameter *pd-on-windows* t) ;; is your pd-version running on windows?
(defparameter *default-dir* (parent-dir
			     (parent-dir (get-sc-config 'default-dir))))
(defparameter *default-sample-dir* (format nil "~a~a" *default-dir* "/samples"))
;; what is the maximum length for the smallest value in the structure?
(defparameter *max-smallest-sample-length* 0.003)
(defparameter *loop* nil)
(defparameter *start-stop* t)
;; when true, the layer decides the panorama value for all files played in it
;; when false, each file is panned according to its panorama value
(defparameter *use-pan-of-layer* t)
(defparameter *use-preferred-lengths* t)
(defparameter *score-file* *load-pathname*) ;; set which file to reload to reset
(defparameter *load-risky-files* nil) ;; load clm and sketch,creates warnings :(
(defparameter *debug* '()) ;; hopefully not needed for now

#|||||||||||||||||||||||||||||||||||#
;; ** utilities
#|||||||||||||||||||||||||||||||||||#

(when *load-risky-files* (load-from-same-dir "export-with-clm.lsp"))

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
			(make-event pitch
				    duration
				    :start-time start-time
				    :duration t
				    :amplitude velocity)))))
    (unless start (events-update-time events))
    (event-list-to-midi-file
     events
     :start-tempo tempo
     :midi-file (format nil "~a~a~a" (or dir *default-dir*) "/" name))))

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

#|||||||||||||||||||||||||||||||||||#
;; ** base-object
#|||||||||||||||||||||||||||||||||||#

;; *** base-object
;;; as simple as it gets
(defclass base-object ()
  ((id :accessor id :initarg :id :initform nil)
   (data :accessor data :initarg :data :initform nil)))

;; *** get-id
;;; get the id of an object
(defmethod get-id ((bo base-object))
  (id bo))

#|||||||||||||||||||||||||||||||||||#
;; ** random
#|||||||||||||||||||||||||||||||||||#

;; *** random-number
;;; stores current pseudo random number and can get the next one
(defclass random-number (base-object)
  ((data :accessor data :initarg :data :type float :initform *seed*)
   (pcg :accessor pcg :initarg :pcg :initform nil)))

;; *** random-number
;;; make an instance of random-number
(defun random-number (&optional seed)
  (make-instance 'random-number
		 :data (if seed seed *seed*)
		 :pcg (pcg::make-pcg :seed (if seed seed *seed*))))

;;; set the global variable *random-number* to an instance of random-number
(setf *random-number* (random-number))

;;; get current value of a random-numer object
(defmethod get-number ((rn random-number))
  (data rn))

;; *** get-next
;;; get a random-number object to the next pseudo random number
(defmethod get-next ((rn random-number))
  (setf (data rn)
	(pcg::pcg-random (pcg rn) 1.0)))

;; *** set-state
;;; set the random-state of the prng
(defmethod set-state ((rn random-number) state)
  (prog1 (setf (pcg::pcg-state (pcg rn))
	       state)
    (get-next rn)))


#|||||||||||||||||||||||||||||||||||#
;; ** list-object
#|||||||||||||||||||||||||||||||||||#

;; *** list-object
;;; just a very basic list object with basic methods
(defclass list-object (base-object)
  ((current :accessor current :initarg :current :type integer :initform 0)))

;; *** get-ids
;;; get the ids of all elements in the list
(defmethod get-ids ((lo list-object))
  (loop for element in (data lo) collect (id element)))

;; *** get-current
;;; get current element of list
(defmethod see-current ((lo list-object))
  (when (data lo)
    (nth (current lo) (data lo))))

;; *** get-next
;;; proceed to next element of the list
(defmethod get-next ((lo list-object))
  (when (data lo)
    (let* ((data (data lo)))
      (progn
	(incf (current lo))
	(when (= (current lo) (length data))
	  (setf (current lo) 0))
	(nth (current lo) data)))))

;; *** see-next
;;; get next element in a list but don't change the current-slot
(defmethod see-next ((lo list-object))
  (when (data lo)
    (let* ((data (data lo))
	   (n (current lo)))
      (progn (incf n)
	     (when (= n (length (data lo)))
	       (setf n 0))
	     (nth n data)))))

#|||||||||||||||||||||||||||||||||||#
;; ** markov
#|||||||||||||||||||||||||||||||||||#

;; *** markov-list
;;; list of pairs of soundfile-ids and allocated relative odds
(defclass markov-list (list-object)
  ())

;; *** make-markov-list
;;; make an instance of markov-list
(defun make-markov-list (id ml)
  (make-instance 'markov-list :data ml :id id))

;;; example:
#|(make-markov-list 'test
		  '(("/E/test.wav" 5)
		    ("/E/test1.wav" 2)
		    ("/E/test2.wav" 7)))|#

;; *** decide-for-snd-file
;;; returns the car of the chosen sublist of a markov-list object
;;; (the id of the next soundfile)
(defmethod decide-for-snd-file ((ml markov-list) random-number)
  (car (nth (decider
	     random-number
	     (loop for i in (data ml) collect (cadr i)))
	    (data ml))))


#|||||||||||||||||||||||||||||||||||#
;; ** length-dependant-list
#|||||||||||||||||||||||||||||||||||#

;; *** length-dependant-list
;;; list of pairs of soundfile-ids and allocated lengths
;;; -> the length of the currently played soundfile decides the next file
(defclass length-dependant-list (list-object)
  ())

;; *** make-length-dependant-list
;;; make an instance of length-dependant-list
(defun make-length-dependant-list (id ldl)
  (make-instance 'length-dependant-list :data ldl :id id))

;;; example:
#|(make-length-dependant-list 'test
		  '(("/E/test.wav" 0.5)
		    ("/E/test1.wav" 2)
		    ("/E/test2.wav" 10)))|#

;; *** decide-for-snd-file
;;; returns the car of the chosen sublist of a length-dependant-list object
;;; (the id of the next soundfile)
(defmethod decide-for-snd-file ((ldl length-dependant-list) current-length)
  (loop for ls in ldl do
       (when (> current-length (cadr ls)) (return (car ls)))))

#|||||||||||||||||||||||||||||||||||#
;; ** structure
#|||||||||||||||||||||||||||||||||||#

;; *** scale-structure
;;; scale all values in the recursive structure to wanted length.
;;; returns flattened list
(defun scale-structure (ls target-sum)
  (let* ((ls (sc::flatten ls))
	 (sum-ls (loop for i in ls sum i)))
    (loop for i in ls collect
	 (rescale i 0 sum-ls 0 target-sum))))

;; *** lindenmayer
;;; generates a lindenmayer-like recursive list
;;; it's supposed to work in ratios though.
;;; an example of a resulting list could be:
;;; 2nd iteration: '((0.2 1 0.2))
;;; which means, that the second part is 5 times as long as the other ones
;;; 3rd iteration: '(((5) (25 5) (5)))
;;; which normally should be '((5) (5 1) (5)) but the ratios are multiplied!
;;; the second list is 5 times as long as the other ones, see?
;;; 4th iteration: '((((1 5 1)) ((5 25 5) (25 5)) ((1 5 1)))) and so on...
;;; the arguments:
;;; seed - the starting list ("0th iteration")
;;; rules - set of rules for how each element will be substituted next itertion
;;; ratios - the ratio, that each element represents
(defun lindenmayer (total-length seed rules ratios &optional smallest)
  (let* ((result seed)    ; recursive list containing the elements
	 (final 1)        ; recursive list containing the ratios
	 (collector '(1))) ; collect all results in this list
    (labels ((get-lists (res-ls fin-ls) ; define recursive function
	       (if (atom (car res-ls))  ; check if list is still nested
		   ;; if the current res-ls is flat, return two values:
		   (values		    
		    ;; firstly generate the result list,
		    ;; replacing each element according to given rules
		    (loop for element in res-ls collect
			 (caadr (assoc element rules)))
		    ;; secondly generate the final list, by replacing
		    ;; all elements with ratios and scaling them recursively
		    (progn ; needed, since loop itself doesn't return res-ls
		      ;; loop through all elements and check, if they match
		      ;; an element from the replace-list
		      (let ((sum (loop for i in res-ls sum i)))
			(loop for rep in ratios do
			     (setf res-ls 
				   (loop for element in res-ls collect
					(if (eq (car rep)
						element) 
					    (* (cadr rep)
					       (/ fin-ls
						  sum)) 
					    element)))))
		      res-ls)) ; by this point it should act. be called final
		   ;; if the list is still nested, loop through al elements:
		   ;; since we're working with a multiple-value bind,
		   ;; this is a bit of a weird loop.
		   ;; first we'll need two helping variables, since loop itself
		   ;; can't return multiple values (if it can, teach me)
		   (let (help1 help2)
		     ;; now loop through the nested res-list
		     (loop for l in res-ls
			;; and through the fin-ls, if it's still nested
			for i from 0
			for b = (if (atom fin-ls) fin-ls (nth i fin-ls))
			;; now we bind the two values that get-lists will return
			do (multiple-value-bind (r f) (get-lists l b)
			     ;; save those values into the helping variables
			     (progn (push r help1)
				    (push f help2))))
		     ;; and finally return the two values
		     (values (reverse help1) (reverse help2))))))
      ;; loop for how many recursions you like (not more than 20 please)
      (loop until break
	 for i from 0
	 with break do	
	   (setf (values result final)	; bind the two returned values
		 (get-lists result final))
	   (push (scale-structure final total-length) collector)
	   (when (or (< (apply #'min (car collector))
			(if smallest
			    smallest
			    *max-smallest-sample-length*))
		     ;; we never want more than 20 recursions, though we will
		     ;; probably never even go beyond 10
		     (> i 20))
	     (setf break t))))
    ;; output
    ;;(format t "~&next result: ~a" result)
    ;;(format t "~&final: ~a" final)
    collector))

;; *** compartmentalise
;;; similar to lindenmayer but 
(defun compartmentalise (total-length seed rules ratios &optional smallest)
  (let* ((result seed)	      ; recursive list containing the elements
	 (final 1)	      ; recursive list containing the ratios
	 (collector '(1)))    ; collect all results in this list
    (labels ((get-lists (res-ls fin-ls) ; define recursive function
	       (if (atom (car res-ls)) ; check if list is still nested
		   ;; if the current res-ls is flat, return two values:
		   (values		    
		    ;; firstly generate the result list,
		    ;; replacing each element according to given rules
		    (loop for element in res-ls collect
			 (caadr (assoc element rules)))
		    ;; secondly generate the final list, by replacing
		    ;; all elements with ratios and scaling them recursively
		    (progn ; needed, since loop itself doesn't return res-ls
		      ;; loop through all elements and check, if they match
		      ;; an element from the replace-list
		      (loop for rep in ratios do
			   (setf res-ls 
				 (loop for element in res-ls collect
				      (if (eq (car rep)
					      element) 
					  (cadr rep)
					  element))))
		      (let ((sum (loop for i in res-ls sum i)))
			(setf res-ls
			      (loop for element in res-ls
				 collect (* element (/ fin-ls sum)))))
		      res-ls)) ; by this point it should act. be called final
		   ;; if the list is still nested, loop through al elements:
		   ;; since we're working with a multiple-value bind,
		   ;; this is a bit of a weird loop.
		   ;; first we'll need two helping variables, since loop itself
		   ;; can't return multiple values (if it can, teach me)
		   (let (help1 help2)
		     ;; now loop through the nested res-list
		     (loop for l in res-ls
			;; and through the fin-ls, if it's still nested
			for i from 0
			for b = (if (atom fin-ls) fin-ls (nth i fin-ls))
			;; now we bind the two values that get-lists will return
			do (multiple-value-bind (r f) (get-lists l b)
			     ;; save those values into the helping variables
			     (progn (push r help1)
				    (push f help2))))
		     ;; and finally return the two values
		     (values (reverse help1) (reverse help2))))))
      ;; loop for how many recursions you like (not more than 20 please)
      (loop until break
	 for i from 0
	 with break do	
	   (setf (values result final)	; bind the two returned values
		 (get-lists result final))
	   (push (scale-structure final total-length) collector)
	   (when (or (< (apply #'min (car collector))
			(if smallest
			    smallest
			    *max-smallest-sample-length*))
		     ;; we never want more than 20 recursions, though we will
		     ;; probably never even go beyond 10
		     (> i 20))
	     (setf break t))))
    ;; output
    ;;(format t "~&next result: ~a" result)
    ;;(format t "~&final: ~a" final)
    collector))

;;; example:
#|(lindenmayer 60
	    '(2)
	    '((1 ((2 1)))
	      (2 ((3 1 3)))
	      (3 ((2))))
	    '((1 1)
	      (2 5)
(3 .2)))|#

;; *** scale-smallest-value-to
;;; intended to be used with structures generated using the same ratio for all
;;; elements in the rule-set (never mind)
;;; eg.: (lindenmayer 60
;;;                   '(2)
;;;                   '((1 ((2 3)))
;;;	                (2 ((1 1 2 1)))
;;;	                (3 ((1))))
;;;	              '((1 1)
;;;	                (2 1)
;;;                     (3 1)))
(defun scale-smallest-value-to (structure new-smallest-value)
  (let* ((data (data structure))
	 (minimum (apply #'min (first data)))
	 (scaler (/ new-smallest-value minimum)))
    (setf (data structure)
	  (loop for ls in data collect
	       (if (atom ls) (* ls scaler 1.0)
		   (loop for i in ls collect (* i scaler)))))
    structure))

;; *** scale-biggest-value-to
(defun scale-biggest-value-to (structure new-smallest-value)
  (let* ((data (data structure))
	 (maximum (apply #'max (first data)))
	 (scaler (/ new-smallest-value maximum)))
    (setf (data structure)
	  (loop for ls in data collect
	       (if (atom ls) (* ls scaler 1.0)
		   (loop for i in ls collect (* i scaler)))))
    structure))
    

;; *** structure
;;; structure-class
(defclass structure (list-object)
  ((seed :accessor seed :initarg :seed)
   (rules :accessor rules :initarg :rules)
   (ratios :accessor ratios :initarg :ratios)))

;; *** make-structure
;;; generate a structure-object
;;; takes several arguments to generate a structure using the lindenmayer fun.
;;; total-length: length of the structure (and piece) in seconds
;;; the higher the n, the lower the recursion
(defun make-structure (seed rules ratios
		       &key id
			    (duration *total-length*)
		            (type 'lindenmayer)
		            (smallest *max-smallest-sample-length*))
  (make-instance 'structure
		 :id id
		 :data (funcall type duration seed rules ratios smallest)
		 :seed seed
		 :rules rules
		 :ratios ratios))

;; *** re-gen-structure
;;; in case the *total-length* changed,
;;; this function will generate a new structure, based on the initial arguments
(defmethod re-gen-structure ((st structure))
  (setf (data st)
	(lindenmayer *total-length* (seed st) (rules st) (ratios st))))

;; example:
(defparameter *structure*
  (make-structure '(2)
		  '((1 ((2 1)))
		    (2 ((3 1 3)))
		    (3 ((2))))
		  '((1 1)
		    (2 5)
		    (3 .2))))

;; *** length-list
;;; a structure is a list of length-lists, for the length list, we choose one
;;; of those lists
(defclass length-list (list-object)
  ((structure :accessor structure :initarg :structure :initform nil)))

;; *** make-length-list
;;; initialize a length-list object
(defmethod make-length-list ((st structure) &optional (n 0) (current 0))
  (make-instance 'length-list
		 :data (nth n (data st))
		 :structure st
		 :current current))

;; *** get-next-by-time
;;; don't just get the next value in the list, use decider and the current time
;;; then go by current time use 
(defmethod get-next-by-time (current-time (ll length-list))
  (when (data ll)
    (let* ((data (data ll)))
      (progn
	(setf (current ll)
	      (decider (mod (/ current-time *total-length*) 1.0) data))
	(when (= (current ll) (length data))
	  (setf (current ll) 0))
	;;(format t "~&index: ~a~&current: ~a" (current ll) current-time)
	(nth (current ll) data)))))

;; *** visualize-structure
;;; use the sketch library to visualize the structure. bit crooked
(when *load-risky-files*
  (load-from-same-dir "show-structure.lsp")
  (defmethod visualize-structure ((st structure))
    (show-structure::show-structure (reverse (cdr (reverse (data st)))))))

#|||||||||||||||||||||||||||||||||||#
;; ** soundfiles
#|||||||||||||||||||||||||||||||||||#

;; *** stored-file
;;; a file in the data-base, with certain properties, eg. a markov-list,
;;; which links it to other soundfiles
(defclass stored-file (base-object)
  ((name :accessor name :initarg :name :initform nil)
   (path :accessor path :initarg :path :initform nil)
   (markov-list :accessor markov-list :initarg :markov-list
		:initform nil)
   ;; this helps decide the next file dependant on the current play-length
   (length-dependant-list :accessor length-dependant-list
			  :initarg :length-dependant-list
			  :initform nil)
   (preferred-length :accessor preferred-length :initarg :preferred-length
		     :initform nil)
   (duration :accessor duration :initarg :duration :initform 0)
   (start :accessor start :initarg :start :initform 0)
   (amplitude :accessor amplitude :initarg :amplitude :initform 1)
   (loop-flag :accessor loop-flag :initarg :loop-flag :initform nil)
   (decay :accessor decay :initarg :decay :initform 0)
   (panorama :accessor panorama :initarg :panorama :initform 45)))

;; *** make-stored-file
;;; create an instance of stored-file and config with markov-list etc.
(defun make-stored-file (id path-from-default-dir markov-list decay
			 &key
			   (directory *default-sample-dir*)
			   (start 0)
			   (amplitude 1)
			   (panorama 45)
			   (loop-flag nil)
			   (preferred-length nil))
  (let* ((path (format nil "~a~a" directory path-from-default-dir)))
    (unless (probe-file path)
      (warn "~&the file with path ~a does not exist" path))
    (make-instance 'stored-file
		 :id id
		 :name (pathname-name path-from-default-dir)
		 :path path
		 :decay decay ; in seconds
		 :markov-list (make-markov-list nil markov-list)
		 :duration (soundfile-duration path)
		 :start start
		 :amplitude amplitude
		 :panorama panorama
		 :loop-flag loop-flag
		 :preferred-length preferred-length)))

;; *** setf-markov
;;; sets the markov list of a stored-file
(defmethod setf-markov ((sf stored-file) new-markov-list)
  (setf (markov-list sf) new-markov-list))

;;; example
(make-stored-file
 'noisy1
 "/rhythmic/noisy/1.wav"
 '((noisy1 1)
   (noisy2 1)
   (noisy3 1)
   (noisy4 1)
   (noisy5 1))
 0)

;; *** stored-file-list
;;; list of stored files, no doubles
(defclass stored-file-list (list-object)
  ;; this helps to switch sfls dependant on the next play-length
  ((length-min :accessor length-min :initarg :length-min :initform 0)
   (length-max :accessor length-max :initarg :length-max :initform 100)
   (sfl-when-shorter :accessor sfl-when-shorter :initarg :sfl-when-shorter
		     :initform nil)
   (sfl-when-longer :accessor sfl-when-longer :initarg :sfl-when-longer
		    :initform nil)
   (last-played :accessor last-played :initarg :last-played :initform nil)))

;; *** make-stored-file-list
;;; make an instance of stored-file-list
(defun make-stored-file-list (id data &key
					(length-min 0)
					(length-max 100)
					sfl-when-shorter
					sfl-when-longer)
  (make-instance 'stored-file-list :id id :data data
		 :length-min length-min
		 :length-max length-max
		 :sfl-when-shorter sfl-when-shorter
		 :sfl-when-longer sfl-when-longer))

;; *** create-rest
;;; make a stored-file-list object representing a rest
(defun create-rest () (make-stored-file 'rest "/rest.wav" '() 0))

;; *** get-ids
;;; get ids of all stored-files in stored-file-list
(defmethod get-ids ((sfl stored-file-list))
  (loop for sf in (data sfl) collect
       (get-id sf)))

;; *** store-file-in-list
;;; stores a stored-file object in a stored-file-list, when its id is unique
(defmethod store-file-in-list ((sf stored-file) (sfl stored-file-list)
			       &key (warn-if-double t))
  (if (null (data sfl))
      (setf (data sfl) (list sf))
      (let* ((double (member (id sf)
			     (get-ids sfl))))
	(if double
	    (progn
	      (when warn-if-double
		(warn (format nil "the id ~a is already in the sfl ~a ~
                                   and will be replaced"
			      (get-id sf) (get-id sfl))))
	      (setf (data sfl)
		    (remove-nth (- (length (data sfl))
				   (length double))
				(data sfl)))
	      (push sf (data sfl)))
	    (push sf (data sfl)))))
  (unless (last-played sfl) (setf (last-played sfl) sf)))

;; *** get-paths
;;; show all paths to all sounds on the sfl
(defmethod get-paths ((sfl stored-file-list))
  (loop for p in (data sfl) collect (path p)))

;; *** folder-to-stored-file-list
;;; bunch-add all soundfiles in a folder to a stored-file-list
(defmethod folder-to-stored-file-list ((sfl stored-file-list)
				       folder
				       &key
					 id-uniquifier
					 markov-list
					 (start 0)
					 (decay 0) ; in seconds
					 (panorama 45)
					 loop-flag
					 (path-to-folder ""))
  (let* ((dir (format nil "~a~a" path-to-folder folder))
	 (files (sc::get-sndfiles dir))
	 (names (loop for i in files collect (pathname-name i)))
	 (ids (loop for name in names collect
		   (read-from-string (if id-uniquifier
					 (format nil "~a-~a" id-uniquifier name)
					 name)))))
    (when (null files) (warn "no soundfiles found in ~a" dir))
    (loop for file in files and name in names and id in ids do
	 (store-file-in-list
	  (make-stored-file
	   id
	   file
	   (loop for i in ids
	      for markov = (assoc i markov-list)
	      collect
		(list i (if markov
			    (cadr markov)
			    1)))
	   decay
	   :start start
	   :directory ""
	   :panorama panorama
	   :loop-flag loop-flag)
	  sfl))))

;; *** set-alternate-sfls
;;; set a few slots important for switching betweend sfls in a layer
;;; -> when the next cue is too short or too long for the current sfl,
;;; which sfl is next?
(defmethod set-alternate-sfls ((sfl stored-file-list)
			       length-min
			       length-max
			       sfl-when-shorter
			       sfl-when-longer)
  (setf (length-min sfl)
	length-min
	(length-max sfl)
	length-max
	(sfl-when-shorter sfl)
	sfl-when-shorter
	(sfl-when-longer sfl)
	sfl-when-longer))

(defparameter *stored-file-list* (make-stored-file-list 'test nil))

;; *** combine-stored-file-lists
;;; combine two stored-file-lists into a new one
(defmethod combine-stored-file-lists ((sfl1 stored-file-list)
				      (sfl2 stored-file-list)
				      new-id)
  (make-stored-file-list
   new-id
   (append (data sfl1) (data sfl2))
   :length-min (length-min sfl1)
   :length-max (length-max sfl1)
   :sfl-when-shorter (sfl-when-shorter sfl1)
   :sfl-when-longer (sfl-when-longer sfl1)))
		  
#|||||||||||||||||||||||||||||||||||#
;; ** layer
#|||||||||||||||||||||||||||||||||||#

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
   (n-for-length-list :accessor n-for-length-list
		      :initarg :n-for-length-list :type integer)
   (length-list :accessor length-list :initarg :length-list :initform nil)
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
	(length-list ly)
	(make-length-list (structure ly) (n-for-length-list ly))))

;; *** make-layer
;;; create a layer-object
(defun make-layer (id stored-file-list structure &optional (n 0) (panorama 45)
						   (use-pan-of-layer t))
  (make-instance 'layer
		 :id id
		 :stored-file-list stored-file-list
		 :structure structure
		 :n-for-length-list n
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
;;; then checks wheter the preferred-length matches, if no: repeat.
(defmethod determine-new-stored-file ((ly layer))
  (let ((data (data (stored-file-list ly))))
    (loop for snd in data until break
       with break
       with new-id =
       ;; when current file has no length-dependant-list
       ;; a random number and the markov-list will decide.
       ;; else we use the length-dependant-list
	 (if (length-dependant-list (current-stored-file ly))
	     (decide-for-snd-file
	      (length-dependant-list (current-stored-file ly))
	      (this-length ly))
	     (decide-for-snd-file
	      (markov-list
	       (current-stored-file ly))
	      (get-next *random-number*)))
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
  (let ((next-len (see-next (length-list ly))))
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
    (let ((ll (length-list ly)))
      (when (and (= (current ll) (1- (length (data ll)))) (not *loop*))
	(setf (play ly) nil)))
    ;; set time for the next sample and then actually determine the new sample
    (setf
     (last-length ly)
     (this-length ly)
     (this-length ly)
     (get-next-by-time (current-time ly) (length-list ly))
     (current-stored-file ly)
     (determine-new-stored-file ly)
     )))

;; *** update-slots
;;; updates the slots of a layer, when needed
;;; should maybe be called reset-layer?!
(defmethod update-layer ((ly layer))
  (setf (current-stored-file ly)
	(first (data (stored-file-list ly)))
	(last-stored-file ly)
	(current-stored-file ly)
	(length-list ly)
	(make-length-list (structure ly) (n-for-length-list ly))
	(this-length ly)
	(see-current (length-list ly))))

;; *** reset-index
;;; sets current slot of the length-list of a layer back to 0 (start of loop)
(defmethod reset-index ((ly layer))
  (setf (current (length-list ly)) 0)
  (format t "~& current timing index of layer ~a set to ~a"
	  (get-id ly) (current (length-list ly))))

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
	       (string-replace "/E/"
			       "E:/"
			       (path (current-stored-file ly)))
	       (path (if get-next (current-stored-file ly)
			 (last-stored-file ly))))
	   ;; soundfile-length in seconds
	   (this-length ly)
	   ;; start in seconds
	   (mod
	    (if (eq (start (current-stored-file ly)) 'random)
	       (if (> (duration (current-stored-file ly)) (this-length ly))
		   (random (- (duration (current-stored-file ly)) (this-length ly)))
		   0)
	       (start (current-stored-file ly)))
	    (duration (current-stored-file ly)))
	   ;; attack in milliseconds
	   10
	   ;; decay in miliseconds
	   (* 1000 ;; from seconds to miliseconds
	      (let ((max-decay (see-next (length-list ly)))
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

;; *** update-length-list
(defmethod update-length-list ((ly layer) &optional (current 0))
  (setf (length-list ly)
	(make-length-list (structure ly) (n-for-length-list ly) current)))

#|||||||||||||||||||||||||||||||||||#
;; ** layers
#|||||||||||||||||||||||||||||||||||#

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
;;; change the n-for-length-list value even while playing
(defmethod set-n (n layer-id (lys layers) current-time)
  (let* ((ly (loop for layer in (data lys) do
		  (when (eq layer-id (get-id layer))
		    (return layer))))
	 ;; check and see if n is too big, then choose list
	 (ls (progn (when (>= n (length (data (structure ly))))
		      (progn (warn "~&n ~a is too big for structure of layer ~a"
				   n layer-id)
			     (setf n (- (length (data (structure ly))) 1))))
		    (nth n (data (structure ly)))))
	 (len (length ls))
	 (current 0) ;; current for new length-list
	 (new-next-trigger (loop for n from 0 and i = (nth (mod n len) ls)
			  sum i into sum
			  until (> sum current-time)
			  finally (progn (setf current (mod (- n 1) len))
					 (return sum))))
	 (old-next-trigger (current-time ly)))
    ;; when resetting n, we need to tell lisp and the layer that we reset n:
    (setf (n-for-length-list ly) n)
    ;; setting "current" should be obsolete, since the next length
    ;; will be chosen by using the current-time anyways. but for good measure:
    (update-length-list ly current)
    ;; function get-next sets current time of layer to the time
    ;; when the next sample will be triggered. (old-next-trigger)
    ;; reset to new-next-trigger
    (setf (current-time ly)
	  new-next-trigger
	  ;; reset this-length (for next sample)
	  (this-length ly)
	  (get-next-by-time (current-time ly) (length-list ly))
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
	 (current 0) ;; current for new length-list
	 (last-trigger (loop for n from 0 and i = (nth (mod n len) ls)
			  sum i into sum
			  until (> sum current-time)
			  finally (progn (setf current (mod (- n 1) len))
					 (return (- sum i))))))
    (prog2
	(progn (setf (n-for-length-list ly) n)
	       (update-length-list ly current)
	       (setf (this-length ly) (see-current (length-list ly)))
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
  (declare (special *layers*))
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
(defun reload-layers ()
  (load *score-file*)
  nil)

;; *** get-ids
;;; get ids of all stored-files in stored-file-list
(defmethod get-ids ((lys layers))
  (loop for ly in (data lys) collect
       (get-id ly)))

;;; examples:

(defparameter *layer1* (make-layer '1 *stored-file-list* *structure* 0))

;; * finished!

(format t "~&finished loading!")


