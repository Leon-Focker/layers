#|(defpackage :layers
  (:use :cl))
(in-package :layers)|#
(in-package :sc)
(auto-set-default-dir)

;;;; To do:
;;;; check reset (?) - layer-objects are updated - layers-ojects are not
;;;; loop soundfiles? how?
;;;; give stored-file an amplitude-value
;;;; give layers an overlap-value
;;;; combine sfl
;;;; chosen sfl is dependant on current-length
;;;; variable rest multiplier
;;;; create template
;;;; change probabilities depending on Formteil
;;;; multichannel?
;;;; one layer could influence another (for example by appending markov lists)
;;;; decide next sample on how long current file is being played?

;; * Layers
#|||||||||||||||||||||||||||||||||||#
;; ** global parameters
#|||||||||||||||||||||||||||||||||||#

(defparameter *total-length* 30)
(defparameter *seed* 5)
(defparameter *random-number* nil)
(defparameter *pd-on-windows* t)
(defparameter *default-sample-dir* "/E/ZKF/Layers/samples")
;; what is the maximum length for the smallest value in the structure?
(defparameter *max-smallest-sample-length* 0.05)
(defparameter *loop* nil)
(defparameter *start-stop* t)

#|||||||||||||||||||||||||||||||||||#
;; ** utilities
#|||||||||||||||||||||||||||||||||||#

;; *** start-osc
;;; simple function to open the osc-call
(defun start-osc ()
  (sc::osc-call :send-ip #(192 168 56 1) :listen-port 5000 :send-port 5000))

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
  (labels ((helper (chooser ls index sum)
	     (cond ((null ls) 0)
		   ((< chooser sum) index)
		   (t (helper chooser
			      (cdr ls)
			      (+ index 1)
			      (+ sum (car ls)))))))
    (helper (sc::rescale chooser 0 1 0 (loop for i in ls sum i))
	    ls 0 (car ls))))

;; *** remove-nth
;;; remove nth element from list
(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

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
  ((data :accessor data :initarg :data :type float :initform *seed*)))

;; *** random-number
;;; make an instance of random-number
(defun random-number ()
  (make-instance 'random-number))

;;; set the global variable *random-number* to an instance of random-number
(setf *random-number* (random-number))

;;; get current value of a random-numer object
(defmethod get-number ((rn random-number))
  (data rn))

;; *** get-next
;;; get a random-number object to the next pseudo random number
(defmethod get-next ((rn random-number))
  (setf (data rn)
	(mod (+ (* (data rn)
		   5.266935713607)
		3.6472963)
	     1.0)))

#|||||||||||||||||||||||||||||||||||#
;; ** list-object
#|||||||||||||||||||||||||||||||||||#

;; *** list-object
;;; just a very basic list object with basic methods
(defclass list-object (base-object)
  ((current :accessor current :type integer :initform 0)))

;; *** get-ids
;;; get the ids of all elements in the list
(defmethod get-ids ((lo list-object))
  (loop for element in (data lo) collect (id element)))

;; *** get-current
;;; get current element of list
(defmethod get-current ((lo list-object))
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
(defun scale-structure (structure target-sum)
  (let* ((ls (sc::flatten structure))
	 (length-ls (loop for i in ls sum i)))
    (loop for i in ls collect
	 (rescale i 0 length-ls 0 target-sum))))

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
(defun lindenmayer (total-length seed rules ratios)
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
		      (loop for rep in ratios do
			   (setf res-ls 
				 (loop for element in res-ls collect
				      (if (eq (car rep)
					      element) 
					  (* (cadr rep)
					     fin-ls) 
					  element))))
		      res-ls)) ; by this point it should act. be called final
		   ;; if the list is still nested, loop through al elements:
		   ;; since we're working with a multiple-value bind,
		   ;; this is a bit of a weird loop.
		   ;; first we'll need to helping variables, since loop itself
		   ;; can't return multiple values (if yes, teach me)
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
			*max-smallest-sample-length*)
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
(defun make-structure (seed rules ratios)
  (make-instance 'structure
		 :data (lindenmayer *total-length* seed rules ratios)
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
(defmethod make-length-list ((st structure) &optional (n 0))
  (make-instance 'length-list
		 :data (nth n (data st))
		 :structure st))

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
   ;; this helps to switch sfls dependant on the next play-length
   (length-min :accessor length-min :initarg :length-min :initform 0)
   (length-max :accessor length-max :initarg :length-max :initform 100)
   (sfl-when-shorter :accessor sfl-when-shorter :initarg :sfl-when-shorter
		     :initform nil)
   (sfl-when-longer :accessor sfl-when-longer :initarg :sfl-when-longer
		    :initform nil)
   (decay :accessor decay :initarg :decay :initform 0)))

;; *** make-stored-file
;;; create an instance of stored-file and config with markov-list etc.
(defun make-stored-file (id path-from-default-dir markov-list decay
			 &key
			   (directory *default-sample-dir*))
  (make-instance 'stored-file
		 :id id
		 :name (pathname-name path-from-default-dir)
		 :path (format nil "~a~a" directory path-from-default-dir)
		 :decay decay ; in seconds
		 :markov-list (make-markov-list nil markov-list)))

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
  ())

;; *** make-stored-file-list
;;; make an instance of stored-file-list
(defun make-stored-file-list (id data)
  (make-instance 'stored-file-list :id id :data data))

;; *** make-rest
;;; make a stored-file-list object representing a rest
(defun make-rest () (make-stored-file 'rest "/rest.wav" '() 0))

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
	    (push sf (data sfl))))))

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
					 (decay 0) ; in seconds
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
	   :directory "")
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
		  
#|||||||||||||||||||||||||||||||||||#
;; ** layer
#|||||||||||||||||||||||||||||||||||#

;; *** layer
;;; a layer with distinct sound-file and properties
;;; one layer will send audio to one mixer channel
;;; play slot is responsible to stop layer after structure ended
(defclass layer (base-object)
  ((stored-file-list :accessor stored-file-list
		     :initarg :stored-file-list :initform nil)
   (current-stored-file :accessor current-stored-file
			:initarg :current-stored-file :initform nil)
   (last-stored-file :accessor last-stored-file
		     :initarg :last-stored-file :initform nil)
   (this-length :accessor this-length :initarg :this-length :initform 1)
   (structure :accessor structure :initarg :structure)
   (n-for-length-list :accessor n-for-length-list
		      :initarg :n-for-length-list :type integer)
   (length-list :accessor length-list :initarg :length-list :initform nil)
   (length-min :accessor length-min :initarg :length-min :initform 0)
   (length-max :accessor length-max :initarg :length-max :initform 100)
   (sfl-when-shorter :accessor sfl-when-shorter :initarg :sfl-when-shorter
		     :initform nil)
   (sfl-when-longer :accessor sfl-when-longer :initarg :sfl-when-longer
		    :initform nil)
   (play :accessor play :initarg :play :initform t)))

;; *** initialize-instance
;;; called automatically, sets last-stored-file to current-s-f when initializing
(defmethod initialize-instance :after ((ly layer) &rest initargs)
  (declare (ignore initargs))
  (setf (current-stored-file ly) (first (data (stored-file-list ly)))
	(last-stored-file ly) (current-stored-file ly)
	(length-list ly)
	(make-length-list (structure ly) (n-for-length-list ly))
	(sfl-when-shorter ly)
	(stored-file-list ly)
	(sfl-when-longer ly)
	(stored-file-list ly)))

;; *** make-layer
;;; create a layer-object
(defun make-layer (id stored-file-list structure &optional (n 0))
  (make-instance 'layer
		 :id id
		 :stored-file-list stored-file-list
		 :structure structure
		 :n-for-length-list n))

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
             ~&play:              ~a"
	  (id ly)
	  (get-id-current-file ly)
	  (get-id-last-file ly)
	  (this-length ly)
	  (play ly)))

;; *** get-next
;;; moves on to the next stored sound file, determined by markov-odds and random
(defmethod get-next ((ly layer))
  ;; when the current event is actually a rest, copy the last markov list
  ;; to the rest stored-file-object
  (when (eq 'rest (get-id (current-stored-file ly)))
    (setf-markov (current-stored-file ly)
		 ;; if the first file is a rest, use markov-list of the second
		 (if (data (markov-list (last-stored-file ly)))
		     (markov-list (last-stored-file ly))
		     (markov-list (second (data (stored-file-list ly)))))))
  ;; error, if current file still has no markov-list
  (unless (data (markov-list (current-stored-file ly)))
    (error "~&No markov-list found in current-stored-file ~a in Layer~a"
	   (get-id (current-stored-file ly)) (get-id ly)))
  ;; make the playback stop when the structure has ended (and *loop* is nil)
  (let ((lo (length-list ly)))
    (when (and (= (current lo) (1- (length (data lo)))) (not *loop*))
      (setf (play ly) nil)))
  ;; here we will actually determinde the next file that will be played:
  (setf (last-stored-file ly)
	(current-stored-file ly)
	;; here we will change the sample-bank (sfl) of the current layer
	;; based off of different factors:
	;; 
	(stored-file-list ly)
	(let* ((len (see-next (length-list ly))))
	  (cond ((< len (length-min ly)) (sfl-when-shorter ly))
		((> len (length-max ly)) (sfl-when-longer ly))
		(t (stored-file-list ly))))
	(current-stored-file ly)
	(let ((data (data (stored-file-list ly))))
	  (loop for snd in data until break
	     with break
	     ;; determine id of next soundfile
	     ;; when current file has no length-dependant-list
	     ;; a random number and the markov-list will decide.
	     ;; else we use the length-dependant-list
	     with new-id =
	       (if (length-dependant-list (current-stored-file ly))
		   (decide-for-snd-file
		    (length-dependant-list ly)
		    (this-length ly))
		   (decide-for-snd-file
		    (markov-list
		     (current-stored-file ly))
		    (get-next *random-number*)))
	     do
	       (when (eq (id snd)
			 new-id)
		 (setf break t)
		 (return snd))
	       (when (and (eq snd (car (last data)))
			   (not break))
		 (error "there is no file with ID: ~a in current-stored-file ~
                         list: ~a" new-id (get-id (stored-file-list ly))))))
	(this-length ly)
	(get-next (length-list ly))))

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
	(get-current (length-list ly))))

;; *** reset-index
;;; sets current slot of the length-list of a layer back to 0 (start of loop)
(defmethod reset-index ((ly layer))
  (setf (current (length-list ly)) 0)
  (format t "~& current timing index of layer ~a set to ~a"
	  (get-id ly) (current (length-list ly))))

;; *** play-this
;;; sends list with all necessary information to pd, tsouo play
;;; the current stored-file
(defmethod play-this ((ly layer))
  (if (and (play ly) *start-stop*)
      (prog1
	  (list
	   ;; layer id (which voice in PD to send to)
	   (get-id ly)
	   ;; soundfile
	   (if *pd-on-windows*
	       (string-replace "/E/"
			       "E:/"
			       (path (current-stored-file ly)))
	       (path (current-stored-file ly)))
	   ;; soundfile-length
	   (this-length ly)
	   ;; decay
	   (* 1000 ;; from seconds to miliseconds
	      (let ((max-decay (see-next (length-list ly)))
		    (decay (decay (current-stored-file ly))))
		(if (>= decay max-decay)
		    max-decay
		    decay)))
	   ;; soundfile-id (displayed in PD)
	   (get-id-current-file ly))
	
	(when t (print-layer ly))
	(get-next ly))
      (progn
	(setf (play ly) t)
	(format t "~&Playback for layer ~a ends now, last sound was ~a seconds"
		(get-id ly)
		(this-length ly)))))

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
  (let* ((ly))
    (loop for layer in (data lys) do
	 (when (eq layer-id (get-id layer))
	   (setf ly layer)))
    (if ly
	(play-this ly)
	(error "~&there is no Layer with ID ~a in layers ~a"
	       layer-id (get-id lys)))))

;; *** reset-layers
;;; resets everything to the start of the piece and re-read structure
(defun reset-layers (&optional layers-object)
  (declare (special *layers*))
  (setf (data *random-number*) *seed*)	; reset *random-number*
  (unless layers-object (setf layers-object *layers*))
  (let ((sts '()))
    (loop for layer in (data layers-object) do
	 (unless (member (structure layer) sts)
	   (push (structure layer) sts)))
    (loop for st in sts do
	 (re-gen-structure st))
    (loop for layer in (data layers-object) do
	 (update-layer layer)
	 (reset-index layer)
	 (setf (play layer) t)))
  (format t "~&Layers have been reset"))

;;; examples:

(defparameter *layer1* (make-layer '1 *stored-file-list* *structure* 0))

;;; example:
;;; store 5 sounds in a layer, update current-stored-file

#|
(store-file-in-list (make-rest) *stored-file-list*)

(store-file-in-list
 (make-stored-file
  'noisy1
  "/rhythmic/noisy/1.wav"
  '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1)))
 *stored-file-list*)

(store-file-in-list
 (make-stored-file
  'noisy2
  "/rhythmic/noisy/2.wav"
  '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1)))
 *stored-file-list*)

(store-file-in-list
 (make-stored-file
  'noisy3
  "/rhythmic/noisy/3.wav"
  '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1)))
 *stored-file-list*)

(store-file-in-list
 (make-stored-file
  'noisy4
  "/rhythmic/noisy/4.wav"
  '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1)))
 *stored-file-list*)

(store-file-in-list
 (make-stored-file
  'noisy5
  "/rhythmic/noisy/5.wav"
  '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1)))
 *stored-file-list*)

(update-layer *layer1*)

(load-from-same-dir "stored-files.lsp")
(defparameter *layer2* (make-layer '2 *background* *structure* 2))
(defparameter *layer3* (make-layer '3 *background* *structure* 3))
(update-layer *layer3*)
(update-layer *layer4*)

(defparameter *chords* (make-stored-file-list 'chords '()))
(folder-to-stored-file-list *chords* "/E/ZKF/Layers/samples/chords/organ")
(store-file-in-list (make-rest) *chords*)
(defparameter *layers*
  (make-layers 'layers
	       (list
		*layer1*
		*layer2*
		*layer3*
		(make-layer '4 *chords* *structure*))))


(list '2 "/E/ZKF/Layers/samples/background/atmo/piano.wav"2 500 'test)
|#
