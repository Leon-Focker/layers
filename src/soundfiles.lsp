;; ** soundfiles
;;;; storing and categorising soundfiles etc.

(in-package :layers)

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
   (panorama :accessor panorama :initarg :panorama :initform 45)
   ;; position in a 3d coordinate space - can also be used to get next file
   (x :accessor x :initarg :x :initform 0.5)
   (y :accessor y :initarg :y :initform 0.5)
   (z :accessor z :initarg :z :initform 0.5)))

;; *** make-stored-file
;;; create an instance of stored-file and config with markov-list etc.
(defun make-stored-file (id path-from-default-dir
			 &key
			   markov
			   (decay 100)
			   (directory *default-sample-dir*)
			   (start 0)
			   (amplitude 1)
			   (panorama 45)
			   (loop-flag nil)
			   (preferred-length nil)
			   (x 0.5)
			   (y 0.5)
			   (z 0.5))
  (let* ((path (format nil "~a~a" directory path-from-default-dir)))
    (unless (probe-file path)
      (warn "~&the file with path ~a does not exist" path))
    (make-instance 'stored-file
		 :id id
		 :name (pathname-name path-from-default-dir)
		 :path path
		 :decay decay ; in seconds
		 :markov-list (make-markov-list nil (if markov
							markov
							`((,id 1))))
		 :duration (soundfile-duration path)
		 :start start
		 :amplitude amplitude
		 :panorama panorama
		 :loop-flag loop-flag
		 :preferred-length preferred-length
		 :x x
		 :y y
		 :z z)))

;; *** setf-markov
;;; sets the markov list of a stored-file
(defmethod setf-markov ((sf stored-file) new-markov-list)
  (setf (markov-list sf) new-markov-list))

;;; example
(make-stored-file
 'noisy1
 "/rhythmic/noisy/1.wav"
 :markov '((noisy1 1)
	   (noisy2 1)
	   (noisy3 1)
	   (noisy4 1)
	   (noisy5 1))
 :decay 0)

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

;; *** subordinate-stored-file-list
;;; only contains a list of stored-files and its probability weight
;;; designed for one time use, eg. (result of get-sub-list-of-closest)
(defclass subordinate-stored-file-list (list-object)
  ())

;; *** make-subordinate-stored-file-list
(defun make-subordinate-stored-file-list (id data)
  (make-instance 'subordinate-stored-file-list :id id :data data))

;; *** decide-for-snd-file
;;; returns the id of the chosen soundfile, based on the subordinate-sfl it got
(defmethod decide-for-snd-file ((sub-sfl subordinate-stored-file-list)
				random-number)
  (let* ((weights '())
	 (ids '()))
    (loop for i in (data sub-sfl) do
	 (push (car i) weights)
	 (push (get-id (cadr i)) ids))
    (nth (decider random-number
		  weights)
	 ids)))

;; *** get-ids
;;; get ids of all stored-files in stored-file-list
(defmethod get-ids ((sfl stored-file-list))
  (loop for sf in (data sfl) collect
       (get-id sf)))

;; *** get-coordinates
;;; get coordinates of all stored-files in stored-file-list as '((x1 y1 z1 )...)
(defmethod get-coordinates ((sfl stored-file-list))
  (loop for sf in (data sfl) collect
       (list (x sf) (y sf) (z sf))))

;; *** get-paths
;;; show all paths to all sounds on the sfl
(defmethod get-paths ((sfl stored-file-list))
  (loop for p in (data sfl) collect (path p)))

;; *** get-sub-list-of-closest
;;; get a list of soundfiles which are close to current position in x,y,z
(defmethod get-sub-list-of-closest ((sfl stored-file-list)
				    current-position
				    &key (max-distance 0.6)) ;; maybe change?
  (let* ((min 10)
	 (closest)
	 (ls '())) ;; no distance should ever be greater when x,y,z are < 1
    (loop for sf in (data sfl) do
	 ;; get distance between point of sf and current position
	 (let ((dis (distance-between-points (vector (x sf) (y sf) (z sf))
					     current-position)))
	   (when (< dis min) (setf min dis) (setf closest sf))
	   (when (< dis max-distance) (push (list (/ 1 (+ 0.01 dis)) sf) ls))))
    ;; when no point is close enough, at least return the closest one
    (when (null ls) (push (list (/ 1 min) closest) ls))
    (print (id (cadar (data (make-subordinate-stored-file-list 'closest-files ls)))))
    (make-subordinate-stored-file-list 'closest-files ls)))

;; *** create-rest
;;; make a stored-file-list object representing a rest
(defun create-rest () (make-stored-file 'rest "/rest.wav" :markov '() :decay 0))

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
	   :markov (loop for i in ids
		      for markov = (assoc i markov-list)
		      collect
			(list i (if markov
				    (cadr markov)
				    1)))
	   :decay decay
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

;;;; EOF soundfiles.lsp
