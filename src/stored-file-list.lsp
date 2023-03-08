;; ** stored-file-list
;;;; sfl class, storing and categorising soundfiles etc.

(in-package :layers)

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
	 (ids '())
	 (ls (sort (data sub-sfl) #'(lambda (x y) (> (car x) (car y))))))
    (loop for i in ls do
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

;; *** get-all
;;; gets list of all values in a slot of the stored-file ins stored-file-list
(defmethod get-all (slot (sfl stored-file-list) &optional error)
  (loop for sf in (data sfl)
     do (when (and (not (funcall slot sf)) error)
	  (error "get-all found no value for ~a in sf ~a" slot (id sf)))
     collect (funcall slot sf)))

;; *** get-coordinates
;;; get coordinates of all stored-files in stored-file-list as '((x1 y1 z1 )...)
(defmethod get-coordinates ((sfl stored-file-list))
  (loop for sf in (data sfl)
     do (unless (and (x sf) (y sf) (z sf))
	  (error "In get-coordinates, not all coordinates were given for sf: ~a"
		 (id sf)))
     collect (list (x sf) (y sf) (z sf))))

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
    (unless (data sfl) (error 'no-value))
    (loop for sf in (data sfl) do
	 ;; get distance between point of sf and current position
	 (let ((dis (distance-between-points (vector (x sf) (y sf) (z sf))
					     current-position)))
	   (when (< dis min) (setf min dis) (setf closest sf))
	   (when (< dis max-distance) (push (list (/ 1 (+ 0.01 dis)) sf) ls))))
    ;; when no point is close enough, at least return the closest one
    (unless closest (error 'weird-values))
    (when (null ls) (push (list (/ 1 min) closest) ls))
    ;;(print (id (cadar (data (make-subordinate-stored-file-list 'closest-files ls)))))
    (make-subordinate-stored-file-list 'closest-files ls)))

;; *** auto-scale-mapping
;;; automatically scale all x, y and z values
;;; to optimally fill out coordinate space
(defmethod auto-scale-mapping ((sfl stored-file-list) &key remap)
  (let* ((len (length (data sfl)))
	 (all-x (sort (get-all 'x sfl t) #'<))
	 (all-y (sort (get-all 'y sfl t) #'<))
	 (all-z (sort (get-all 'z sfl t) #'<))
	 (x-min (first all-x))
	 (y-min (first all-y))
	 (z-min (first all-z))
	 (x-max (car (last all-x)))
	 (y-max (car (last all-y)))
	 (z-max (car (last all-z))))
    (if remap
      (loop for sf in (data sfl) do
	   (setf (x sf) (/ (index-of-element (x sf) all-x) len)
		 (y sf) (/ (index-of-element (y sf) all-y) len)
		 (z sf) (/ (index-of-element (z sf) all-z) len)))
      (loop for sf in (data sfl) do
	 (setf (x sf) (/ (- (x sf) x-min) (- x-max x-min))
	       (y sf) (/ (- (y sf) y-min) (- y-max y-min))
	       (z sf) (/ (- (z sf) z-min) (- z-max z-min)))))))

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
;;; auto-map - analyzes and maps the file automatically in a x y z space
;;; f1, f2, f3 are the mapping functions for x y z, leave them nil for default
;;; fft-size must be a power of 2 and starts at the beginning of the sample
;;;   when fft-size is nil, the biggest one possible is chosen
;;; auto-scale-mapping - scales all mapping values to range from 0 to 1
;;; remap - allows for rearranging of the files within the x y z space
(defmethod folder-to-stored-file-list ((sfl stored-file-list)
				       folder
				       &key
					 id-uniquifier
					 markov-list
					 analyse
					 auto-map
					 f1
					 f2
					 f3
					 fft-size
					 auto-scale-mapping
					 remap
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
	  (prog1
	      (let ((sf (make-stored-file
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
			 :loop-flag loop-flag)))
		(cond ((and analyse (not auto-map))
		       (analyse-soundfile sf))
		      (auto-map
		       (map-soundfile sf :f1 f1 :f2 f2 :f3 f3
				      :fft-size fft-size))
		      (t sf)))
	    (format t "~&storing file: ~a" id))
	  sfl))
    (when auto-scale-mapping
      (auto-scale-mapping sfl :remap remap))))

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

;; *** check-mapping
;;; checks wheter any x, y or z value is not between 0 and 1,
;;; maybe does more in the future
(defmethod check-mapping ((sfl stored-file-list))
  (loop for x in (get-all 'x sfl)
     and y in (get-all 'y sfl)
     and z in (get-all 'z sfl) do
       (when (or (> (max x y z) 1)
		 (< (min x y z) 0))
	 (warn "~&found x, y or z value that is not in bounds 0 - 1 in sfl ~a"
	       (get-id sfl)))))

;; *** make-load-form
(defmethod make-load-form ((sfl stored-file-list) &optional environment)
  (declare (ignore environment))
  `(make-instance 'stored-file-list
		  :id ',(id sfl)
		  :data ,(cons 'list
				 (loop for sf in (data sfl) collect
				      (make-load-form sf)))
		  :length-min ',(length-min sfl)
		  :length-max ',(length-max sfl)
		  ;;:sfl-when-shorter ',(sfl-when-shorter sfl)
		  ;;:sfl-when-longer ',(sfl-when-longer sfl)
		  ;;:last-played ',(last-played sfl)
		  ))

;; *** store-in-text-file
;;; store a sfl in a text file, so the analysis can be skipped by reading in
;;; the soundfiles.
(defmethod store-in-text-file ((sfl stored-file-list) &optional file)
  (let* ((file (or file (format nil "~a~a-load-file.txt" *src-dir* (id sfl)))))
    (sc::write-to-file file (make-load-form sfl))
    (format t "~&wrote ~a into ~a" (id sfl) file)))

;;;; EOF stored-file-list.lsp
