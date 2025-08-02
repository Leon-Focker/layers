;; ** stored-file

(in-package :layers)

;; *** stored-file
(defclass stored-file (soundfile)
  ;; links this file to other soundfiles with certain probabilities
  ((markov-list :accessor markov-list :initarg :markov-list
		:initform nil)
   ;; this helps decide the next file dependant on the current play-length
   (length-dependant-list :accessor length-dependant-list
			  :initarg :length-dependant-list
			  :initform nil)
   (preferred-length :accessor preferred-length :initarg :preferred-length
		     :initform nil)
   (start :accessor start :initarg :start :initform 0)
   ;; multiplier when playing soundfile back
   (amplitude :accessor amplitude :initarg :amplitude :initform 1)
   (loop-flag :accessor loop-flag :initarg :loop-flag :initform nil)
   (decay :accessor decay :initarg :decay :initform 0)
   (panorama :accessor panorama :initarg :panorama :initform 45)
   ;; position in a 3d coordinate space - can also be used to get next file
   (x :accessor x :initarg :x :initform 0.5 :reader x)
   (y :accessor y :initarg :y :initform 0.5 :reader y)
   (z :accessor z :initarg :z :initform 0.5 :reader z)))

;; *** setf-markov
;;; sets the markov list of a stored-file
(defmethod setf-markov ((sf stored-file) new-markov-list)
  (setf (markov-list sf) new-markov-list))

;; *** create-rest
;;; make a stored-file-list object representing a rest
(defun create-rest () (make-stored-file 'rest "/rest.wav" :markov '() :decay 0))

;;; example
#+nil(make-stored-file
      'noisy1
      "/rhythmic/noisy/1.wav"
      :markov '((noisy1 1)
		(noisy2 1)
		(noisy3 1)
		(noisy4 1)
		(noisy5 1))
      :decay 10)

;; *** map-soundfile
;;; map x y z of a soundfile according to analysis
(defmethod map-soundfile ((sf stored-file) &key f1 f2 f3 fft-size)
  (setf f1 (or f1 #'(lambda (sf) (/ (log (centroid sf)) 12000))))
  (setf f2 (or f2 #'(lambda (sf) (+ (* (/ (log (spread sf)) 12000)
				  0.5)
			       (* (flatness sf) 0.5)))))
  (setf f3 (or f3 #'(lambda (sf) (+ (* (- 1
				     (expt (smoothness sf)
					   0.5))
				  0.4)
			       (* (expt (transient sf) 0.7)
				  0.6)))))
  (analyse-soundfile sf :fft-size fft-size)
  (setf (x sf) (funcall f1 sf))
  (setf (y sf) (funcall f2 sf))
  (setf (z sf) (funcall f3 sf))
  sf)

;; *** make-stored-file
;;; create an instance of stored-file and config with markov-list etc.
(defun make-stored-file (id relative-path
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
			   (z 0.5)
			   analyse)
  (let* ((path (format nil "~a~a" directory relative-path)))
    (unless (probe-file path)
      (warn "~&the file with path ~a does not exist" path))
    (let ((sf (make-instance 'stored-file
			     :id id
			     :name (pathname-name relative-path)
			     :path path
			     :decay decay ; in seconds
			     :markov-list (make-markov-list nil (or markov
								    `((,id 1))))
			     :duration (soundfile-duration path)
			     :total-samples (soundfile-framples path)
			     :samplerate (soundfile-samplerate path)
			     :start start
			     :amplitude amplitude
			     :panorama panorama
			     :loop-flag loop-flag
			     :preferred-length preferred-length
			     :x x
			     :y y
			     :z z)))
      (if analyse (analyse-soundfile sf) sf))))

;; *** check-sanity
(defmethod check-sanity :after ((sf stored-file) &optional (error-fun #'warn))
  ;; These should be nil, a number or 'random
  (loop for slot in '(start amplitude decay)
	do
	   (unless (or (not (funcall slot sf)) (numberp (funcall slot sf))
		       (equal (funcall slot sf) 'random))
	     (funcall error-fun "check-sanity: weird ~a for stored-file ~a: ~a"
		      slot (id sf) (funcall slot sf))))
  ;; These should be numbers between 0 and 1
  (loop for slot in '(x y z) do
    (unless (and (numberp (funcall slot sf)) (<= 0 (funcall slot sf) 1))
      (funcall error-fun "check-sanity: weird ~a for stored-file ~a: ~a"
	       slot (id sf) (funcall slot sf))))
  ;; check markov-list
  (unless (or (not (markov-list sf))
	      (equal (type-of (markov-list sf)) 'markov-list))
    (funcall error-fun "check-sanity: weird ~a for stored-file ~a: ~a"
	     'markov-list (id sf) (markov-list sf)))
  ;; check ldl
  (unless (or (not (LENGTH-DEPENDANT-LIST sf))
	      (equal (type-of (LENGTH-DEPENDANT-LIST sf))
		     'LENGTH-DEPENDANT-LIST))
    (funcall error-fun "check-sanity: weird ~a for stored-file ~a: ~a"
	     'LENGTH-DEPENDANT-LIST (id sf) (LENGTH-DEPENDANT-LIST sf)))
  ;; Should be numbers between 0 and 90
  (unless (and (numberp (panorama sf)) (<= 0 (panorama sf) 90))
    (funcall error-fun "check-sanity: weird ~a for stored-file ~a: ~a"
	     'panorama (id sf) (panorama sf)))
  t)

;; *** make-load-form
(defmethod make-load-form ((sf stored-file) &optional environment)
  (declare (ignore environment))
  `(make-instance 'stored-file
		  :id ',(id sf)
		  :data ',(data sf)
		  :name ',(name sf)
		  :path ',(path sf)
		  :markov-list ,(make-load-file (markov-list sf))
		  :length-dependant-list ',(length-dependant-list sf)
		  :preferred-length ',(preferred-length sf)
		  :duration ',(duration sf)
		  :total-samples ',(total-samples sf)
		  :samplerate ',(samplerate sf)
		  :start ',(start sf)
		  :amplitude ',(amplitude sf)
		  :peak ',(peak sf)
		  :peak-index ',(peak-index sf)
		  :loop-flag ',(loop-flag sf)
		  :decay ',(decay sf)
		  :panorama ',(panorama sf)
		  :centroid ',(centroid sf)
		  :spread ',(spread sf)
		  :flatness ',(flatness sf)
		  :dominant-frequency ',(dominant-frequency sf)
		  :smoothness ',(smoothness sf)
		  :transient ',(transient sf)
		  :x ',(x sf)
		  :y ',(y sf)
		  :z ',(z sf)))

;; *** store-in-text-file
;;; store a sfl in a text file, so the analysis can be skipped by reading in
;;; the soundfiles.
(defmethod store-in-text-file ((sf stored-file) &optional file)
  (let* ((file (or file (format nil "~a~a-load-file.txt" *src-dir* (id sf)))))
    (write-to-file file (make-load-form sf))
    (format t "~&wrote ~a into ~a" (id sf) file)))

;;;; EOF stored-file.lsp
