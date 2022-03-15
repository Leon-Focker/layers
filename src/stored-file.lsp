;; ** stored-file
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
   ;; multiplier when playing soundfile back
   (amplitude :accessor amplitude :initarg :amplitude :initform 1)
   ;; loudness of the file before multiplying by 
   (loudness :accessor loudness :initarg :loudness :initform 1)
   (loop-flag :accessor loop-flag :initarg :loop-flag :initform nil)
   (decay :accessor decay :initarg :decay :initform 0)
   (panorama :accessor panorama :initarg :panorama :initform 45)
   ;; analysed, not user set data:
   ;; spectral centroid
   (centroid :accessor centroid :initarg :centroid)
   ;; spectral spread
   (spread :accessor spread :initarg :spread)
   ;; spectral flatness
   (flatness :accessor flatness :initarg :flatness)
   ;; envelope smoothness
   (smoothness :accessor smoothness :initarg :smoothness)
   ;; biggest jump up in the envelope
   (transient :accessor transient :initarg :transient)
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

;; *** create-rest
;;; make a stored-file-list object representing a rest
(defun create-rest () (make-stored-file 'rest "/rest.wav" :markov '() :decay 0))

;;; example
(make-stored-file
 'noisy1
 "/rhythmic/noisy/1.wav"
 :markov '((noisy1 1)
	   (noisy2 1)
	   (noisy3 1)
	   (noisy4 1)
	   (noisy5 1))
 :decay 10)

;; *** spectral-centroid
;;; centroid of soundfile. file can either be an array or the path to a file
(defun spectral-centroid (file &key fftsize (srate 48000))
  (let* ((ar (not (stringp file)))
	 (srate (if ar srate (clm::sound-srate file)))
	 (sample-len (if ar (length file) (soundfile-duration file)))
	 ;; if not supplied, fftsize is set to the hightes possible power of two
	 ;; (up until ca. a second)
	 (fftsize (if fftsize
		      fftsize
		      (loop with num = 1 for i from 1 until (or (> num srate)
							     (> num sample-len))
			 finally (return (expt 2 (- i 2)))
			 do (setf num (* num 2)))))
	 ;; by repeating fftsize/4 times we take into account freqs up to 12kHz
	 (repeats (/ fftsize 4))
	 (base-freq (/ srate fftsize))
	 (arithmetic-mean 0)
	 (geometric-mean 0)
	 (a 0)
	 (b 0)
	 (c 0)
	 (d 0)
	 (m '()))
    (setf m (clm::fft-from-file file
				:fftsize fftsize
				:window clm::hanning-window))
    (loop for i from 0 and j in m repeat repeats do
	 (incf a (* i base-freq (expt j 2)))
	 (incf b (expt j 2))
	 (incf arithmetic-mean j)
	 (incf geometric-mean (log j)))
    (setf arithmetic-mean (/ arithmetic-mean repeats))
    (setf geometric-mean (exp (/ geometric-mean repeats)))
    (setf c (/ a b))
    (loop for i from 0 and j in m repeat repeats do
	 (incf d (* (expt (- (* i base-freq) c) 2)
		    (expt j 2))))
    (values c                    ;; centroid
	    (sqrt (/ d b))       ;; spread
	    (/ geometric-mean    ;; flatness
	       arithmetic-mean)
	    )))

;; *** analyse-soundfile
;;; analyse some basic parameters and write them into the sf-object
(defmethod analyse-soundfile ((sf stored-file))
  ;;(clm::play (path sf))
  (let* ((array (clm::table-from-file (path sf)))
	 (envelope (clm::envelope-follower array)))
    (multiple-value-bind (c s f)
	(spectral-centroid array)
      (setf (centroid sf) c
	    (spread sf) s
	    (flatness sf) f))
    (setf (smoothness sf) (list-flatness envelope))
    (setf (transient sf) (biggest-jump envelope)))
  sf)

;; *** map-soundfile
;;; map x y z of a soundfile according to analysis
(defmethod map-soundfile ((sf stored-file)
			  &key
			    (f1 #'(lambda (sf) (/ (log (centroid sf)) 12000)))
			    (f2 #'(lambda (sf) (+ (* (/ (log (spread sf)) 12000)
						     0.5)
						  (* (flatness sf) 0.5))))
			    (f3 #'(lambda (sf) (+ (* (- 1
							(expt (smoothness sf)
							      0.5))
						     0.4)
						  (* (expt (transient sf) 0.7)
						     0.6)))))
  (analyse-soundfile sf)
  (setf (x sf) (funcall f1 sf))
  (setf (y sf) (funcall f2 sf))
  (setf (z sf) (funcall f3 sf))
  sf)

;;;; EOF stored-file.lsp
