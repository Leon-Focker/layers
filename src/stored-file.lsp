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
   ;; duration in seconds
   (duration :accessor duration :initarg :duration :initform 0)
   ;; duration in samples
   (total-samples :accessor total-samples :initarg :total-samples :initform 0)
   ;; samplerate of the soundfile
   (samplerate :accessor samplerate :initarg :samplerate :initform 48000)
   (start :accessor start :initarg :start :initform 0)
   ;; multiplier when playing soundfile back
   (amplitude :accessor amplitude :initarg :amplitude :initform 1)
   ;; loudest sample in the soundfile
   (peak :accessor peak :initarg :peak :initform nil)
   (peak-index :accessor peak-index :initarg :peak-index :initform 0)
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
   ;; dominant frequency
   (dominant-frequency :accessor dominant-frequency :initarg :dominant-frequency)
   ;; envelope smoothness
   (smoothness :accessor smoothness :initarg :smoothness)
   ;; biggest jump up in the envelope
   (transient :accessor transient :initarg :transient)
   ;; position in a 3d coordinate space - can also be used to get next file
   (x :accessor x :initarg :x :initform 0.5 :reader x)
   (y :accessor y :initarg :y :initform 0.5 :reader y)
   (z :accessor z :initarg :z :initform 0.5 :reader z)))

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

;; *** spectral-centroid
;;; dominant frequency, centroid, flattnes and spread of soundfile.
;;; (function needs new name?)
;;; file can either be an array or the path to a file
(defun spectral-centroid (file &key fft-size (srate 48000))
  (let* ((ar (not (stringp file)))
	 (srate (if ar srate (clm::sound-srate file)))
	 (sample-len (if ar (length file) (soundfile-duration file)))
	 ;; if not supplied, fft-size is set to the hightes possible power of two
	 ;; (up until ca. a second)
	 (fft-size (or fft-size
		      (loop with num = 1 for i from 1 until (or (> num srate)
							     (> num sample-len))
			 finally (return (expt 2 (- i 2)))
			 do (setf num (* num 2)))))
	 ;; by repeating fft-size/4 times we take into account freqs up to 12kHz
	 (repeats (/ fft-size 4))
	 (base-freq (/ srate fft-size))
	 (arithmetic-mean 0)
	 (geometric-mean 0)
	 (a 0)
	 (b 0)
	 (c 0)
	 (d 0)
	 (e '(0 0))
	 (m '()))
    (setf m (clm::fft-from-file file
				:fft-size fft-size
				:window clm::hanning-window))
    (loop for i from 0 and j in m repeat repeats do
	 (when (> j (car e)) (setf e (list j (* i base-freq 1.0))))
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
	    (cadr e)             ;; dominant frequency
	    )))

;; *** analyse-soundfile
;;; analyse some basic parameters and write them into the sf-object
(defmethod analyse-soundfile ((sf stored-file) &key fft-size (srate 48000))
  ;;(clm::play (path sf))
  (let* ((array (clm::table-from-file (path sf)))
	 (envelope (clm::envelope-follower array))
	 (max (max-of-array-with-index array t)))
    (multiple-value-bind (c s f d)
	(spectral-centroid array :fft-size fft-size :srate srate)
      (setf (centroid sf) c
	    (spread sf) s
	    (flatness sf) f
	    (dominant-frequency sf) d))
    (setf (peak sf) (first max))
    (setf (peak-index sf) (second max))
    (setf (smoothness sf) (list-flatness envelope))
    (setf (transient sf) (biggest-jump (reduce-by envelope 50))))
  sf)

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

;;;; EOF stored-file.lsp
