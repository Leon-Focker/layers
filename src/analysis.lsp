;; ** analysis
;;;; some functions to analyse soundfiles with clm

(in-package :clm)

(defparameter *separate* (format nil ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~
                                      ;;;;;;;;;;;;;;;;;;;;;;;;"))
(defparameter *layers-buffer* '())

;; *** table-from-file
;;; get an array from a soundfile
(definstrument table-from-file (file &optional start length)
  (let ((f (open-input* file)))
    (unwind-protect 
	 (let* ((st (or start 0))
		(len (mus-length file))
		(size (or length (1- len)))
		(ar (make-double-array size :initial-element 0.0))
		(readin0 (make-readin f :start st)))
	   (unless (< (+ st size) len)
	     (error "~&start ~a and size ~ too big for file ~
                     with ~a samples" start size len))
	   (run*
	    (ar)
	    (loop for i below size do
		 (setf (aref ar i) (readin readin0)))))
      (close-input f))))

;; *** fft-from-file
;;; file can be a pathname or an array
(defun fft-from-file (file &key
			     (fftsize 2048)
			     (start-sample 0)
			     (window 0)
			     visualize)
  (let* ((fdr (let* ((frame (make-double-array
			     fftsize
			     :initial-contents
			     (cond ((stringp file)				 
				    (table-from-file file start-sample fftsize))
				   ((arrayp file)
				    (subseq file
					    start-sample
					    (+ start-sample fftsize)))
				   (t (error "fft-from-file: file is of a weird~
                                                  type: ~a" (type-of file)))))))
		(multiply-arrays frame (make-fft-window window fftsize))))
	 (fdi (make-double-array fftsize))
	 (magnitudes '())
	 (phases '()))
    ;; visualize amplitude of file:
    (if visualize (progn (ly::visualize fdr)
			 (terpri)
			 *separate*))
    ;; do fft
    (fft fdr fdi fftsize 1)
    (loop for i below fftsize do
	 (let ((real (aref fdr i))
	       (im (aref fdi i)))
	   (push (sqrt (+ (* real real) (* im im))) magnitudes)
	   (push (atan (/ im (+ real 0.0000000001d0 ))) phases)))
    (setf magnitudes (reverse magnitudes)
	  phases (reverse phases))
    ;; visualize magnitues and phases
    (if visualize (progn (ly::visualize magnitudes)
			 (terpri)
			 *separate*))
    (if visualize (ly::visualize phases))
    ;; output
    (values magnitudes
	    phases
	    fdr
	    fdi)))

;; *** envelope-follower-file
;;; returns the envelope of a soundfile
(definstrument envelope-follower-file (file &optional (len 30) (as-list t))
  (let ((f (open-input* file)))
    (unwind-protect 
	 (let* ((st 0)
		(nd (file-frample-count file))
		(readin0 (make-readin f :start st))
		(fil (make-hilbert-transform len)))
	   (setf *layers-buffer*
		 (make-double-array (- nd st) :initial-element 0.0))
	   (run*
	    (*layers-buffer*)
	    (loop for i from st to nd do
		 (let ((x (readin readin0)))
		   (setf (aref *layers-buffer* i)
			 (sqrt (+ (expt x 2)
				  (expt (fir-filter fil x) 2))))))))
      (close-input f))
    (if as-list
	(loop for i across *layers-buffer* collect i)
	*layers-buffer*)))

;; *** envelope-follower-array
;;; returns the envelope of an array as a list
(defun envelope-follower-array (array &optional (len 30))
  (let* ((fil (make-hilbert-transform len)))
    (loop for x across array collect
	 (sqrt (+ (expt (fir-filter fil x) 2)
		  (expt x 2))))))

;; *** envelope-follower
;;; returns the envelope of an array or a soundfile as a list
(defun envelope-follower (file-or-array &optional (len 30))
  (typecase file-or-array
    (string (envelope-follower-file (probe-file file-or-array)))
    (array (envelope-follower-array file-or-array len))
    (t (error "envelope-follower needs either and array or a pathname but got:~& ~a: ~a"
	      (type-of file-or-array) file-or-array))))
