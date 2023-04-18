;; * Layers
;;;; Layers is designed to load, analyse and store soundfiles and play them back
;;;; in a live setup in pure data. This provides the infrastructure and the
;;;; interface to pure data. Soundfiles can be chosen trough markov chains or
;;;; their attributes, depending on some other limitations. Rhythms are for now
;;;; provided by "structure" objects.

;;;; This software is in a weird state, in that it depends immensely on
;;;; Michael Edwards' slippery chicken, especially its utilities, but builds its
;;;; entire separate object infastructure. This would not have been neccessary
;;;; but it is too late now I think.

;;;; To do:
;;;; check reset (?) - layer-objects are updated - layers-ojects are not
;;;; variable rest probability multiplier?
;;;; create template
;;;; multichannel?
;;;; build "sets" for songs, smoothly switch between sets, live transistions
;;;; one-time-use of samples? (eg. sample can only be played once every 3 mins)
;;;; dynamic panning
;;;; PD needs to be restarted after every reload. else some timings seem to be
;;;;   getting stuck. pls fix :c
;;;; (changing position in coordinate space in Pd, needs function that slowly adjusts
;;;;   position (moving average?) as to not jump to a new position)
;;;;  -> smoothing factor adjustable in pd
;;;; while analysing soundfile, find better way to determine transients?
;;;; when a layer is triggered (even though the remaining-time is > 0.01) by next-trigger
;;;;  and trigger-all is t, it should not start at the begining of the sample but rather skipp
;;;;  the already played part. -> tried to implement but is bugged, see #'next-trigger
;;;; better distinction between restart and reload
;;;; set-n is implemented in a kinda dirty way (shouldn't have to call next-trigger) and
;;;;  setting n introduces a small general delay, it seems like.

;;;; reset-layers should actually reset structure etc without having to reload
;;;; what does it currently reset-to?

;;;; make-load-file for sfl: :sfl-when-longer and :sfl-when-shorter and :last-played
;;;; are not saved yet. They should link to an existing sfl/sf, not create a new one.

;;;; #'get-list-of-clm-calls needs a rework

;;;; more interesting xyz mapping
;;;; -> envelope follower, attack count, rms value

;;;; stereo-output for each layer?

(in-package :cl-user)

(defpackage :layers
  (:use :common-lisp)
  (:nicknames :ly))

(defun ly ()
  (in-package :layers))

(in-package :layers)

(unless (find-package :cl-pcg)
  (ql:quickload :cl-pcg))

(unless (find-package :slippery-chicken)
  (error "Package Slippery Chicken is needed but couldn't be found"))

(unless (fboundp 'os-path)
  (defun os-path (path)
    (let* ((new-path (substitute #\/ #\: path))
	   (device (if (char= #\/ (elt path 0))
		       (second (pathname-directory path))
		       (format nil "~{~a~}"
			       (loop with break until break for i from 0 collect
				    (let ((this (elt path i))
					  (next (elt path (1+ i))))
				      (when (or (char= #\: next)
						(char= #\/ next))
					(setf break t))
				      this)))))
	   (helper (subseq new-path (1+ (position #\/ new-path :start 1))))
	   (rest (if (char= #\/ (elt helper 0))
		     helper
		     (format nil "/~a" helper))))
      #+(or win32 win64) (format nil "~a:~a" device rest)
      #-(or win32 win64) (format nil "/~a~a" device rest))))

(unless (fboundp 'directory-name)
  (defun directory-name (path)
    (when (> (length path) 0)
      (loop until (char= #\/ (elt path (1- (length path)))) do
	    (setf path (subseq path 0 (1- (length path)))))
      path)))

(defun parent-dir (path)
  (subseq path 0 (position #\/ path :from-end t)))

;; is os-path neccessary here?
(defparameter *layers-src-dir*
  (directory-name (namestring *load-pathname*)))

(defparameter *src-dir* *layers-src-dir*)

(defparameter *layers-home-dir* (parent-dir *layers-src-dir*))

(defun quiet-warning-handler (c)
  (let ((r (find-restart 'muffle-warning c)))
    (when r 
      (invoke-restart r))))

;; clm makes a lot of annoying warnings :c
(when (ignore-errors clm::*clm*)
  (handler-bind ((warning
		  #'quiet-warning-handler))
    (load (compile-file (format nil "~a~a" *src-dir* "analysis.lsp")))))

;; *** load-all
;;; load most of the files
(defun load-all ()
  (dolist (file '("globals.lsp"
		  "utilities.lsp"
		  "base-object.lsp"
		  "random.lsp"
		  "list-object.lsp"
		  "markov.lsp"
		  "length-dependant-list.lsp"
		  "generate-structure.lsp"
		  "structure.lsp"
		  "stored-file.lsp"
		  "stored-file-list.lsp"
		  "layer.lsp"
		  "layers.lsp"
		  ))
    (load (probe-file (format nil "~a~a" *src-dir* file)))))
(load-all)

(when (ignore-errors clm::*clm*)
  (handler-bind ((warning
		  #'quiet-warning-handler))
    (load (compile-file (format nil "~a~a" *src-dir* "export-with-clm.lsp")))))

(let ((pack (find-package :layers)))
  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))

(format t "~&finished loading!")

;;;; EOF all.lsp
