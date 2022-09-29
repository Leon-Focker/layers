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
;;;; when trying to auto-scale, check wheter x y z are actually set.
;;;; implement layers-into-txt
;;;; when a layer is triggered (even though the remaining-time is > 0.01) by next-trigger
;;;;  and trigger-all is t, it should not start at the begining of the sample but rather skipp
;;;;  the already played part. -> tried to implement but is bugged, see #'next-trigger
;;;; better distinction between restart and reload
;;;; currently playback doesn't end when loop isn't true
;;;; set-n is implemented in a kinda dirty way (shouldn't have to call next-trigger) and
;;;;  setting n introduces a small general delay it seems like.

;;;; more interesting xyz mapping
;;;; -> envelope follower, attack count, rms value

;; * Layers

(in-package :cl-user)

(defpackage :layers
  (:use :common-lisp)
  (:nicknames :ly))

(defun ly ()
  (in-package :layers))

(in-package :layers)

(defparameter *src-dir*
  (sc::trailing-slash (directory-namestring *load-pathname*)))

(defun quiet-warning-handler (c)
  (let ((r (find-restart 'muffle-warning c)))
    (when r 
      (invoke-restart r))))

;; clm makes a lot of annoying warnings :c
(handler-bind ((warning
		#'quiet-warning-handler))
  (load (compile-file (format nil "~a~a" *src-dir* "analysis.lsp"))))

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
    (load (probe-file (format nil "~a~a" *src-dir* file))))
  (format t "~&finished loading!"))
(load-all)

;;;; EOF all.lsp
