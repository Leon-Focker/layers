;;;; To do:
;;;; check reset (?) - layer-objects are updated - layers-ojects are not
;;;; variable rest probability multiplier?
;;;; create template
;;;; multichannel?
;;;; build "sets" for songs, smoothly switch between sets, live transistions
;;;; one-time-use of samples? (eg. sample can only be played once every 3 mins)
;;;; dynamic panning
;;;; when changing n of a layer whilst playing (set-n). each time a slight delay
;;;;   is added to the layer (something aroung 20 ms). Probably happens in PD. where?
;;;; PD needs to be restarted after every reload. else some timings seem to be
;;;;   getting stuck. pls fix :c
;;;; (changing position in coordinate space in Pd, needs function that slowly adjusts
;;;;   position (moving average?) as to not jump to a new position)
;;;; while analysing soundfile, find better way to determine transients?
;;;; when trying to auto-scale, check wheter x y z are actually set.
;;;; implement layers-into-txt

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
		  "play.lsp"
		  ))
    (load (probe-file (format nil "~a~a" *src-dir* file))))
  (format t "~&finished loading!"))
(load-all)

;;;; EOF all.lsp
