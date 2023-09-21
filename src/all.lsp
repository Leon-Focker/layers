;; * Layers

;; ** TODO:
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

;;;; use layers with pd as live version of fplay:
;;;; - (start values) then functions to determine next file and new parameters
;;;; - lists to determine -"-

;;;; stereo-output for each layer?

;;;; usage without clm?

(in-package :cl-user)

;; ** Dependencies

(unless (and (find-package 'slippery-chicken) (find-package 'clm))
  (error "slippery chicken and clm need to be installed and loaded for layers ~
          to work..."))

(unless (find-package :cl-pcg)
  (handler-case
      (ql:quickload :cl-pcg)
    (error (condition)
      (error "please make sure that the cl-pcg library is installed correctly"
	     ))))

;; ** Package

(defpackage :layers
  (:use :common-lisp :slippery-chicken)
  (:nicknames :ly))

(defun ly () (in-package :layers))

(in-package :layers)

;; ** Paths
(defparameter *layers-home-dir*
  (format nil "~A" (asdf:system-source-directory 'layers)))

(defparameter *layers-src-dir*
  (format nil "~asrc/" *layers-home-dir*))

(defparameter *src-dir* *layers-src-dir*)

(defun load-layers-src-file (file)
  (load (format nil "~a~a" *layers-src-dir* file)))

;; ** Load

(defun quiet-warning-handler (c)
  (let ((r (find-restart 'muffle-warning c)))
    (when r 
      (invoke-restart r))))

;; clm makes a lot of annoying warnings :c
(when (ignore-errors clm::*clm*)
  (handler-bind ((warning
		  #'quiet-warning-handler))
    (load (compile-file (format nil "~a~a" *layers-src-dir* "analysis.lsp")))
    ;; Michael Edwards samp1 instrument, but you can select the input channel:
    ;; It would also be possible to use samp5 (slippery chicken).
    (load (compile-file (format nil "~a~a" *layers-src-dir* "Samp0.ins")))))

(import '(clm::samp0))

;;; load all .lsp files
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
		  "morph.lsp"
		  "transitions.lsp"
		  "fplay.lsp"
		  ))
    (load-layers-src-file file)))
(load-all)

(when (ignore-errors clm::*clm*)
  (handler-bind ((warning
		  #'quiet-warning-handler))
    (load (compile-file (format nil "~a~a" *layers-src-dir* "export-with-clm.lsp")))))

;; *** export symbols
(let ((pack (find-package :layers)))
  (do-all-symbols (sym pack)
    ;; these are most of the symbol names that are also found in sc, but since
    ;; we now use sc in the layers package there should be no clashing...
    #|(unless (member sym '(id duration data name total get-next rhythms path
    value this minimum pitch scale env centroid visualize ;
    double tempo))|#
    (when (eql (symbol-package sym) pack) (export sym)))) ;)

;; export some symbols that are not exported because they are already in sc etc.
(loop for sym in '(data duration) do (export sym))

(format t "~&finished loading!")

;;;; EOF all.lsp
