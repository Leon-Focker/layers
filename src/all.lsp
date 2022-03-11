;;;; To do:
;;;; check reset (?) - layer-objects are updated - layers-ojects are not
;;;; variable rest probability multiplier?
;;;; create template
;;;; change probabilities depending on Formteil
;;;; multichannel?
;;;; build "sets" for songs, smoothly switch between sets, live transistions
;;;; one-time-use of samples? (eg. sample can only be played once every 3 mins)
;;;; dynamic panning
;;;; reseting should reset n of a layer as well

;;;; set-n looks for the layer in layers by id. abstract that function and
;;;; build in an error case (not found)

;;;; when markov-chain has no weight there is no error message
;;;; when changing n of a layer whilst playing (set-n). each time a slight delay
;;;; is added to the layer (something aroung 20 ms). Probably happens in PD. where?
;;;; PD needs to be restarted after every reload. else some timings seem to be
;;;; getting stuck. pls fix :c
;;;; changing position in coordinate space in Pd, needs function that slowly adjusts
;;;; position (moving average?) as to not jump to a new position

;;;; new method/function analyse sample - get duration and other parameters
;;;; automatically and use them for for x y z mapping when loading a folder

;; * Layers

(in-package :cl-user)

(defpackage :layers
  (:use :common-lisp)
  (:nicknames :ly))

(in-package :layers)

(defparameter *src-dir*
  (sc::trailing-slash (directory-namestring *load-pathname*)))

(dolist (file '("globals.lsp"
		"utilities.lsp"
		"base-object.lsp"
		"random.lsp"
		"list-object.lsp"
		"markov.lsp"
		"length-dependant-list.lsp"
		"structure.lsp"
		"soundfiles.lsp"
		"layer.lsp"
		"layers.lsp"))
  (load (probe-file (format nil "~a~a" *src-dir* file))))

;; * finished!
(format t "~&finished loading!")

;;;; EOF all.lsp
