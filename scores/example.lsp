;;; if you want to listen to something in real time, open the pure data patch:
;;; ".../layers/pd/Layers.pd"

;;; load layers and set this file to current score file:

(unless (find-package 'ly)
  #-(or win32 win64)(load "/E/code/layers/src/all.lsp") ; set to correct path
  #+(or win32 win64)(load "e:/code/layers/src/all.lsp"))

(in-package :ly)

(setf *score-file* *load-pathname*)

;;; load soundfiles:
(load-from-same-dir "stored-files.lsp") ; fill in with your own samples!

;;; make structure:

(defparameter *structure*
  (make-fractal-structure '(2)
		  '((1 ((2 2)))
		    (2 ((1 1))))
		  '((1 1)
		    (2 1))
		  :smallest 1))

;;; define each layer:

(defparameter *layer1* (make-layer '1 *drums* *structure* 0 45))
(defparameter *layer2* (make-layer '2 *drums* *structure* 1 45))
(defparameter *layer3* (make-layer '3 *drumloops* *structure* 0 45))
(defparameter *layer4* (make-layer '4 *background* *structure* 4 0))
(defparameter *layer5* (make-layer '5 *background* *structure* 3 90))

;;; define layers-object:

(defparameter *layers*
  (make-layers 'layers (list *layer1* *layer2* *layer3* *layer4* *layer5*)))

(start-osc)

;; EOF example.lsp
