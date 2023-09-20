;;; load layers and set this file to current score file:

(unless (find-package 'ly)
  #-(or win32 win64)(load "/E/code/layers/src/all.lsp")
  #+(or win32 win64)(load "e:/code/layers/src/all.lsp"))

(in-package :ly)

(setf *score-file* *load-pathname*)

;;; load soundfiles:

(load-from-same-dir "stored-files.lsp")

;;; make structure:

(defparameter *structure*
  (make-structure '(2)
		  '((1 ((2 1)))
		    (2 ((3 1 3)))
		    (3 ((2))))
		  '((1 1)
		    (2 5)
		    (3 .2))))

(defparameter *structure1*
  (make-structure '(2)
		  '((1 ((1 2)))
		    (2 ((3 1 3)))
		    (3 ((1 1 3 1))))
		  '((1 1)
		    (2 4)
		    (3 .2))))

(defparameter *structure2*
  (make-structure '(2)
		  '((1 ((2 2)))
		    (2 ((1 1))))
		  '((1 1)
		    (2 1))
		  :smallest 1))

;;; define each layer:

(defparameter *layer1* (make-layer '1 *drums* *structure2* 0 45))
(defparameter *layer2* (make-layer '2 *drums* *structure2* 1 45))
(defparameter *layer3* (make-layer '3 *drumloops* *structure2* 0 45))
(defparameter *layer4* (make-layer '4 *background* *structure2* 4 0))
(defparameter *layer5* (make-layer '5 *background* *structure2* 3 90))

;;;define layers-object:

(defparameter *layers*
  (make-layers 'layers (list *layer1* *layer2* *layer3* *layer4* *layer5*)))
