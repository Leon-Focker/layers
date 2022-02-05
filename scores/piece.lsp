(in-package :sc)

;;; load soundfiles:

(load "/E/code/layers/src/layers.lsp")
(load "/E/code/layers/scores/stored-files.lsp")

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

;;; define each layer:

(defparameter *layer1* (make-layer '1 *whispers* *structure1* 0 45))
(defparameter *layer2* (make-layer '2 *noisy* *structure1* 1 45))
(defparameter *layer3* (make-layer '3 *drumloops* *structure1* 0 45))
(defparameter *layer4* (make-layer '4 *background* *structure1* 4 0))
(defparameter *layer5* (make-layer '5 *background* *structure1* 3 90))

;;;define layers-object:

(defparameter *layers*
  (make-layers 'layers (list *layer1* *layer2* *layer3* *layer4* *layer5*)))
