(in-package :sc)

;;; load layers and set this file to current score file:

(load "/E/code/layers/src/layers.lsp")
(setf *score-file* *load-pathname*)

;; * stored files
;; ** snare

(defparameter *snare* (make-stored-file-list 'snare '()))


;;#################
(store-file-in-list
 (make-stored-file
  'snare
  "/one-shots/drums/snare.wav"
  '((snare 1))
  4.0
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5))
 *snare*)

;; ** accents

(defparameter *accents* (make-stored-file-list 'accents '()))

(store-file-in-list (create-rest) *accents*)

;;#################
(store-file-in-list
 (make-stored-file
  'accent1
  "/no-input/accents/accent1.wav"
  '((rest 3)
    (accent1 0)
    (accent2 1)
    (accent3 3)
    (accent4 3)
    (accent5 1)
    (accent6 1)
    (accent7 1)
    (accent8 1)
    (accent9 3))
  0.5
  :loop-flag nil
  :start 'random
  :panorama 'random
  :preferred-length '(0.3 5))
 *accents*)

;;#################
(store-file-in-list
 (make-stored-file
  'accent2
  "/no-input/accents/accent2.wav"
  '((rest 0)
    (accent1 1)
    (accent2 1)
    (accent3 1)
    (accent4 3)
    (accent5 3)
    (accent6 3)
    (accent7 5)
    (accent8 3)
    (accent9 1))
  0.1
  :loop-flag nil
  :start 'random
  :panorama 'random
  :preferred-length '(0.2 0.5))
 *accents*)

;;#################
(store-file-in-list
 (make-stored-file
  'accent3
  "/no-input/accents/accent3.wav"
  '((rest 1)
    (accent1 3)
    (accent2 1)
    (accent3 1)
    (accent4 1)
    (accent5 1)
    (accent6 1)
    (accent7 3)
    (accent8 4)
    (accent9 3))
  0.1
  :loop-flag nil
  :start 'random
  :panorama 'random
  :preferred-length '(0.8 1.5))
 *accents*)

;;#################
(store-file-in-list
 (make-stored-file
  'accent4
  "/no-input/accents/accent4.wav"
  '((rest 2)
    (accent1 3)
    (accent2 2)
    (accent3 1)
    (accent4 0)
    (accent5 1)
    (accent6 1)
    (accent7 1)
    (accent8 1)
    (accent9 2))
  0.7
  :loop-flag nil
  :start 'random
  :panorama 'random
  :preferred-length nil)
 *accents*)

;;#################
(store-file-in-list
 (make-stored-file
  'accent5
  "/no-input/accents/accent5.wav"
  '((rest 5)
    (accent1 1)
    (accent2 1)
    (accent3 1)
    (accent4 1)
    (accent5 5)
    (accent6 1)
    (accent7 4)
    (accent8 1)
    (accent9 1))
  0.2
  :loop-flag nil
  :start 'random
  :panorama 'random
  :preferred-length '(0.4 3))
 *accents*)

;;#################
(store-file-in-list
 (make-stored-file
  'accent6
  "/no-input/accents/accent6.wav"
  '((rest 2)
    (accent1 1)
    (accent2 1)
    (accent3 3)
    (accent4 1)
    (accent5 1)
    (accent6 1)
    (accent7 1)
    (accent8 1)
    (accent9 3))
  0.2
  :loop-flag nil
  :start 'random
  :panorama 'random
  :preferred-length '(0.3 2))
 *accents*)

;;#################
(store-file-in-list
 (make-stored-file
  'accent7
  "/no-input/accents/accent7.wav"
  '((rest 3)
    (accent1 2)
    (accent2 2)
    (accent3 2)
    (accent4 0)
    (accent5 1)
    (accent6 0)
    (accent7 1)
    (accent8 0)
    (accent9 2))
  0.3
  :loop-flag nil
  :start 'random
  :panorama 'random
  :preferred-length '(0.2 0.45))
 *accents*)

;;#################
(store-file-in-list
 (make-stored-file
  'accent8
  "/no-input/accents/accent8.wav"
  '((rest 3)
    (accent1 1)
    (accent2 1)
    (accent3 1)
    (accent4 4)
    (accent5 1)
    (accent6 1)
    (accent7 1)
    (accent8 1)
    (accent9 1))
  0.2
  :loop-flag t
  :start 'random
  :panorama 'random
  :preferred-length '(0.2 5))
 *accents*)

;;#################
(store-file-in-list
 (make-stored-file
  'accent9
  "/no-input/accents/accent9.wav"
  '((rest 2)
    (accent1 4)
    (accent2 1)
    (accent3 1)
    (accent4 1)
    (accent5 1)
    (accent6 1)
    (accent7 1)
    (accent8 5)
    (accent9 1))
  0.5
  :loop-flag nil
  :start 'random
  :panorama 'random
  :preferred-length nil)
 *accents*)

;; ** transformations
(defparameter *transformations* (make-stored-file-list 'transformations '()))

;; (store-file-in-list (create-rest) *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation1
  "/no-input/transformations/transformation1.wav"
  '((rest 0)
    (transformation1 1)
    (transformation2 1)
    (transformation3 1)
    (transformation4 1)
    (transformation5 1)
    (transformation6 1)
    (transformation7 1)
    (transformation8 1)
    (transformation9 1)
    (transformation10 1))
  15
  :loop-flag t
  :preferred-length '(12 34))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation2
  "/no-input/transformations/transformation2.wav"
  '((rest 0)
    (transformation1 1)
    (transformation2 1)
    (transformation3 1)
    (transformation4 1)
    (transformation5 1)
    (transformation6 1)
    (transformation7 1)
    (transformation8 1)
    (transformation9 1)
    (transformation10 1))
  5
  :loop-flag t
  :preferred-length '(16 30))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation3
  "/no-input/transformations/transformation3.wav"
  '((rest 0)
    (transformation1 1)
    (transformation2 1)
    (transformation3 1)
    (transformation4 1)
    (transformation5 1)
    (transformation6 1)
    (transformation7 1)
    (transformation8 1)
    (transformation9 1)
    (transformation10 1))
  5
  :loop-flag t
  :preferred-length '(2 42))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation4
  "/no-input/transformations/transformation4.wav"
  '((rest 0)
    (transformation1 1)
    (transformation2 1)
    (transformation3 1)
    (transformation4 1)
    (transformation5 1)
    (transformation6 1)
    (transformation7 1)
    (transformation8 1)
    (transformation9 1)
    (transformation10 1))
  3
  :loop-flag t
  :preferred-length '(8 12.5))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation5
  "/no-input/transformations/transformation5.wav"
  '((rest 0)
    (transformation1 1)
    (transformation2 1)
    (transformation3 1)
    (transformation4 1)
    (transformation5 1)
    (transformation6 1)
    (transformation7 1)
    (transformation8 1)
    (transformation9 1)
    (transformation10 1))
  4
  :loop-flag t
  :preferred-length '(5 20))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation6
  "/no-input/transformations/transformation6.wav"
  '((rest 0)
    (transformation1 1)
    (transformation2 1)
    (transformation3 1)
    (transformation4 1)
    (transformation5 1)
    (transformation6 1)
    (transformation7 1)
    (transformation8 1)
    (transformation9 1)
    (transformation10 1))
  20
  :loop-flag t
  :preferred-length '(10 50))
 *transformations*)
;;#################
(store-file-in-list
 (make-stored-file
  'transformation7
  "/no-input/transformations/transformation7.wav"
  '((rest 0)
    (transformation1 1)
    (transformation2 1)
    (transformation3 1)
    (transformation4 1)
    (transformation5 1)
    (transformation6 1)
    (transformation7 1)
    (transformation8 1)
    (transformation9 1)
    (transformation10 1))
  5
  :loop-flag t
  :preferred-length '(20 40))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation8
  "/no-input/transformations/transformation8.wav"
  '((rest 0)
    (transformation1 1)
    (transformation2 1)
    (transformation3 1)
    (transformation4 1)
    (transformation5 1)
    (transformation6 1)
    (transformation7 1)
    (transformation8 1)
    (transformation9 1)
    (transformation10 1))
  2
  :loop-flag t
  :preferred-length '(2 20))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation9
  "/no-input/transformations/transformation9.wav"
  '((rest 0)
    (transformation1 1)
    (transformation2 1)
    (transformation3 1)
    (transformation4 1)
    (transformation5 1)
    (transformation6 1)
    (transformation7 1)
    (transformation8 1)
    (transformation9 1)
    (transformation10 1))
  6
  :loop-flag t
  :preferred-length '(1 8))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation10
  "/no-input/transformations/transformation10.wav"
  '((rest 0)
    (transformation1 1)
    (transformation2 1)
    (transformation3 1)
    (transformation4 1)
    (transformation5 1)
    (transformation6 1)
    (transformation7 1)
    (transformation8 1)
    (transformation9 1)
    (transformation10 1))
  3
  :loop-flag t
  :preferred-length '(15 25))
 *transformations*)

;; * structure

;;; make structure:

(defparameter *structure*
  (make-structure '(2)
		  '((1 ((2 2)))
		    (2 ((1 1))))
		  '((1 1)
		    (2 1))
		  :id "test"
		  :type 'compartmentalise
		  :smallest 0.5))


;; * layers

;;; define each layer:

(defparameter *layer1* (make-layer '1 *snare* *structure* 4 0 nil))
(defparameter *layer2* (make-layer '2 *snare* *structure* 4 90 nil))
(defparameter *layer3* (make-layer '3 *accents* *structure* 4 45))
(defparameter *layer4* (make-layer '4 *transformations* *structure* 4 0))
(defparameter *layer5* (make-layer '5 *transformations* *structure* 4 90))

;;;define layers-object:

(defparameter *layers*
  (make-layers 'layers (list *layer1* *layer2* *layer3* *layer4* *layer5*)))
