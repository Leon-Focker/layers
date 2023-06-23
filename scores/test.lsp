(in-package :ly)

;;; load layers and set this file to current score file:

(unless (fboundp 'ly::layers-has-been-loaded)
  (load "/E/code/layers/src/layers.lsp"))
(setf *score-file* *load-pathname*)

;; * stored files
;; ** glitch


(defparameter *glitch* (make-stored-file-list 'glitch '()))
(defparameter *glitch-txt* "/E/code/layers/scores/glitch.txt")

(if (probe-file *glitch-txt*)
    (progn
      (folder-to-stored-file-list *glitch*
				  "/E/code/layers/samples/one-shots/glitch/"
				  :decay 100
				  :auto-map t
				  :auto-scale-mapping t
				  :remap t)
      (store-in-text-file *glitch* *glitch-txt*))
    (setf *glitch* (load-from-file *glitch-txt*)))

(defparameter *one-shots* (make-stored-file-list 'one-shots '()))

(folder-to-stored-file-list *one-shots*
			    "/E/code/layers/samples/one-shots/bass/"
			    :decay 100
			    :auto-map t)
(folder-to-stored-file-list *one-shots*
			    "/E/code/layers/samples/one-shots/breath/"
			    :decay 100
			    :auto-map t)
(folder-to-stored-file-list *one-shots*
			    "/E/code/layers/samples/one-shots/drums/"
			    :decay 100
			    :auto-map t)
(folder-to-stored-file-list *one-shots*
			    "/E/code/layers/samples/one-shots/glitch/"
			    :decay 100
			    :auto-map t)
(folder-to-stored-file-list *one-shots*
			    "/E/code/layers/samples/one-shots/whispers/"
			    :decay 100
			    :auto-map t)

(auto-scale-mapping *one-shots* :remap t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *drums1* (make-stored-file-list 'drums1 '()))

(folder-to-stored-file-list *drums1*
			    "/E/code/layers/samples/one-shots/drums/"
			    :decay 100
			    :auto-map t
			    :auto-scale-mapping t
			    :remap t)

;; ** snare

(defparameter *snare* (make-stored-file-list 'snare '()))


;;#################
(store-file-in-list
 (make-stored-file
  'snare
  "/one-shots/drums/snare.wav"
  :markov '((snare 1))
  :decay 4.0
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5))
 *snare*)

;; ** drums

(defparameter *drums* (make-stored-file-list 'drums '()))

;;(store-file-in-list (create-rest) *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'kick1
  "/one-shots/drums/kick1.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0
  :y 0
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'kick2
  "/one-shots/drums/kick2.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.35
  :y 0
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'kick3
  "/one-shots/drums/kick3.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.7
  :y 0
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'kick4
  "/one-shots/drums/kick4.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 1
  :y 0
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'snare1
  "/one-shots/drums/snare1.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0
  :y 0.3
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'snare2
  "/one-shots/drums/snare2.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.35
  :y 0.5
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'snare3
  "/one-shots/drums/snare3.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.7
  :y 0.7
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'snare4
  "/one-shots/drums/snare4.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 1
  :y 1
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'hat1
  "/one-shots/drums/hat1.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.2
  :y 0.2
  :z 0.5)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'hat2
  "/one-shots/drums/hat2.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.4
  :y 0.4
  :z 0.6)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'hat3
  "/one-shots/drums/hat3.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.7
  :y 0.7
  :z 0.7)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'hat4
  "/one-shots/drums/hat4.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.8
  :y 0.8
  :z 0.9)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'ride1
  "/one-shots/drums/ride1.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0
  :y 0
  :z 1)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'ride2
  "/one-shots/drums/ride2.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0
  :y 1
  :z 1)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'ride3
  "/one-shots/drums/ride3.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 1
  :y 0
  :z 1)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'ride4
  "/one-shots/drums/ride4.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 1
  :y 1
  :z 1)
 *drums*)

#|
;; ** accents

(defparameter *accents* (make-stored-file-list 'accents '()))

(store-file-in-list (create-rest) *accents*)

;;#################
(store-file-in-list
 (make-stored-file
  'accent1
  "/no-input/accents/accent1.wav"
  :markov '((rest 3)
    (accent1 0)
    (accent2 1)
    (accent3 3)
    (accent4 3)
    (accent5 1)
    (accent6 1)
    (accent7 1)
    (accent8 1)
    (accent9 3))
  :decay 0.5
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
  :markov '((rest 0)
    (accent1 1)
    (accent2 1)
    (accent3 1)
    (accent4 3)
    (accent5 3)
    (accent6 3)
    (accent7 5)
    (accent8 3)
    (accent9 1))
  :decay 0.1
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
  :markov '((rest 1)
    (accent1 3)
    (accent2 1)
    (accent3 1)
    (accent4 1)
    (accent5 1)
    (accent6 1)
    (accent7 3)
    (accent8 4)
    (accent9 3))
  :decay 0.1
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
  :markov '((rest 2)
    (accent1 3)
    (accent2 2)
    (accent3 1)
    (accent4 0)
    (accent5 1)
    (accent6 1)
    (accent7 1)
    (accent8 1)
    (accent9 2))
  :decay 0.7
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
  :markov '((rest 5)
    (accent1 1)
    (accent2 1)
    (accent3 1)
    (accent4 1)
    (accent5 5)
    (accent6 1)
    (accent7 4)
    (accent8 1)
    (accent9 1))
  :decay 0.2
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
  :markov '((rest 2)
    (accent1 1)
    (accent2 1)
    (accent3 3)
    (accent4 1)
    (accent5 1)
    (accent6 1)
    (accent7 1)
    (accent8 1)
    (accent9 3))
  :decay 0.2
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
  :markov '((rest 3)
    (accent1 2)
    (accent2 2)
    (accent3 2)
    (accent4 0)
    (accent5 1)
    (accent6 0)
    (accent7 1)
    (accent8 0)
    (accent9 2))
  :decay 0.3
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
  :markov '((rest 3)
    (accent1 1)
    (accent2 1)
    (accent3 1)
    (accent4 4)
    (accent5 1)
    (accent6 1)
    (accent7 1)
    (accent8 1)
    (accent9 1))
  :decay 0.2
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
  :markov '((rest 2)
    (accent1 4)
    (accent2 1)
    (accent3 1)
    (accent4 1)
    (accent5 1)
    (accent6 1)
    (accent7 1)
    (accent8 5)
    (accent9 1))
  :decay 0.5
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
  :markov '((rest 0)
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
  :decay 15
  :loop-flag t
  :preferred-length '(12 34))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation2
  "/no-input/transformations/transformation2.wav"
  :markov '((rest 0)
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
  :decay 5
  :loop-flag t
  :preferred-length '(16 30))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation3
  "/no-input/transformations/transformation3.wav"
  :markov '((rest 0)
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
  :decay 5
  :loop-flag t
  :preferred-length '(2 42))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation4
  "/no-input/transformations/transformation4.wav"
  :markov '((rest 0)
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
  :decay 3
  :loop-flag t
  :preferred-length '(8 12.5))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation5
  "/no-input/transformations/transformation5.wav"
  :markov '((rest 0)
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
  :decay 4
  :loop-flag t
  :preferred-length '(5 20))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation6
  "/no-input/transformations/transformation6.wav"
  :markov '((rest 0)
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
  :decay 20
  :loop-flag t
  :preferred-length '(10 50))
 *transformations*)
;;#################
(store-file-in-list
 (make-stored-file
  'transformation7
  "/no-input/transformations/transformation7.wav"
  :markov '((rest 0)
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
  :decay 5
  :loop-flag t
  :preferred-length '(20 40))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation8
  "/no-input/transformations/transformation8.wav"
  :markov '((rest 0)
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
  :decay 2
  :loop-flag t
  :preferred-length '(2 20))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation9
  "/no-input/transformations/transformation9.wav"
  :markov '((rest 0)
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
  :decay 6
  :loop-flag t
  :preferred-length '(1 8))
 *transformations*)

;;#################
(store-file-in-list
 (make-stored-file
  'transformation10
  "/no-input/transformations/transformation10.wav"
  :markov '((rest 0)
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
  :decay 3
  :loop-flag t
  :preferred-length '(15 25))
 *transformations*)
|#

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

(defparameter *structure1*
  (make-structure '(2)
		  '((1 ((1 2)))
		    (2 ((3 1 3)))
		    (3 ((1 1 3 1))))
		  '((1 1)
		    (2 4)
		    (3 .2))
		  :type 'lindenmayer))
;; * layers

;;; define each layer:

(defparameter *layer1* (make-layer '1 *drums1* *structure1* 2 0 nil))
(defparameter *layer2* (make-layer '2 *drums1* *structure1* 2 90 nil))
(defparameter *layer3* (make-layer '3 *glitch* *structure1* 0 90))
(defparameter *layer4* (make-layer '4 *glitch* *structure1* 1 0))
(defparameter *layer5* (make-layer '5 *one-shots* *structure1* 2 45))

;;;define layers-object:

(setf *layers*
  (make-layers 'layers (list *layer1* *layer2* *layer3* *layer4* *layer5*)))
