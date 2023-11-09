;; ** globals
;;;; global variables for layers
;;;; some user settable, some not pls c:

(in-package :layers)

;; *** custom setup (front end)
(defparameter *total-length* 100)
(defparameter *seed* 5)
;; what is the maximum length for the smallest value in the structure?
(defparameter *max-smallest-sample-length* 0.003)
(defparameter *use-sample-clouds* t) ;; skips markov chains, uses x y z
;; when true, the layer decides the panorama value for all files played in it
;; when false, each file is panned according to its panorama value
(defparameter *use-pan-of-layer* t)
(defparameter *use-preferred-lengths* t) ; check wheter sf likes current length
(defparameter *loop* nil)
(defparameter *start-stop* t)
(defparameter *pd-on-windows* t) ;; is your pd-version running on windows?
(defparameter *load-risky-files* nil) ;; load clm and sketch,creates warnings :(
(defparameter *print-to-console* nil)

;; *** not for user (back end)
(defparameter *all-layers* '()) ;; list with all layers objects that were called
(defparameter *random-number* nil)
(defparameter *next-trigger* 0) ;; time until next sample(s) will be triggered in ms
(defparameter *default-sample-dir*
  (format nil "~a~a" (parent-dir *layers-home-dir*) "/samples"))
(defparameter *x-y-z-position* (vector 0 0 0))
(defparameter *score-file* nil) ;; set which file to reload to reset
(defparameter *layers-buffer* '()) ;; variable used to temporarily store data
(defparameter *debug* '()) ;; hopefully not needed for now
(defparameter *current-time* 0)
(defparameter *current-timer* 0)

;;;; EOF globals.lsp
