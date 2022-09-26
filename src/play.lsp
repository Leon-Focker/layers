;; ** play
;;;; the interaction with pure data is happening through these functions

(in-package :layers)

;; *** set-timer
;;; sets timer within pure data to value in ms, usually next-trigger is used to do this.
(defun set-timer (time)
  (unless (numberp time) (error "time in set-timer must be a number"))
(list 'timer time))

;; *** next-trigger
;;; this function is called, when the timer within pure data reaches 0
(defmethod next-trigger ((lys layers) current-time)
  ;; subtract current next-trigger from time until next trigger of each layer
  ;; and look for the times until the next layer will be triggered
  (let* ((next-triggers (loop for ly in (data lys)
			   do (setf (remaining-duration ly)
				    (- (remaining-duration ly) *next-trigger*))
			   collect (let* ((remaining (remaining-duration ly)))
				     (if (> remaining 0.01)
					 remaining
					 (next-trigger-for-layer remaining ly))))))
    ;; (format t "~&next-triggers: ~a" next-triggers)
    ;; set *next-trigger* to the minimal time until the next layer needs one
    (setf *next-trigger* (* 0.01 (round (* 100 (apply #'min next-triggers)))))
    ;; layers with remaining time < 0.01 are put in a list
    (setf next-triggers (loop for ly in (data lys)
			   when (<= (remaining-duration ly) 0.01)
			   collect ly))
    ;; set timer to next-trigger and send list of layers that will be triggered
    (format t "~&current-time: ~a" current-time)
    (append
     (list 'trigger *next-trigger*)
     (loop for i in next-triggers append (play-this i)))))

;; *** next-trigger-for-layer
;;; helper function
(defun next-trigger-for-layer (remaining ly &optional (index 1))
  (setf remaining (+ remaining (see-with-relative-index (list-of-durations ly) index)))
  (if (> remaining 0.01) remaining (next-trigger-for-layer remaining ly (+ index 1))))

;; loop over next-triggers and call play-this

;; *** current-timer
;;; when offsetting the timer, this function is called to check, wheter triggers
;;; need to be skipped (because they already should have happened) and another
;;; offset needs to be added to the timer, because it currently is negative
(defun current-timer (current-timer)
  (unless (> current-timer 0)
    (;; skip next trigger points and get new offset
     (list 'timer-offset offset!!!!!!!!!!!!))))
