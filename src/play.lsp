;; ** play
;;;; the interaction with pure data is happening through these functions

(in-package :layers)

;; *** set-timer
;;; sets timer within pure data to value in ms
(defun set-timer (time)
  (list 'timer time))

;; *** next-trigger
;;; this function is called, when the timer within pure data reaches 0
(defun next-trigger ()
  ;; subtract current next-trigger from all times until new sf
  ;; set next-trigger to minimum time to new sf
  ;; play all files that have time < 0.01 until next sf
  (play-this ....)
  (play-this ....)
  ;; set timer to next-trigger
  )

;; *** current-timer
;;; when offsetting the timer, this function is called to check, wheter triggers
;;; need to be skipped (because they already should have happened) and another
;;; offset needs to be added to the timer, because it currently is negative
(defun current-timer (current-timer)
  (unless (> current-timer 0)
    (;; skip next trigger points and get new offset
     (list 'timer-offset offset!!!!!!!!!!!!))))
