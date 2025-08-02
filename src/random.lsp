;; ** random

(in-package :layers)

(defun random-number (&optional seed)
  (make-random-number seed))
  
;;; set the global variable *random-number* to an instance of random-number
(setf *random-number* (random-number))

;; EOF random.lsp
