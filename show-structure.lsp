(ql:quickload :sketch)
(defpackage :show-structure (:use :cl :sketch))
(in-package :show-structure)

(defun show-structure (structure-list)
  (defsketch structure-vis ())
  (make-instance 'structure-vis)
  (defsketch structure-vis ((title "structure")
			(width 1000)
			(height 600))
    (loop for ls in structure-list and k from 0 do
	 (setf ls (mapcar (lambda (x) (sc::rescale x
						   0
						   (loop for j in ls sum j)
						   0
						   1000))
			  ls))
	 (loop for val in ls and last = 0 then (+ last val) and i from 0 do
	      (with-pen (make-pen :fill (lerp-color +black+ +white+
						    (/ val (apply #'max ls))))
		(rect last (* k 100) val 100))))))
