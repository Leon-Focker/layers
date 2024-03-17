(ql:quickload :imago)
; (ql:quickload :imago/pngload)

(in-package :imago)

(defun show-structure (structure-list &optional file (size-factor 1))
  (let* ((h (* 100 size-factor))
	 (w (* 1000 size-factor))
	 (ls (loop for l in structure-list collect
		  (mapcar (lambda (x) (sc::rescale x
						   0
						   (loop for j in l sum j)
						   0
						   1))
			  l)))
	 (length (length ls))
	 (height (* h length))
	 (array (make-array `(,height ,w)))
	 (n 0)
	 (sublist '())
	 (sublength 0)
	 (index 0)
	 (color 0)
	 (element 0)
	 (name (or file (format nil "~a~a" ly::*src-dir* "structure.png"))))
    (loop for y from 0 to (1- height) do
	 (setf n (floor y h))
	 (setf sublist (nth n ls))
	 (setf sublength (length sublist))
	 (loop for x from 0 to (1- w) do
	      (setf index (sc::decider (/ x w) sublist))
	      (setf element (nth index sublist))
	      (setf color (make-color (floor (* (expt (- 1 (/ element 1)) 15) 255))
				      (floor (* (/ n length) 100))
				      (floor (* (/ index sublength) 100)))) ;(/ n length)
	      (setf (aref array y x) color)
	      ))
    (write-png
     (make-instance 'rgb-image
		    :pixels array)
     name)
    name))

(in-package :layers)

(defmethod visualize-structure ((st structure) &optional file (size-factor 1))
  (format t "~&visualizing structure: ~a" (id st))
  (imago::show-structure (reverse (reverse (data st))) file size-factor))

#|
(ql:quickload :sketch)

(in-package :layers)

(defun show-structure (structure-list)
  ;(sketch::defsketch structure ())
  ;(sketch::make-instance 'structure)
  (sketch::defsketch structure ((title "structure")
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
	      (sketch::with-pen
		  (sketch::make-pen :fill
				    (sketch::lerp-color sketch::+black+ sketch::+white+
						    (/ val (apply #'max ls))))
		(sketch::rect last (* k 100) val 100))))))
|#

;; EOF show-structure.lsp
