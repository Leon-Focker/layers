;; * transitions.lsp
;;; Collection of transition-functions, basically a way of generating lists.
;;; These are intended to use as "control data", for example as a
;;; morphing-function in morph-patterns. But of course they can be used in any
;;; way possible.

(in-package :ly)

;; ** fibonacci-transitions
;;; see sc::fibonacci-transitions

;; ** procession
;;; see sc::procession

;; ** kernel-transition

;; *** move-item
;;; move items from a certain index to a new index. This is not achieved
;;; by swapping items! The item is taken from the list and then placed into
;;; the list at the new position.
(defun move-item (list index new-index)
  (let ((index (mod index (length list)))
	(new-index (mod new-index (length list))))
    (unless (= index new-index)
      (let* ((min (min index new-index))
	     (max (max index new-index))
	     (diff (1+ (- max min)))
	     (list-head (subseq list 0 min))
	     (list-tail (nthcdr min list))
	     (mid '()))
	(setf mid (subseq list-tail 0 diff)
	      mid (if (> index new-index)
		      (append (last mid) (subseq mid 0 (1- diff)))
		      (append (cdr mid) (list (first mid))))
	      list-tail (nthcdr diff list-tail)
	      list (append list-head mid list-tail))))
    list))

;; *** kernel-transitions
;;; unpredictable transition between items
;;; total-items - length of the list of results
;;; levels - either a number (how many different elements)
;;;  or a list, with the elements to transition over
;;; kernel - list of numbers, as many numbers as wanted
;;;  every number represents items moved to the left (positive) or the right
;;;  (negative) during a loop. - Just experiment :D
;;; e - exponent for the list we start with, controls the ratio of items used in
;;;  the final list.
;;;  Below 1 -> more of the higher levels: 0.5 => (0 0 1 1 1 1)
;;;  Above 1 -> more of the lower levels:  2   => (0 0 0 0 0 1)
#|
(kernel-transitions 20 3 '(-3 2 4))
=> (1 1 1 1 0 1 1 2 2 2 2 0 2 0 0 0 0 0 1 2)
|#
(defun kernel-transitions (total-items levels &optional (kernel '(0)) (e 1))
  ;; make sure that all arguments are valid:
  (unless (and (numberp total-items) (numberp e) (listp kernel))
    (error "total-items and e must be a number, kernel a list in ~
            make-transition but are: ~&~a~&~a~&~a"
	   total-items e kernel))
  (unless (or (numberp levels) (listp levels))
    (error "levels must either be a number or a list in make-transition ~
              but is: ~&~a" levels))
  (let* ((result '())
	 (total-levels levels))
    ;; make sure levels is a list and total-levels its length:
    (if (numberp levels)
	(setf levels (loop for i below levels collect i))
	(setf total-levels (length levels)))
    ;; get a basic transition to then transform:
    (setf result
	  (loop for i below 1 by (/ 1 total-items)
	     collect (nth (floor (* (expt i e) total-levels)) levels)))
    ;; move everything around according to kernel:
    (loop for i to (1- total-items) do
	 (loop for j in kernel do
	      (setf result (move-item result i (- i j)))))
    result))

;; ** window-transition

;; *** index-of-min-within-range
;;; get the index of the smallest element in an array,
;;; within a min and max index
(defun index-of-min-within (array &optional (from 0) (to (1- (length array))))
  (unless (arrayp array)
    (error "index-of-min-within-range didn't get an array as input: ~a" array))
  (loop for i from from to (min to (1- (length array)))
     with min-i = from with min = (aref array i)
     when (< (aref array i) min) do (setf min-i i min (aref array i))
     finally (return min-i)))

;; *** window-transitions
;;; make a transition over items, using a sort of window.
;;; total-items - length of the list of results
;;; levels - either a number (how many different elements)
;;;  or a list, with the elements to transition over
;;; e - exponent for the list we start with, controls the ratio of items used in
;;;  the final list.
;;;  Below 1 -> more of the higher levels: 0.5 => (0 0 1 1 1 1)
;;;  Above 1 -> more of the lower levels:  2   => (0 0 0 0 0 1)
;;; window-size - controls which items can be chosen at any point. The closer to
;;;  1 it is, the longer will earlier items be chosen. If it is 0, no earlier
;;;  item will be chosen.
;;;  This controls which items can be chosen at any point. If 0, only the item
;;;  at that point in the original sequence is chosen, If 1, all items can be
;;;  chosen at the end of the transition.
;;; control-function - this should be a function with two arguments - total-items
;;;  and levels, which will be called for the generation of the control-sequence
;;;  instead of the linear/exponential standard function. Thus 'e will be ignored.
;;;  the function must return a list!
;;;  for example 'fibonacci-transitions or 'procession.
#|
EXAMPLE
(window-transitions 20 3)
=> (0 0 0 0 0 0 0 1 1 1 0 1 0 1 2 2 2 0 2 1)
=> #(10 6 4)
|#
(defun window-transitions (total-items levels &optional (e 1) (window-size 0.5)
						control-function)
  ;; make sure that all arguments are valid:
  (unless (and (numberp total-items) (numberp e) (numberp window-size)
	       (>= total-items 0) (>= e 0) (>= window-size 0))
    (error "total-items, e and window-size must be a positive number in ~
            make-transition but are: ~&~a~&~a~&~a"
	   total-items e window-size))
  (unless (or (numberp levels) (listp levels))
    (error "levels must either be a number or a list in make-transition ~
              but is: ~&~a" levels))
  (let* ((control '())
	 (window 0)
	 (weights '())
	 (spread '())
	 (total-levels levels))
    ;; make sure levels is a list and total-levels its length:
    (if (numberp levels)
	(setf levels (loop for i below levels collect i))
	(setf total-levels (length levels)))
    ;; get actual window, control sequence and initial weights:
    (setf window
	  (round (* window-size total-levels))
	  control
	  (if control-function
	      (funcall control-function total-items total-levels)
	      (loop for i below 1 by (/ 1 total-items)
		    ;; other control sequences might be fun:
		    ;; collect (* (expt (sin (* i 20)) e) total-levels))
		    collect (* (expt i e) total-levels)))
	  weights
	  (make-array total-levels :initial-element 0)
	  spread
	  (make-array total-levels :initial-element 0))
    ;; *magically* generate the actual result, using window and weights:
    (values
     (if (= window-size 0)
	 control
	 (loop for el in control
	    for min = (max 0 (floor (- el window)))
	    for n = (index-of-min-within weights min el)
	    do (incf (aref weights n) el)
	      (incf (aref spread n))
	    collect (nth (floor n) levels)))
     spread)))

;; EOF transitions.lsp
