;; * layers test suite

;; this is very sparse for now :)

;; ** functionality

;;; because I don't want to test all of slippery chicken every time but still
;;; use it's testing capabilities, I'm doing the bare minimum here to make a
;;; unique testing suite for layers.

(in-package :ly)

(import '(sc-test-check file-write-ok-multi probe-delete-multi test-suite-file
	  equal-within-less-tolerance probe-delete file-size file-write-ok)
	'slippery-chicken)

(defparameter *ly-test-all-tests* nil)
(load-from-test-suite-dir "sc-test-suite-aux.lsp")

;;; 08.12.2011 SAR: Added a line to push the name of each newly defined test
;;; into the list of all tests
(defmacro ly-deftest (name parameters &body body)
  "Define a test function. Within a test function we can call other test
  functions or use 'sc-test-check' to run individual test cases."
  (unless (member `,name *ly-test-all-tests*) (push `,name *ly-test-all-tests*))
  `(defun ,name ,parameters
     (let ((sc::*sc-test-name* (append sc::*sc-test-name* (list ',name))))
       ,@body)))

;;; 08.12.11 SAR: Added a macro to test all tests stored in the
;;; *ly-test-all-tests* list
(defmacro ly-test-test-all ()
  "Run all tests whose names are stored within *ly-test-all-tests* (which
   should be all tests defined using ly-deftest)"  
  `(sc::sc-test-combine-results
     ;; MDE Thu Dec 15 22:54:32 2011 -- reverse so that they run in the order
     ;; in which they were defined  
    ,@(loop for at in (reverse *ly-test-all-tests*) collect (list at))))

(defun ly-test-next-test (last-test)
  (nth (1+ (position last-test *ly-test-all-tests*)) *ly-test-all-tests*)) 

;; ** utilities.lsp

(ly-deftest utils ()
  (let* ((ls1 '(0 1 2 3 4 5 6 7 8 9))
	 (ls2 '(0 1 (2) 3 (4 (5) 6) 7)))
    (sc-test-check
      ;; some little helpers:
      (equal (remove-nth 3 ls1) '(0 1 2 4 5 6 7 8 9))
      (equal (rotate ls1 4) '(4 5 6 7 8 9 0 1 2 3))
      (= (depth ls2) 3)
      (equal (normalize-depth ls2)
	     '(((0)) ((1)) ((2)) ((3)) ((4) (5) (6)) ((7))))
      (= (mirrors 10 1 3) 2)
      (equal (get-start-times ls1) '(0 0 1 3 6 10 15 21 28 36))
      (equal (get-durations ls1) '(1 1 1 1 1 1 1 1 1))
      (equal (avoid-repetition '(0 1 2 2 3 4 2)) '(0 1 2 3 4 2))
      (equal (insert ls1 5 22) '(0 1 2 3 4 22 5 6 7 8 9))
      (= (funcall (list-to-function '(1 2 3) 1) .5) 2)
      ;; some cooler ones:
      (equal (loop for i from 0 to 1 by .125 collect (beat-proximity i))
	     '(0 3 2 3 1 3 2 3 0))
      (equal (loop for i from 0 to 1 by 0.125 collect
		  (funcall (rqq-to-indispensability-function
			    '(2 ((2 ((2 (1 1)) (2 (1 1))))
				 (2 ((2 (1 1)) (2 (1 1)))))))
			   i))
	     '(0 7 3 5 1 6 2 4 0))
      (equal (rqq-to-indispensability '(8 (1 1 1 1 1 1 1 1)))
	     '(0 7 6 5 4 3 2 1))
      )))

;; ** morph.lsp

(ly-deftest test-morph ()
  (let* ((pts1 '((1 1 (1) 1 1 (1)) ((1) 1 1 (1) 1)))
	 (pts2 '((2 3 4) (5 (6) 7) (8 (9) (0))))
	 (pts3 '((2 3) (4 5 (6) 7) (8 (9) (0)))))
    (sc-test-check
     (= 0 (mod1 0))
     (= .5 (mod1 1.5))
     (= 1 (mod1 2))
     ;; morph-patterns
     (equal (patterns-to-rhythms-list pts1) '((1 1 1 1 1 1) (1 1 1 1 1)))
     (equal (morph-patterns pts2 25) '(2 3 (6) 3 7 3 (1)))
     (equal (morph-patterns pts2 0 t t 10) '(2 3 (6) 3 4 2 5 (6) 7 5))
     (equal (morph-patterns pts2 0 t t 10 t)
	    '((0 0) (0 1) (1 1) (0 1) (0 2) (0 0) (1 0) (1 1) (1 2) (1 0)))
     (equal (morph-patterns pts3 25 nil nil nil nil
			    (list-to-function '(0 4 5 2 3) 1))
	    '(2 4 5 (6) 7 1))
     ;; interpolate-patterns
     (equal (interpolate-patterns pts2 25)
	    '(2 87/25 3322/625 71732/15625 (2614317/390625)
	      (15975862/9765625)  12552463/9765625))
     (equal (interpolate-patterns pts3 25 t '(2 1)
				  '((0 0  1 1) (0 .2) (0 2 .5 1 1 0.1)))
	    '(2 114085069/33554432 3573547/1048576 2144933511/419430400
	      80530637/33554432 27468205/8388608 (17571859529/2621440000)))
     )))

;; EOF layers-test-suite.lsp
