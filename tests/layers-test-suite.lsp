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

;; ** base-object.lsp

(ly-deftest base-object ()
  (let ((bo (make-instance 'base-object :data '(0 1 2 3 4) :id 'testsuite)))
    (sc-test-check
	(equal (get-id bo) 'testsuite)
	(equal (data bo) '(0 1 2 3 4)))))

;; ** utilities.lsp

(ly-deftest utils ()
  (let* ((ls1 '(0 1 2 3 4 5 6 7 8 9))
	 (ls2 '(0 1 (2) 3 (4 (5) 6) 7))
	 (ls3 '(0 1 2 4 -1 6 3 4 2)))
    (probe-delete (format nil "~a~a" *src-dir* "midi-output.mid"))
    (sc-test-check
      ;; some little helpers:
      (equal (remove-nth 3 ls1) '(0 1 2 4 5 6 7 8 9))
      (equal (rotate ls1 4) '(4 5 6 7 8 9 0 1 2 3))
      ;; re-order
      (equal (re-order '(a b c d e) '(4 1 3 2 0)) '(e b d c a))
      (equal (re-order '(a b c d e) '(3 4 1)) '(d e b a c))
      (equal (re-order '(a b c d e) '(2 3 4 6 0 1)) '(c d e b a))
      (equal (re-order '(a b c d e) '(0 3 7 0 7 3 5)) '(a d c b e))
      (equal (re-order '(a b c d e) '(0 0 0)) '(a b c d e))
      ;; more small ones
      (= (depth ls2) 3)
      (= (rqq-depth '(1 ((1 (3)) 1 1))) 1)
      (equal (normalize-depth ls2)
	     '(((0)) ((1)) ((2)) ((3)) ((4) (5) (6)) ((7))))
      (= (mirrors 10 1 3) 2)
      (equal (get-start-times ls1) '(0 0 1 3 6 10 15 21 28 36))
      (equal (get-durations ls1) '(1 1 1 1 1 1 1 1 1))
      (equal (avoid-repetition '(0 1 2 2 3 4 2)) '(0 1 2 3 4 2))
      (equal (insert '(0 1 2 3 4 5 6 7 8 9) 5 22) '(0 1 2 3 4 22 5 6 7 8 9))
      (equal (insert-multiple '(0 1 2 3 4 5) '(2 4 2 2 1) '(a b c d e))
	     '(0 E 1 A C D 2 3 B 4 5))
      (= (dry-wet 3 5 .2) 3.4)
      (= (biggest-jump ls3) 7)
      (= (biggest-jump-up ls3) 7)
      (= (biggest-jump-down ls3) 5)
      (equal (reduce-by ls1 2)
	     '(1/2 5/2 9/2 13/2 17/2))
      ;; beat prox:
      (= (get-beat-prox 0) 0)
      (= (get-beat-prox 1) 0)
      (= (beat-proximity .66) 3)
      (= (thomaes-function .66 10) 8)
      (equal (loop for i from 0 to 1 by .125 collect (beat-proximity i))
	     '(0 3 2 3 1 3 2 3 0))
      ;; indispensability:
      (equal (loop for i from 0 to 1 by 0.125 collect
		  (funcall (rqq-to-indispensability-function
			    '(2 ((2 ((2 (1 1)) (2 (1 1))))
				 (2 ((2 (1 1)) (2 (1 1)))))))
			   i))
	     '(0 7 3 5 1 6 2 4 0))
      (equal (loop for i from 0 to 1 by 0.125 collect 
		  (funcall (rqq-to-indispensability-function
			    '(8 (1 1 1 1 1 1 1 1)))
			   i))
	     '(0 7 6 5 4 3 2 1 0))
      ;; list-interp
      (equal (loop for i from 1 to 9 by .4 collect
		  (list-interp i '(0 1 2 3 4 5 6 7 8 9)))
	     '(1 1.3999999 1.8 2.2 2.6000001 3.0000005 3.4000003
		3.8000002 4.2000003 4.6000004 5.0 5.4000006 5.8000007
		6.200001 6.600001 7.000001 7.400001 7.800001
	       8.200001 8.6 9))
      ;; list-into-function
      (= (funcall (list-to-function '(1 2 3) 1) .5) 2)
      ;; lists-into-envelopes
      (equal (lists-to-envelopes '(0 1 2 0 3 0.2 4 5 6 1)
				 '(0 1 2 3 4 5 6 7 8 9))
	     '((0 0 1 1 2 2) (0 3 3 4) (0.2 5 4 6 5 7 6 8) (1 9)))
      ;; function-to-env
      (equal (make-function-into-env #'sin)
	     '(0 0.0 0.1 0.09983342 0.2 0.19866933 0.3 0.29552022 0.4 0.38941833
	       0.5 0.47942555 0.6 0.5646425 0.70000005 0.6442177 0.8000001
	       0.71735615 0.9000001 0.783327))
      (equal (function-to-env #'(lambda (x) (1+ x)) 0 20 1)
	     '(0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13 14
	       14 15 15 16 16 17 17 18 18 19 19 20 20 21))
      ;; flatness
      (= (flatness-of-list '(0 1 2 3 4 5 6 7 8 9)) 0.2008)
      ;; midi (the test files should be written to tmp...)
      (lists-to-midi '(e3 54 c2) '(1) '(1 5) :velocity-list '(.1 .4 .8))
      (file-write-ok (format nil "~a~a" *src-dir* "midi-output.mid"))
      (equal (midi-file-to-list (format nil "~a~a" *src-dir* "midi-output.mid"))
       '((1.0 52 1.0 0.09448819 0 2.0) (1.0 36 1.0 0.79527557 0 2.0)
	 (5.0000005 54 1.0 0.39370078 0 6.0000005)))
      ;;
      (= (distance-between-points (vector 3 5 1) (vector 0 0 0)) 5.91608)
      (= (max-of-array (vector 0 3 6 2 6 1 -9) t) 9)
      (equal (max-of-array-with-index (vector 0 3 6 2 6 1 -9) t) '(-9 6))
      (= (get-spectral-centroid '((440 0.5) (630 0.46) (880 0.25))) 603.1405)
      )
    (probe-delete (format nil "~a~a" *src-dir* "midi-output.mid"))))

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
      (equal (interpolate-patterns pts3 25 t '(2 1)
				   '((0 0  1 1) (0 .2) (0 2 .5 1 1 0.1)) 10)
	     '(2 114085069/33554432 7549747/2097152 24/5 80530637/33554432
	       34393293/8388608 (28/5) 248302797/33554432 3355443/1048576
	       71/10))
      (equal (interpolate-envelopes '((0 1  .5 1  1 1) (0 0  .2 3  3 0)
				      (0 1  .2 4  1 1))
				    24)
	     '((0 1 3984589/8388608 7/6 4/3 5/6)
	       (0 3/4 13421773/33554432 5/3 11/6 7/12)
	       (0 1/2 5452595/16777216 13/6 7/3 1/3)
	       (0 1/4 1/4 8/3 17/6 1/12)
	       (0 0 13421773/67108864 37/12 8/3 1/6)
	       (0 1/4 6710887/33554432 10/3 13/6 5/12)
	       (0 1/2 13421773/67108864 43/12 5/3 2/3)
	       (0 3/4 13421773/67108864 23/6 7/6 11/12)))
      )))

;; EOF layers-test-suite.lsp
