;; ** fplay

(in-package :ly)

#|
;; *** dynamic-collect
;;; give any amount of arguments to a collect in a loop in a macro :)
;;; naybe useless?
(defmacro dynamic-collect (&rest rest)
  `(loop for i in (quote ,rest) collect 'collect collect i))

(defmacro dynamic-collect-ls (rest)
  `(loop for i in ,rest collect 'collect collect i))
|#

;; *** name-var
;;; name a variable after the following scheme:
;;; i = 1 => name, i = 2 => name2, i = 3 => name3 ...
(defmacro name-var (name i)
  `(progn (unless (symbolp ,name)
	    (error "name must be a symbol in name-var: ~a" ,name))
	  (unless (numberp ,i)
	    (error "i must be an number in name-var: ~a" ,i))
	  (if (<= ,i 1) ,name (read-from-string (format nil "~a~a" ,name ,i)))))

;; *** name-var-highest
(defmacro name-var-highest (name i arg-list)
  `(let* ((ass (assoc ,name ,arg-list))
	  (len (1- (length ass)))
	  (n (min ,i len)))
     (name-var ,name n)))
  
;; *** get-loop-vars
;;; return a list with statements of type 'for var-name = var-def'
;;; arg-list - list of lists, these contain a name for a variable
;;;  as first element, then how those varable is defined in the loop
;;;  if there is more than one definition (eg. the sublist is longer
;;;  than 2 elements, (1- length) variables will be created. The first
;;;  variable is always called var-name, after that: var-name2, var-name3...
;;;  The definition can also be a string and will.
;;; There is one exception: when var-name = time, 'with is used, not 'for.
;;; EXAMPLE
#|
(get-loop-vars '((time 5) (duration) (rhythm (nth i ls)) (sound 2 "4 then 5")))
=> (WITH TIME = 5 FOR RHYTHM = (NTH I LS) FOR SOUND = 2 FOR SOUND2 = 4 THEN 5)
|#
(defmacro get-loop-vars (arg-list)
  `(progn
     (unless (listp ,arg-list)
       (error "arg-list in get-loop-vars must be a list: ~a" ,arg-list))
     (loop for el in
	  (loop for var in ,arg-list
	     for len = (length var)
	     for var-name = (first var)
	     for substring = (when (> (length (string var-name)) 3)
			       (subseq (string var-name) 0 4))
	     for flag = (or (equal "TIME" substring))
	     do (unless (symbolp var-name)
		  (error "invalid name for a variable: ~a" var-name))
	       (when (> len 10)
		 (warn "are you sure about ~a different instances of ~a?"
		       len var-name))
	     append (when (> len 1)
		      (loop for i from 1 for var-def in (cdr var)
			 collect (if flag 'with 'for)
			 collect (name-var var-name i)
			 collect '=
			 collect var-def)))
	append (if (stringp el) (string-to-list el) (list el)))))

;; *** merge-var-lists
;;; push elements from list 'from into list 'into, but only if no sublist with
;;; the same first element is already in 'into.
(defmacro merge-var-lists (from into)
  `(progn (unless (and (listp ,from) (listp ,into))
	    (error "from and into in merge-var-lists must be lists: ~a, ~a"
		   ,from ,into))
	  (loop for el in ,from unless (assoc (first el) ,into)
	     do (push el ,into))))

;; *** fplay-get-loop-vars
;;; this is basically the body of fplay:
;;; collects all variable names and definitions that are needed for fplay
;;; and then generates them using #'get-loop-vars. There's probably still a few
;;; bugs that I haven't discovered yet.
;;; no pretty error message if sound is nil and file doesnt get anything useful
(defmacro fplay-get-loop-vars (start-time end-time arg-list)
  `(progn
     (loop for i in ,arg-list unless (listp i) do
	  (error "argument ~a in fplay is malformed." i))
     (let* ((max-len (1- (apply #'max (mapcar #'length ,arg-list))))
	    ;; to render only some voices, enable these and replace max-len 
	    ;(from (or (second (assoc 'from ,arg-list)) 1))
	    ;(to (or (second (assoc 'to ,arg-list)) max-len))
	    (rthm (assoc 'rhythm ,arg-list))
	    (tim (assoc 'time ,arg-list))
	    (con (assoc 'condition ,arg-list))
	    (snd (assoc 'sound ,arg-list))
	    (print (assoc 'printing ,arg-list))
	    (total-times (max 2 (length rthm) (length tim)))
	    (all-vars '()))
       ;; add as many 'time, 'condition, and 'line variables as needed
       (merge-var-lists
	(list (loop for i from 0 below total-times collect
		   (if (= 0 i)
		       'time
		       (if tim
			   (nth (1- (min i (1- (length tim)))) (cdr tim))
			   ,start-time)))
	      (loop for i from 0 below total-times collect
		   (if (= 0 i)
		       'condition
		       (if con
			   (nth (1- (min i (1- (length con))))
				(cdr con))
			   `(<= ,(name-var 'time i)
				,,end-time))))
	      (loop for i from 0 below total-times collect
		   (if (= 0 i)
		       'line
		       `(/ (- ,(name-var 'time i) ,,start-time)
			   (- ,,end-time ,,start-time)))))
	all-vars)
       ;; get user-defined variables in the mix
       (merge-var-lists ,arg-list all-vars)
       ;; starting with some stubs for error messages, not sure yet if this is
       ;; usefull, since more often than not i will first run into an error and
       ;; then eval this.
       #+nil(merge-var-lists
	(list (loop for i from 0 below (max 2 (length snd)) collect
		   (if (= 0 i) 'errors `(unless ,(name-var 'sound i)
					  (error "~a is nil at i = ~a"
						 ',(name-var 'sound i)
						 i)))))
	all-vars)
       ;; add sufficient 'file variables
       (merge-var-lists
	(list (loop for i from 0 below (max 2 (length snd)) collect
		   (if (= 0 i) 'file `(path ,(name-var 'sound i)))))
	all-vars)
       ;; add all other variables that are not set through arg-list
       (merge-var-lists
	`((sfl nil) (sound nil) (rhythm (1+ ,,end-time))
	  (duration nil) (reflect nil) (reverse nil) (start 0) (end 0) (srt 1)
	  (width 5) (srt-env '(0 0 100 0)) (srt-scaler 1.0) (amp 1.0)
	  (amp-env '(0 1 100 1)) (degree 45) (distance 0) (rev-env '(0 1 100 1))
	  (rev-amt 0) (channel 0) (out-channels nil) (printing nil))
	all-vars)
       ;; get all neccessary variables:
       (append
	(get-loop-vars (reverse all-vars))
	;; while:
	(append '(while)
		(list (loop for i from 0 below (max 2 (length rthm)) collect
			   (if (= 0 i) 'or (name-var 'condition i)))))
	;; error cases:
	`(do ,@(loop for i in '(rhythm file) collect
		    `(unless ,i
		       (error "~&~a returned with nil in fplay" ',i)))
	     ,@(loop for i from 1 to max-len collect
		    `(when (and ,(name-var-highest 'duration i all-vars)
				(<= ,(name-var-highest 'duration i all-vars) 0.0001))
		       (setf ,(name-var-highest 'condition i all-vars) nil))))
	;; instrument calls:
	(loop for i from 1 to max-len append
	     (list 'when (name-var-highest 'condition i all-vars) 'collect
		   `(funcall (lambda ()
	        (samp0 ,(name-var-highest 'file i all-vars)
		       ,(name-var-highest 'time i all-vars)
		       :duration ,(name-var-highest 'duration i all-vars)
		       :reflect ,(name-var-highest 'reflect i all-vars)
		       :reverse ,(name-var-highest 'reverse i all-vars)
		       :start ,(name-var-highest 'start i all-vars)
		       :end ,(name-var-highest 'end i all-vars)
		       :srt ,(name-var-highest 'srt i all-vars)
		       :width ,(name-var-highest 'width i all-vars)
		       :srt-env ,(name-var-highest 'srt-env i all-vars)
		       :srt-scaler ,(name-var-highest 'srt-scaler i all-vars)
		       :amp ,(name-var-highest 'amp i all-vars)
		       :amp-env ,(name-var-highest 'amp-env i all-vars)
		       :degree ,(name-var-highest 'degree i all-vars)
		       :distance ,(name-var-highest 'distance i all-vars)
		       :rev-env ,(name-var-highest 'rev-env i all-vars)
		       :rev-amt ,(name-var-highest 'rev-amt i all-vars)
		       :channel ,(name-var-highest 'channel i all-vars)
		       :out-channels ,(name-var-highest 'out-channels i all-vars)
		       :printing nil
		       )))))
	;; printing:
	`(do 
	  ,@(loop for i from 1 to max-len 
	       when (nth (max 0 (1- (min i (1- (length print))))) (cdr print))
	       collect
		 `(when ,(name-var-highest 'condition i all-vars)
		    (format t "~&time: ~a"
			    ,(name-var-highest 'time i all-vars))
		    (when (equal (type-of ,(name-var-highest 'sound i all-vars))
				 'stored-file)
			   (format t "~&sound: ~a"
				   (id ,(name-var-highest 'sound i
							      all-vars))))
		    (format t "~&duration: ~a"
			    ,(name-var-highest 'duration i all-vars))
		    (format t "~&srate-conversion: ~a"
			    ,(name-var-highest 'srt i all-vars))
		    (format t "~&reverse: ~a"
			    ,(name-var-highest 'reverse i all-vars))
		    (format t "~&degree: ~a"
			    ,(name-var-highest 'degree i all-vars))))
	  ;; increasing the different times:
	  ,@(loop for i from 1 to (1- (length (assoc 'time all-vars)))
	       collect `(incf ,(name-var 'time i)
			      ,(name-var-highest 'rhythm i all-vars))))))))

;; *** fplay
;;; A very handy Macro to call samp0 instrument for use in clm
;;; start-time - the initial value for 'time (see below)
;;; end-time - when 'time is > end-time, the loop stops.
;;; REST: All following arguments should be of type list and will create lexical
;;;  variables. Like for #'let the first element should be the name for the
;;;  variable and the next argument is how it shall be defined. However the list
;;;  can be as long as you want. For every definition another variable is
;;;  created. For example: (rhythm 2 1) would create rhythm and define it as 2
;;;  and also rhythm2 and define it as 1. The variables can have any name, but
;;;  some names already have a meaning within fplay. fplay will create n calls
;;;  to samp0, where n is the maximum length of definitions for a variable. So
;;;  if you define 'rhythm as (rhythm 2 1 (nth (mod i 2) '(3 4))), at least
;;;  three different calls to samp0 are generated. If some variables have less
;;;  definitions than others, the last one will be used.
;;;  The following variables are always created internally by fplay and can also
;;;  be used within the variables the user defines. 'Time and 'condition can
;;;  also be altered.
;;;  time - this is given to samp0 as time argument and will be increased by
;;;   'rhythm at the end of each iteration. 'time usually starts at 'start-time.
;;;  condition - while this is t, the loop keeps going.
;;;   Usually is (<= time end-time)
;;;  line - this is basically defined as (/ passed-time entire-time), but
;;;   depends on start-time and end-time. I treat it as the 'x value for any
;;;   function that wants to know the relative time within fplay. If you
;;;   don't modify 'time this should always be within 0 and 1.
;;;  These variable-names have a predefined meaning within fplay but cannot be
;;;  used within the definition of user-defined variables unless they are
;;;  previously defined by the user:
;;;  file - this is given to samp0 as its file argument and should be a path
;;;   pointing to a soundfile. Its initial definition is (path sound)
;;;  sound - this is a variable designed to hold a stored-file (see :layers).
;;;   Initially defined as (first (data sfl)).
;;;  sfl - a stored-file-list (see :layers). This can be thought of as the
;;;   'sample-library' that is used. But can also be totally ignored.
;;;  rhythm - the time between this call to samp0 and the next one. 'Time
;;;   will be increased by 'rhythm at the end of each iteration. However
;;;   this is not neccessarily how long the sample will be played.
;;;   If this is not provided, the sample should only play once.
;;;  printing - when t, print sound, duration and time for isntrument call.
;;;  If you want to access one of samp0's key-argument you can do so by
;;;  creating a variable with the same name. For example:
;;;  duration - how long the sample will be played. This is passed to samp0 as
;;;   the :duration key-argument. Initially defined as nil - the sample will
;;;   be played entirely.
;;; out-channels - how many channels the soundfile will have. If not specified
;;;  this will be 2. For more than two channels locsig assumes them to be 
;;;  arranged in an equally spaced circle.
;;;
;;; For exemplary uses see https://github.com/Leon-Focker/feedback
(defmacro fplay (start-time end-time &body body)
  `(progn (unless (and (numberp ,start-time) (numberp ,end-time))
	    (error "start-time and end-time must be numbers in fplay: ~a, ~a"
		   ,start-time ,end-time))
	  (loop for i from 0 ,@(fplay-get-loop-vars start-time end-time body))))
