(defun lindenmayer (recursions seed rules ratios &key (warning t))
  (let* ((result seed)   ; recursive list containing the elements
	 (final 1))      ; recursive list containing the ratios
    (labels ((get-lists (res-ls fin-ls) ; define recursive function
	       (if (atom (car res-ls))  ; check if list is still nested
		   ;; if the current res-ls is flat, return two values:
		   (values		    
		    ;; firstly generate the result list,
		    ;; replacing each element according to given rules
		    (loop for element in res-ls collect
			 (caadr (assoc element rules)))
		    ;; secondly generate the final list, by replacing
		    ;; all elements with ratios and scaling them recursively
		    (progn ; needed, since loop itself doesn't return res-ls
		      ;; loop through all elements and check, if they match
		      ;; an element from the replace-list
		      (loop for rep in ratios do
			   (setf res-ls 
				 (loop for element in res-ls collect
				      (if (eq (car rep)
					      element) 
					  (* (cadr rep)
					     fin-ls) 
					  element))))
		      res-ls)) ; by this point it should act. be called final
		   ;; if the list is still nested, loop through al elements:
		   ;; since we're working with a multiple-value bind,
		   ;; this is a bit of a weird loop.
		   ;; first we'll need to helping variables, since loop itself
		   ;; can't return multiple values (if yes, teach me)
		   (let (help1 help2)
		     ;; now loop through the nested res-list
		     (loop for l in res-ls
			;; and through the fin-ls, if it's still nested
			for i from 0
			for b = (if (atom fin-ls) fin-ls (nth i fin-ls))
			;; now we bind the two values that get-lists will return
			do (multiple-value-bind (r f) (get-lists l b)
			     ;; save those values into the helping variables
			     (progn (push r help1)
				    (push f help2))))
		     ;; and finally return the two values
		     (values (reverse help1) (reverse help2))))))
      ;; loop for how many recursions you like (not more than 20 please)
      (loop repeat recursions do	
	   (setf (values result final) ; bind the two returned values
		 (get-lists result final))))
    ;; output
    ;;(format t "~&next result: ~a" result)
    ;;(format t "~&final: ~a" final)
    final))

;;; not using multiple value binds:
#|(defun lindenmayer1 (recursions seed rules replacements)
  ;; result for the recursive list of associations and
  ;; final for the recursive list of ratios
  (let* ((result seed)
	 (helper '())
	 (final 1))
    ;; recursive function to get the result list
    (labels ((get-results (ls)
	       (setf result
		     ;; if it's still nested, unpack again
		     (if (atom (car ls))
			 ;; replace elements according to rule-set
			 (loop for element in ls collect
			      (caadr (assoc element rules)))
			 ;; loop through list = unpack
			 (loop for l in ls collect (get-results l)))))
	     ;; recursive function that takes result list and
	     ;; makes it a list of ratios
	     (get-final (res-ls buffer)
	       (setf final
		     ;; if it's still nested, unpack again
		     (if (atom (car res-ls))
			 (progn
			   (loop for rep in replacements do
				(setf res-ls 
				      (loop for element in res-ls
					 collect 
					   (if (eq (car rep)
						   element) 
					       (* (cadr rep)
						  buffer) 
					       element))))
			   res-ls)
			 ;; unpack the result list fully
			 ;; and get current scaler (= unpacked buffer)
			 ;; (buffer = last final list)
			 (loop for l in res-ls
			    for i from 0
			    for b = (if (atom buffer) buffer (nth i buffer))
			    collect (get-final l b))))))
      ;; call both helper functions
      ;; first we need a final list, then we can proceed to the next result list
      (loop repeat recursions do	
	   (get-final result final)		
	   (get-results result)))
    ;; output
    ;(format t "~&next result: ~a" result)
    ;;(format t "~&final: ~a" final)
    final
    nil))|#


;;; the ugly mess we started with:

#|(defun lindenmayer (recursions seed rules replacements)
  ;; result for the recursive list of associations and
  ;; final for the recursive list of ratios
  (let* ((result seed)
	 (final 1))
    ;; recursive function to get the result list
    (labels ((get-results (ls)
	       (setf result
		     ;; if it's still nested, unpack again
		     (if (atom (car ls))
			 ;; replace elements according to rule-set
			 (loop for element in ls collect
			      (caadr (assoc element rules)))
			 ;; loop through list = unpack
			 (loop for l in ls collect (get-results l)))))
	     ;; recursive function that takes result list and
	     ;; makes it a list of ratios
	     (get-final (res-ls buffer)
	       (setf final
		     ;; if it's still nested, unpack again
		     (if (atom (car res-ls))
			 ;; magical creature
			 (setf res-ls

			       ;; do we really need this recursive thing
			       ;; again? feels weird :c
			       
			       (labels
				   ((helper-helper (lsss)
				      (if (atom (car lsss))
					  (progn
					    (loop for rep in replacements do
						 (setf lsss 
						       (loop for element in lsss
							  collect 
							    (if (eq (car rep)
								    element) 
								(* (cadr rep)
								   buffer) 
								element))))
					    lsss)
					  (loop for l in lsss
					     collect (helper-helper l)))))
				 (helper-helper res-ls)))
			 
			 ;; unpack the result list fully
			 ;; and get current scaler (= unpacked buffer)
			 ;; (buffer = last final list)
			 (loop for l in res-ls
			    for i from 0
			    for b = (if (atom buffer) buffer (nth i buffer))
			    collect (get-final l b))))))
      ;; call both helper functions
      ;; first we need a final list, then we can proceed to the next result list
      (loop repeat recursions do	
	   (get-final result final)		
	   (get-results result)))
    ;; output
    (format t "~&next result: ~a" result)
    ;;(format t "~&final: ~a" final)
    final))|#
