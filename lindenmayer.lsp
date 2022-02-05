(defun compartmentalise (total-length seed rules ratios)
  (let* ((result seed)	      ; recursive list containing the elements
	 (final 1)	      ; recursive list containing the ratios
	 (collector '(1)))    ; collect all results in this list
    (labels ((get-lists (res-ls fin-ls) ; define recursive function
	       (if (atom (car res-ls)) ; check if list is still nested
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
					  (cadr rep)
					  element))))
		      (let ((sum (loop for i in res-ls sum i)))
			(setf res-ls
			      (loop for element in res-ls
				 collect (* element (/ fin-ls sum)))))
		      res-ls)) ; by this point it should act. be called final
		   ;; if the list is still nested, loop through al elements:
		   ;; since we're working with a multiple-value bind,
		   ;; this is a bit of a weird loop.
		   ;; first we'll need two helping variables, since loop itself
		   ;; can't return multiple values (if it can, teach me)
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
      (loop until break
	 for i from 0
	 with break do	
	   (setf (values result final)	; bind the two returned values
		 (get-lists result final))
	   (push (scale-structure final total-length) collector)
	   (when (or (< (apply #'min (car collector))
			*max-smallest-sample-length*)
		     ;; we never want more than 20 recursions, though we will
		     ;; probably never even go beyond 10
		     (> i 20))
	     (setf break t))))
    ;; output
    ;;(format t "~&next result: ~a" result)
    ;;(format t "~&final: ~a" final)
    collector))
