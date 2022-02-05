(defun structure-to-midi (structure n &optional (length 100))
  (let ((events '()))
    (loop for ls in (reverse (cdr (reverse (data structure)))) and k from 0 do
	 (let* (;;(ls (nth n (data structure)))
		(ls1 (loop for i in ls collect (round (/ 4 i))))
		(rhythms (substitute 0.1 0 ls1))
		(notes (mapcar #'midi-to-note
			       (loop for i in ls
				  for x = 0 then (mod (+ x 1) 2)
				  collect (+ x 60 (* 3 k))))))
	   (loop for r in rhythms and note in notes do
		(push (make-event notes r) events))))
    (events-update-time events)
    (event-list-to-midi-file events :start-tempo 60 :midi-file "/E/structure.mid")))

(defun concatenate-objects-to-symbol (&rest objects)
  (intern (apply #'concatenate 'string (mapcar #'princ-to-string objects))))

;; this is shit
(defun structure-to-midi (structure &optional (tempo 60))
  (let* ((ls (reverse (cdr (reverse (data structure)))))
	 (mini
          (make-slippery-chicken
           '+mini+
           :title "my mini template"
           :ensemble `(,(loop for i from 1 to (length ls)
			   collect (list (concatenate-objects-to-symbol 'vc i)
					 (list 'cello ':midi-channel 1))))
	   :set-palette `((1 (,(loop for i below (length ls)
				  collect (midi-to-note (+ 60 (* 2 i)))))))
	   :tempo-map `((1 ,tempo))
	   :set-map '((1 (1)))
	   :rthm-seq-palette
	   (loop for i from 1 to (length ls) collect
		(list i (list `(,(append '(4 4)
				       (loop for i in (nth i ls) collect (round (/ 4 i)))))
			      ':pitch-seq-palette (list (list i)))))
	   :rthm-seq-map `((1 ,(loop for i from 1 to (length ls)
    				  collect (list (concatenate-objects-to-symbol 'vc i) (list i))))))))
    #|(map-over-events mini 1 1 (loop for i from 1 to (length ls)
				 collect (concatenate-objects-to-symbol 'vc i))
		     #'(lambda (event) (when (needs-new-note event)
					 (setf (pitch-or-chord event) (first scale))
					 (setf scale (cdr scale)))))|#
    (midi-play mini
	       :midi-file
	       (concatenate 'string
			    "/E/structure.mid")
	       :update-amplitudes nil)))
