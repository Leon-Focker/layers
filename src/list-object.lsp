;; ** list-object
;;;; basic list object

(in-package :layers)

;; *** list-object
;;; just a very basic list object with basic methods
(defclass list-object (base-object)
  ((current :accessor current :initarg :current :type integer :initform 0)))

;; *** get-ids
;;; get the ids of all elements in the list
(defmethod get-ids ((lo list-object))
  (loop for element in (data lo) collect (id element)))

;; *** get-current
;;; get current element of list
(defmethod see-current ((lo list-object))
  (when (data lo)
    (nth (current lo) (data lo))))

;; *** get-next
;;; proceed to next element of the list
(defmethod get-next ((lo list-object))
  (when (data lo)
    (let* ((data (data lo)))
      (progn
	(incf (current lo))
	(when (= (current lo) (length data))
	  (setf (current lo) 0))
	(nth (current lo) data)))))

;; *** see-next
;;; get next element in a list but don't change the current-slot
(defmethod see-next ((lo list-object))
  (when (data lo)
    (let* ((data (data lo))
	   (n (current lo)))
      (progn (incf n)
	     (when (= n (length (data lo)))
	       (setf n 0))
	     (nth n data)))))

;; *** see-with-relative-index
;;; get an element from the list with index (+ current relative)
;;; but don't change the current-slot
(defmethod see-with-relative-index ((lo list-object) relative-index)
  (when (data lo)
    (let* ((data (data lo))
	   (n (current lo)))
      (progn (incf n relative-index)
	     (setf n (mod n (length (data lo))))
	     (nth n data)))))

;;;; EOF list-object.lsp
