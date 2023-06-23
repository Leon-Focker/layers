;; ** base-object
;;;; nothing thrilling going on here

(in-package :layers)

;; *** base-object
;;; as simple as it gets
(defclass base-object ()
  ((id :accessor id :initarg :id :initform nil)
   (data :accessor data :initarg :data :initform nil)))

;; *** get-id
;;; get the id of an object
(defmethod get-id ((bo base-object))
  (id bo))

;;;; EOF base-object.lsp
