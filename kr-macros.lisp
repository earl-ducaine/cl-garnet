
(defstruct s
  name
  value)

(defparameter *hash-table* (make-hash-table :test #'eq))

(defparameter *struct-1* (make-s :value :slot-1-box))
(defparameter *struct-2* (make-s :value :slot-2-box))
(defparameter *struct-3* (make-s :value :slot-3-box))
(defparameter *struct-4* (make-s :value :slot-4-box))

(defparameter *struct-a* (make-s :name :a :value *struct-1*))
(defparameter *struct-b* (make-s :name :b :value *struct-2*))
(defparameter *struct-c* (make-s :name :c :value *struct-3*))
(defparameter *struct-d* (make-s :name :d :value *struct-4*))

(setf (gethash :a *hash-table*) *struct-a*)
(setf (gethash :b *hash-table*) *struct-b*)
(setf (gethash :c *hash-table*) *struct-c*)
(setf (gethash :d *hash-table*) *struct-d*)

(setf (gethash :1 *hash-table*) (make-s))
(setf (gethash :2 *hash-table*) (make-s))
(setf (gethash :3 *hash-table*) (make-s))
(setf (gethash :4 *hash-table*) (make-s))
(setf (gethash :5 *hash-table*) (make-s))
(setf (gethash :6 *hash-table*) (make-s))

(defun create-error ()
  (locally
      (declare (optimize (safety 0)  (debug 3)))
    (maphash
     #'(lambda (iterate-ignored-slot-name slot-value-entry)
	 (declare (ignore iterate-ignored-slot-name))
	 (let ((slot (s-value (s-value
			       (gethash (s-name slot-value-entry)
					*hash-table*)))))
	   (setf (gethash :box *hash-table*) nil)
	   (setf (gethash slot *hash-table*) nil)))
     *hash-table*)))
