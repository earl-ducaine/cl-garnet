
(defstruct sl
  name
  value)

(defparameter *hash-table* (make-hash-table :test #'eq))

(defparameter *constraint-1* (make-sl :value :slot-1-box))
(defparameter *constraint-2* (make-sl :value :slot-2-box))
(defparameter *constraint-3* (make-sl :value :slot-3-box))
(defparameter *constraint-4* (make-sl :value :slot-4-box))

(defparameter *slot-1* (make-sl :name :a :value *constraint-1*))
(defparameter *slot-2* (make-sl :name :b :value *constraint-2*))
(defparameter *slot-3* (make-sl :name :c :value *constraint-3*))
(defparameter *slot-4* (make-sl :name :d :value *constraint-4*))

(setf (gethash :a *hash-table*) *slot-1*)
(setf (gethash :b *hash-table*) *slot-2*)
(setf (gethash :c *hash-table*) *slot-3*)
(setf (gethash :d *hash-table*) *slot-4*)

(setf (gethash :1 *hash-table*) t)
(setf (gethash :2 *hash-table*) t)
(setf (gethash :3 *hash-table*) t)
(setf (gethash :4 *hash-table*) t)
(setf (gethash :5 *hash-table*) t)
(setf (gethash :6 *hash-table*) t)

(defun create-error ()
  (locally
      (declare (optimize (safety 0)  (debug 3)))
    (maphash
     #'(lambda (iterate-ignored-slot-name slot-value-entry)
	 (declare (ignore iterate-ignored-slot-name))
	 (let ((slot (sl-value (sl-value
						 (gethash (sl-name slot-value-entry)
							  *hash-table*)))))
	   (setf (gethash :box *hash-table*) nil)
	   (setf (gethash slot *hash-table*) nil)))
     *hash-table*)))
