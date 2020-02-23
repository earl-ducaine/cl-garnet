
(defstruct schema
  name)

(defstruct sl
  name
  value)

(defstruct sb-constraint
  variables
  other-slots
  set-slot-fn)

(defparameter *hash-table* (make-hash-table :test #'eq))

(defparameter *constraint-1*
  (make-sb-constraint :other-slots nil))

(defparameter *constraint-2*
  (make-sb-constraint
   :other-slots '(:mg-connection :unconnected
		  :mg-variable-paths ((:box) (:slot-2-box)))))

(defparameter *constraint-3*
  (make-sb-constraint
   :other-slots '(:mg-connection :unconnected
		  :mg-variable-paths ((:box) (:slot-3-box)))))

(defparameter *constraint-4*
  (make-sb-constraint
   :other-slots '(:mg-connection :unconnected
		  :mg-variable-paths ((:box) (:slot-4-box)))))


(defparameter *slot-1* (make-sl :name :slot-1 :value *constraint-1*))
(defparameter *slot-2* (make-sl :name :slot-2 :value *constraint-2*))
(defparameter *slot-3* (make-sl :name :slot-3 :value *constraint-3*))
(defparameter *slot-4* (make-sl :name :slot-4 :value *constraint-4* ))


(setf (gethash :slot-1 *hash-table*) *slot-1*)
(setf (gethash :slot-2 *hash-table*) *slot-2*)
(setf (gethash :slot-3 *hash-table*) *slot-3*)
(setf (gethash :slot-4 *hash-table*) *slot-4*)

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
     #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	 (declare (ignore iterate-ignored-slot-name))
	 (let* ((cn  (sl-value
		      (gethash (sl-name iterate-slot-value-entry)
			       *hash-table*))))
	   (format t "cn: ~s~%iterate-slot-value-entry: ~s~%"
		   cn iterate-slot-value-entry)
	     (let ((slots (getf (sb-constraint-other-slots cn) :mg-variable-paths)))
	     (setf (gethash (car slots) *hash-table*) nil)
	     (setf (gethash (cadr slots) *hash-table*) nil)
	   (setf (getf (sb-constraint-other-slots cn)
		       :mg-connection nil)
		 :connected))))
    *hash-table*)))
