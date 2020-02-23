
(defstruct schema
  name)

(defstruct sl
  name
  value)

(defstruct sb-constraint
  variables
  other-slots
  set-slot-fn)

;; Hash tables
(defparameter *hash-table* (make-hash-table :test #'eq))


;; Connections
(defparameter *constraint-1*
  (make-sb-constraint :other-slots '(:mg-connection :unconnected
				     :mg-variable-paths
				     ((:box) (:axis-rectangle-slot-1-box)))))

(defparameter *constraint-2*
  (make-sb-constraint :other-slots '(:mg-connection :unconnected
				     :mg-variable-paths
				     ((:box) (:axis-rectangle-slot-2-box)))))

(defparameter *constraint-3*
  (make-sb-constraint :other-slots '(:mg-connection :unconnected
				     :mg-variable-paths
				     ((:box) (:axis-rectangle-slot-3-box)))))

(defparameter *constraint-4*
  (make-sb-constraint :other-slots '(:mg-connection :unconnected
				     :mg-variable-paths
				     ((:box) (:axis-rectangle-slot-4-box)))))

;; Slots axis rectangle
(defparameter *axis-rectangle-is-a-sl*
  (make-sl :name :is-a))

(defparameter *axis-rectangle-slot-1*
  (make-sl :name :axis-rectangle-slot-1
		 :value *constraint-1*))

(defparameter *axis-rectangle-slot-2*
  (make-sl :name :axis-rectangle-slot-2
	       :value *constraint-2*))

(defparameter *axis-rectangle-slot-3*
  (make-sl :name :axis-rectangle-slot-3
	       :value *constraint-3*))

(defparameter *axis-rectangle-slot-4*
  (make-sl :name :axis-rectangle-slot-4
	   :value *constraint-4* ))




(setf (gethash :axis-rectangle-slot-1 *hash-table*)
	*axis-rectangle-slot-1*)

(setf (gethash :axis-rectangle-slot-2 *hash-table*)
      *axis-rectangle-slot-2*)

(setf (gethash :axis-rectangle-slot-3 *hash-table*)
      *axis-rectangle-slot-3*)

(setf (gethash :axis-rectangle-slot-4 *hash-table*)
      *axis-rectangle-slot-4*)

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
	   (format t "cn: ~s~%iterate-slot-value-entry: ~s~%" cn iterate-slot-value-entry)
	   (when (sb-constraint-p cn)
	     (let ((slots (getf (sb-constraint-other-slots cn) :mg-variable-paths)))
	     (setf (gethash (car slots) *hash-table*) nil)
	     (setf (gethash (cadr slots) *hash-table*) nil))
	   (setf (getf (sb-constraint-other-slots cn)
		       :mg-connection nil)
		 :connected))))
    *hash-table*)))
