
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
(defparameter *axis-rectangle-hash-table* (make-hash-table :test #'eq))


;; Connections
(defparameter *axis-rectangle-cn-1*
  (make-sb-constraint :other-slots '(:mg-connection :unconnected
				     :mg-variable-paths
				     ((:box) (:axis-rectangle-slot-1-box)))))

(defparameter *axis-rectangle-cn-2*
  (make-sb-constraint :other-slots '(:mg-connection :unconnected
				     :mg-variable-paths
				     ((:box) (:axis-rectangle-slot-2-box)))))

(defparameter *axis-rectangle-cn-3*
  (make-sb-constraint :other-slots '(:mg-connection :unconnected
				     :mg-variable-paths
				     ((:box) (:axis-rectangle-slot-3-box)))))

(defparameter *axis-rectangle-cn-4*
  (make-sb-constraint :other-slots '(:mg-connection :unconnected
				     :mg-variable-paths
				     ((:box) (:axis-rectangle-slot-4-box)))))

;; Slots axis rectangle
(defparameter *axis-rectangle-is-a-sl*
  (make-sl :name :is-a))

(defparameter *axis-rectangle-slot-1*
  (make-sl :name :axis-rectangle-slot-1
		 :value *axis-rectangle-cn-1*))

(defparameter *axis-rectangle-slot-2*
  (make-sl :name :axis-rectangle-slot-2
	       :value *axis-rectangle-cn-2*))

(defparameter *axis-rectangle-slot-3*
  (make-sl :name :axis-rectangle-slot-3
	       :value *axis-rectangle-cn-3*))

(defparameter *axis-rectangle-slot-4*
  (make-sl :name :axis-rectangle-slot-4
	   :value *axis-rectangle-cn-4* ))




(setf (gethash :axis-rectangle-slot-1 *axis-rectangle-hash-table*)
	*axis-rectangle-slot-1*)

(setf (gethash :axis-rectangle-slot-2 *axis-rectangle-hash-table*)
      *axis-rectangle-slot-2*)

(setf (gethash :axis-rectangle-slot-3 *axis-rectangle-hash-table*)
      *axis-rectangle-slot-3*)

(setf (gethash :axis-rectangle-slot-4 *axis-rectangle-hash-table*)
      *axis-rectangle-slot-4*)

(setf (gethash :is-a-inv *axis-rectangle-hash-table*) t)

(setf (gethash nil *axis-rectangle-hash-table*) t)

(setf (gethash :is-a *axis-rectangle-hash-table*) t)

(setf (gethash :fast-redraw-p *axis-rectangle-hash-table*) t)

(setf (gethash :filling-style *axis-rectangle-hash-table*) t)

(setf (gethash :line-style *axis-rectangle-hash-table*) t)



(defun create-error ()
  (locally
      (declare (optimize (safety 0)  (debug 3)))
    (maphash
     #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	 (declare (ignore iterate-ignored-slot-name))
	 (let* ((cn  (sl-value
		      (gethash (sl-name iterate-slot-value-entry)
			       *axis-rectangle-hash-table*))))
	   (format t "cn: ~s~%iterate-slot-value-entry: ~s~%" cn iterate-slot-value-entry)
	   (when (sb-constraint-p cn)
	     (let ((slots (getf (sb-constraint-other-slots cn) :mg-variable-paths)))
	     (setf (gethash (car slots) *axis-rectangle-hash-table*) nil)
	     (setf (gethash (cadr slots) *axis-rectangle-hash-table*) nil))
	   (setf (getf (sb-constraint-other-slots cn)
		       :mg-connection nil)
		 :connected))))
    *axis-rectangle-hash-table*)))
