
(defstruct schema
  name)

(defstruct sl
  name
  value
  (bits 0 :type fixnum))

(defstruct sb-constraint
  variables
  other-slots
  set-slot-fn)

(defparameter iterate-slot-value-entry nil
  "Ugly")

;; Hash tables
(defparameter *graphical-object-hash-table* (make-hash-table :test #'eq))
(defparameter *rectangle-hash-table* (make-hash-table :test #'eq))
(defparameter *axis-rectangle-hash-table* (make-hash-table :test #'eq))

;; Schemas
(defparameter *graphical-object* (make-schema :name :graphical-object))
(defparameter *rectangle* (make-schema :name :rectangle ))
(defparameter *axis-rectangle* (make-schema :name :axis-rectangle))

;; Connections

(defparameter *axis-rectangle-hash-table-cn-1*
  (make-sb-constraint :other-slots '(:mg-connection :unconnected
				     :mg-variable-paths
				     ((:box) (:axis-rectangle-slot-1-box)))))

(defparameter *axis-rectangle-hash-table-cn-2*
  (make-sb-constraint :other-slots '(:mg-connection :unconnected
				     :mg-variable-paths
				     ((:box) (:axis-rectangle-slot-2-box)))))

(defparameter *axis-rectangle-hash-table-cn-3*
  (make-sb-constraint :other-slots '(:mg-connection :unconnected
				     :mg-variable-paths
				     ((:box) (:axis-rectangle-slot-3-box)))))

(defparameter *axis-rectangle-hash-table-cn-4*
  (make-sb-constraint :other-slots '(:mg-connection :unconnected
				     :mg-variable-paths
				     ((:box) (:axis-rectangle-slot-4-box)))))

;; Slots axis rectangle

(defparameter *axis-rectangle-is-a-sl*
  (make-sl :name :is-a :bits 0))

(defparameter *axis-rectangle-slot-1*
  (make-sl :name :axis-rectangle-slot-1
		 :value *axis-rectangle-hash-table-cn-1*))

(defparameter *axis-rectangle-slot-2*
  (make-sl :name :axis-rectangle-slot-2
	       :value *axis-rectangle-hash-table-cn-2*))

(defparameter *axis-rectangle-slot-3*
  (make-sl :name :axis-rectangle-slot-3
	       :value *axis-rectangle-hash-table-cn-3*))

(defparameter *axis-rectangle-slot-4*
  (make-sl :name :axis-rectangle-slot-4
	   :value *axis-rectangle-hash-table-cn-4*))

(defparameter *axis-rectangle-fast-redraw-p-sl*
  (make-sl :name :fast-redraw-p :bits 0))

(defparameter *axis-rectangle-filling-style-sl*
  (make-sl :name :filling-style :bits 0))

(defparameter *axis-rectangle-line-style-sl*
  (make-sl :name :line-style :bits 0))

(defparameter *axis-rectangle-is-a-inv-sl*
  (make-sl :name :is-a-inv :bits 0))

(defparameter *axis-rectangle-nil-sl*
  (make-sl :name nil :bits 0))



(setf (gethash :axis-rectangle-slot-1 *axis-rectangle-hash-table*)
	*axis-rectangle-slot-1*)

(setf (gethash :axis-rectangle-slot-2 *axis-rectangle-hash-table*)
      *axis-rectangle-slot-2*)

(setf (gethash :axis-rectangle-slot-3 *axis-rectangle-hash-table*)
      *axis-rectangle-slot-3*)

(setf (gethash :axis-rectangle-slot-4 *axis-rectangle-hash-table*)
      *axis-rectangle-slot-4*)

(setf (gethash :is-a-inv *axis-rectangle-hash-table*)
      *axis-rectangle-is-a-inv-sl*)

(setf (gethash nil *axis-rectangle-hash-table*)
      *axis-rectangle-nil-sl*)

(setf (gethash :is-a *axis-rectangle-hash-table*)
      *axis-rectangle-is-a-sl*)

(setf (gethash :fast-redraw-p *axis-rectangle-hash-table*)
      *axis-rectangle-fast-redraw-p-sl*)

(setf (gethash :filling-style *axis-rectangle-hash-table*)
      *axis-rectangle-filling-style-sl*)

(setf (gethash :line-style *axis-rectangle-hash-table*)
      *axis-rectangle-line-style-sl*)



(defun create-error ()
  (locally
      (declare (optimize (safety 0)  (debug 3)))
    (maphash
     #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	 (declare (ignore iterate-ignored-slot-name))
	 (let* ((cn
		 (sl-value
		  (gethash (sl-name iterate-slot-value-entry)
			   *axis-rectangle-hash-table*))))
	   (when (sb-constraint-p cn)
	     (let ((slots (getf (sb-constraint-other-slots cn) :mg-variable-paths)))
	     (setf (gethash (car slots) *axis-rectangle-hash-table*) nil)
	     (setf (gethash (cadr slots) *axis-rectangle-hash-table*) nil))
	   (setf (getf (sb-constraint-other-slots cn)
		       :mg-connection nil)
		 :connected))))
    *axis-rectangle-hash-table*)))
