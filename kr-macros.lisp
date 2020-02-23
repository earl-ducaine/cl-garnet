
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

;; Slots -- grahical object

(defparameter *graphical-object-is-a-sl*
  (make-sl :name :is-a :value nil))

(defparameter *graphical-object-filling-style-sl*
  (make-sl :name :filling-style :bits 33))

(defparameter *graphical-object-line-style-sl*
  (make-sl :name :line-style :bits 33))

(defparameter *graphical-object-initialize-sl*
  (make-sl :name :initialize :bits 0))

;; Slots rectangle

(defparameter *rectangle-is-a-sl*
  (make-sl :name :is-a :value (list *graphical-object*)))

(defparameter *rectangle-update-slots-sl*
  (make-sl :value '(:fast-redraw-p)))

(defparameter *rectangle-is-a-sl*
  (make-sl :name :is-a :value (list *graphical-object*)))

(defparameter *rectangle-fast-redraw-p-sl*
  (make-sl :name :fast-redraw-p :value '(:no-value) :bits 8192))

(defparameter *rectangle-filling-style-sl*
  (make-sl :name :filling-style :bits 33))

(defparameter *rectangle-line-style-sl*
  (make-sl :name :line-style :bits 33))

(defparameter *rectangle-is-a-inv-sl*
  (make-sl :name :line-style :bits 33))

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






(setf (gethash :is-a-inv *rectangle-hash-table*)
      *axis-rectangle-is-a-inv-sl*)

(setf (gethash nil *rectangle-hash-table*)
      *axis-rectangle-nil-sl*)










(setf (gethash :axis-rectangle-slot-1 *axis-rectangle-hash-table*)
	*axis-rectangle-slot-1*)

(setf (gethash :axis-rectangle-slot-2 *axis-rectangle-hash-table*)
      *axis-rectangle-slot-2*)

(setf (gethash :axis-rectangle-slot-3 *axis-rectangle-hash-table*)
      *axis-rectangle-slot-3*)

(setf (gethash :axis-rectangle-slot-4 *axis-rectangle-hash-table*)
      *axis-rectangle-slot-4*)



(setf (gethash :is-a *graphical-object-hash-table*)
      *graphical-object-is-a-sl*)

(setf (gethash :filling-style *graphical-object-hash-table*)
      *graphical-object-filling-style-sl*)

(setf (gethash :line-style *graphical-object-hash-table*)
      *graphical-object-line-style-sl*)

(setf (gethash :initialize *graphical-object-hash-table*)
      *graphical-object-initialize-sl*)

(setf (gethash :is-a *rectangle-hash-table*)
      *rectangle-is-a-sl*)

(setf (gethash :update-slots *rectangle-hash-table*)
      *rectangle-update-slots-sl*)

(setf (gethash :fast-redraw-p *rectangle-hash-table*)
      *rectangle-fast-redraw-p-sl*)

(setf (gethash :filling-style *rectangle-hash-table*)
      *rectangle-filling-style-sl*)

(setf (gethash :line-style *rectangle-hash-table*)
      *rectangle-line-style-sl*)


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
	   (when (and (sb-constraint-p cn)
		      (getf (sb-constraint-other-slots cn)
			    :mg-connection nil))
	     (setf (getf (sb-constraint-other-slots cn) :mg-os nil)
		   (cons *axis-rectangle* (sl-name iterate-slot-value-entry)))
	     (let* ((constraint-slot
		     (loop for path in (getf (sb-constraint-other-slots cn)
					     :mg-variable-paths nil)
			collect (cons *axis-rectangle* (car path)))))
	       (loop for var-os in constraint-slot
		  collect  (setf (gethash (cdr var-os) *axis-rectangle-hash-table*) nil)))
	     (sb-constraint-set-slot-fn cn)
	     (setf (getf (sb-constraint-other-slots cn)
			 :mg-connection nil)
		   :connected))))
     *axis-rectangle-hash-table*)))
