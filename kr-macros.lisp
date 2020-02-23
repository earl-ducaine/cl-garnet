
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

(defparameter *graphical-object-hash-table* (make-hash-table :test #'eq))
(defparameter *rectangle-hash-table* (make-hash-table :test #'eq))
(defparameter *axis-rectangle-hash-table* (make-hash-table :test #'eq))

(defparameter *graphical-object*
  (make-schema :name :graphical-object))

(defparameter *rectangle* (make-schema :name :rectangle ))
(defparameter *axis-rectangle* (make-schema :name :axis-rectangle))

(defparameter *axis-rectangle-is-a-sl*
  (make-sl :name :is-a :value (list *rectangle*)))

(defparameter *axis-rectangle-is-a-inv-sl*
  (make-sl :name :is-a-inv))

(setf (gethash :is-a *graphical-object-hash-table*)
      (make-sl :name :is-a :value nil))

(setf (gethash :filling-style *graphical-object-hash-table*)
      (make-sl :name :filling-style
	       :bits 33))

(setf (gethash :line-style *graphical-object-hash-table*)
      (make-sl :name :line-style
	       :bits 33))

(setf (gethash :initialize *graphical-object-hash-table*)
      (make-sl :name :initialize :bits 0))


(setf (gethash :is-a *rectangle-hash-table*)
      (make-sl :name :is-a
	       :value (list *graphical-object*)))

(setf (gethash :update-slots *rectangle-hash-table*)
      (make-sl :value '(:fast-redraw-p)))


(setf (gethash :fast-redraw-p *rectangle-hash-table*)
      (make-sl :name :fast-redraw-p
	       :value '(:no-value)
	       :bits 8192))

(setf (gethash :FILLING-STYLE *rectangle-hash-table*)
	       (make-sl :name :FILLING-STYLE
			:bits 33))

(setf (gethash :LINE-STYLE *rectangle-hash-table*)
	       (make-sl :name :LINE-STYLE
			:bits 33))

(setf (gethash :is-a-inv *rectangle-hash-table*)
      *axis-rectangle-is-a-inv-sl*)



(setf (gethash :is-a *axis-rectangle-hash-table*)
      *axis-rectangle-is-a-sl*)


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

(defun create-error ()
  (let* ((symbol :axis-rectangle-slot-1))
    (setf (gethash symbol *axis-rectangle-hash-table*)
	  (make-sl
	   :name symbol
	   :value *axis-rectangle-hash-table-cn-1*)))

  (let* ((symbol :axis-rectangle-slot-2))
    (setf (gethash symbol *axis-rectangle-hash-table*)
	  (make-sl
	   :name symbol
	   :value *axis-rectangle-hash-table-cn-2*)))

  (let* ((symbol :axis-rectangle-slot-3))
    (setf (gethash symbol *axis-rectangle-hash-table*)
	  (make-sl
	   :name symbol
	   :value *axis-rectangle-hash-table-cn-3*)))

  (let* ((symbol :axis-rectangle-slot-4))
    (setf (gethash symbol *axis-rectangle-hash-table*)
	  (make-sl
	   :name symbol
	   :value *axis-rectangle-hash-table-cn-4*)))

  (setf (gethash :filling-style *axis-rectangle-hash-table*)
	(make-sl :name :filling-style))

  (setf (gethash :line-style *axis-rectangle-hash-table*)
	(make-sl :name :line-style))

  (maphash
   #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
       (declare (ignore iterate-ignored-slot-name))
       (let ((slot (sl-name iterate-slot-value-entry)))
	 (setf (gethash slot *axis-rectangle-hash-table*)
	       (make-sl :name slot :bits 0))))
   *rectangle-hash-table*)

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
