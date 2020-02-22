
(defstruct schema
  name
  bins)

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

(defparameter *graphical-object-hash-table*
  (make-hash-table :test #'eq))
(defvar *graphical-object*
  (make-schema :name :graphical-object :bins *graphical-object-hash-table*))

(defparameter *rectangle-hash-table* (make-hash-table :test #'eq))
(defvar *rectangle* (make-schema :name :rectangle :bins *rectangle-hash-table* ))

(setf (gethash :is-a *graphical-object-hash-table*)
      (make-sl :name :is-a :value nil))

(setf (gethash :filling-style *graphical-object-hash-table*)
      (make-sl :name :filling-style
	       :bits 33))

(setf (gethash :line-style *graphical-object-hash-table*)
      (make-sl :name :line-style
	       :bits 33))

(setf (gethash :is-a *rectangle-hash-table*)
      (make-sl :name :is-a
	       :value (list *graphical-object*)))

(setf (gethash :update-slots *rectangle-hash-table*)
      (make-sl :value '(:fast-redraw-p)))

(setf (gethash :initialize *graphical-object-hash-table*)
      (make-sl :name :initialize :bits 0))

(setf (gethash :fast-redraw-p *rectangle-hash-table*)
      (make-sl :name :fast-redraw-p
	       :value '(:no-value)
	       :bits 8192))

(maphash
 #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
     (declare (ignore iterate-ignored-slot-name))
     (let ((slot (sl-name iterate-slot-value-entry))
	   (bits (logand (sl-bits iterate-slot-value-entry) 1023)))
       (unless (zerop bits)
	 (setf (gethash slot *rectangle-hash-table*)
	       (make-sl :name slot
			:value '(:no-value)
			:bits bits)))))
 *graphical-object-hash-table*)

(defparameter *axis-rectangle-hash-table* (make-hash-table :test #'eq))
(defvar *axis-rectangle* (make-schema :name :axis-rectangle
				      :bins *axis-rectangle-hash-table*))

(defun create-error ()
  (setf (gethash :is-a *axis-rectangle-hash-table*)
	(make-sl :name :is-a :value (list *rectangle*)))

  (setf (gethash :is-a-inv *rectangle-hash-table*)
	(make-sl :name :is-a-inv))

  (dotimes (i 4)
    (let* ((symbol (gensym)))
      (setf (gethash symbol *axis-rectangle-hash-table*)
	    (make-sl
	     :name symbol
	     :value (make-sb-constraint
		     :other-slots
		     (list :mg-connection :unconnected
			   :mg-variable-paths
			   (list '(:box) (list (gensym)))))))))


  (maphash
   #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
       (declare (ignore iterate-ignored-slot-name))
       (let ((slot (sl-name iterate-slot-value-entry))
	     (bits (logand (sl-bits iterate-slot-value-entry)
			   1023)))
	 (unless (zerop bits)
	   (setf (gethash slot *axis-rectangle-hash-table*)
		 (make-sl :name slot
			  :value '(:no-value)
			  :bits bits)))))
   *rectangle-hash-table*)

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
			collect (loop for (slot next-slot) on path
				   do (return
					(cons
					 (car
					  (getf
					   (sb-constraint-other-slots cn)
					   :mg-os nil))
					 slot))))))
	       (loop for var-os in constraint-slot
		  collect (let ((obj (car var-os))
				(slot (cdr var-os)))
			    (setf (gethash slot *axis-rectangle-hash-table*)
				  (make-sl :name slot :bits 0))
			    (make-sl))))
	     (sb-constraint-set-slot-fn cn)
	     (setf (getf (sb-constraint-other-slots cn)
			 :mg-connection nil)
		   :connected))))
     *axis-rectangle-hash-table*)))
