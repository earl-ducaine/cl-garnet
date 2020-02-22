
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *special-kr-optimization*
    '(optimize
      (speed 3)
      (safety 0)
      (space 0)
      (debug 0))))

(defstruct schema
  name
  bins)

(defstruct sl
  name
  value
  (bits 0 :type fixnum))

(defparameter *no-value* '(:no-value))

(declaim (fixnum *schema-counter*))
(defvar *schema-counter* 0)

(defparameter iterate-slot-value-entry nil
  "Ugly")

(defun kr-init-method-hook (schema)
  (maphash
   #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
       (declare (ignore iterate-ignored-slot-name))
       (let ((slot (sl-name iterate-slot-value-entry)))
	 (setf (gethash slot (schema-bins schema))
	       (make-sl :name slot :bits 0))))
   (schema-bins (car (sl-value (gethash :is-a (schema-bins schema))))))
  (locally
      (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
    (maphash
     #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	 (declare (ignore iterate-ignored-slot-name))
	 (let* ((cn
		 (sl-value
		  (gethash (sl-name iterate-slot-value-entry)
			   (schema-bins schema)))))
	   (when (and (sb-constraint-p cn)
		      (getf (sb-constraint-other-slots cn)
			    :mg-connection nil))
	     (setf (getf (sb-constraint-other-slots cn) :mg-os nil)
		   (cons schema (sl-name iterate-slot-value-entry)))
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
	       (let ((new1
		      (loop for var-os in constraint-slot
			 collect (let ((obj (car var-os))
				       (slot (cdr var-os)))
				   (setf (gethash slot (schema-bins obj))
					 (make-sl :name slot :bits 0))
				   (make-sl)))))
		 (setf (sb-constraint-variables cn) new)))
	     (sb-constraint-set-slot-fn cn)
	     (setf (getf (sb-constraint-other-slots cn)
			 :mg-connection nil)
		   :connected))))
     (schema-bins schema))))

(defstruct sb-constraint
  variables
  other-slots
  set-slot-fn)

(defun create-mg-constraint (schema)
  (let* ((cn (make-sb-constraint
	      :other-slots
	      (list :mg-connection :unconnected
		    :mg-variable-paths (list '(:box) (list (gensym)))))))
    (let* ((symbol (gensym))
	   (sl (make-sl :name symbol
			:value cn)))
      (setf (gethash symbol (schema-bins schema)) sl)
      sl)))

(defvar *graphical-object* (make-schema :bins (make-hash-table :test #'eq)))
(defvar *rectangle* (make-schema :bins (make-hash-table :test #'eq)))

(setf (gethash :is-a (schema-bins *graphical-object*))
      (make-sl :name :is-a :value nil))



(setf (gethash :filling-style (schema-bins *graphical-object*))
      (make-sl :name :filling-style
	       :bits 33))

(setf (gethash :line-style (schema-bins *graphical-object*))
      (make-sl :name :line-style
	       :bits 33))

(setf (gethash :is-a (schema-bins *rectangle*))
      (make-sl :name :is-a
	       :value (list *graphical-object*)))

(setf (gethash :update-slots (schema-bins *rectangle*))
      (make-sl :value '(:fast-redraw-p)))

(setf (gethash :initialize (schema-bins *graphical-object*))
      (make-sl :name :initialize :bits 0))

(setf (gethash :fast-redraw-p (schema-bins *rectangle*))
      (make-sl :name :fast-redraw-p
	       :value *no-value*
	       :bits 8192))

(maphash
 #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
     (declare (ignore iterate-ignored-slot-name))
     (let ((slot (sl-name iterate-slot-value-entry))
	   (bits (logand (sl-bits iterate-slot-value-entry) 1023)))
       (unless (zerop bits)
	 (setf (gethash slot (schema-bins *rectangle*))
	       (make-sl :name slot
			:value *no-value*
			:bits bits)))))
 (schema-bins *graphical-object*))

(defvar *axis-rectangle* (make-schema))

(setf (schema-bins *axis-rectangle*) (make-hash-table :test #'eq))

(setf (gethash :is-a (schema-bins *axis-rectangle*))
      (make-sl :name :is-a :value (list *rectangle*)))

(setf (gethash :is-a-inv (schema-bins *rectangle*))
      (make-sl :name :is-a-inv))

(loop repeat 4
   collect (create-mg-constraint *axis-rectangle*))

(maphash
 #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
     (declare (ignore iterate-ignored-slot-name))
     (let ((slot (sl-name iterate-slot-value-entry))
	   (bits (logand (sl-bits iterate-slot-value-entry)
			 1023)))
       (unless (zerop bits)
	 (setf (gethash slot (schema-bins *axis-rectangle*))
	       (make-sl :name slot
			:value *no-value*
			:bits bits)))))
 (schema-bins *rectangle*))


;; Note, eliminating this and change the corresponding call to
;; kr-init-method-hook caused the memorry error #0x rather than #5x
(setf (symbol-function 'kr-init-method) #'kr-init-method-hook)

(kr-init-method-hook *axis-rectangle*)
