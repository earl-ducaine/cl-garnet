
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

(declaim (inline is-parent))

(defun is-parent (bits)
  (declare (fixnum bits))
  (logbitp 11 bits))

(declaim (inline slot-accessor))
(defun slot-accessor (schema slot)
  "Returns a slot structure, or NIL."
  (values (gethash slot (schema-bins schema))))

(defparameter iterate-slot-value-entry nil
  "Ugly")

(defun s-value-fn (schema slot)
  (setf (gethash slot (schema-bins schema))
	(make-sl :name slot :bits 0)))

(defun get-sb-constraint-slot (obj slot)
  (getf (sb-constraint-other-slots obj) slot nil))

(defun kr-init-method-hook (schema)
  (let ((parent (car (sl-value (slot-accessor schema :is-a)))))
    (locally
	(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
      (maphash
       #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	   (declare (ignore iterate-ignored-slot-name))
	   (let ((slot (sl-name iterate-slot-value-entry)))
	     (setf (gethash slot (schema-bins schema))
		   (make-sl :name slot :bits 0))))
       (schema-bins parent))))
  (locally
      (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
    (maphash
     #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	 (declare (ignore iterate-ignored-slot-name))
	 (let* ((cn
		 (sl-value (slot-accessor
			    schema
			    (sl-name iterate-slot-value-entry)))))
	   (when (and (sb-constraint-p cn)
		      (get-sb-constraint-slot cn :mg-connection))
	     (setf (getf (sb-constraint-other-slots cn) :mg-os nil)
		   (cons schema (sl-name iterate-slot-value-entry)))
	     (let* ((constraint-slot
		     (loop for path in (get-sb-constraint-slot
					cn
					:mg-variable-paths)
			collect (loop for (slot next-slot) on path
				   do (return
					(cons
					 (car
					  (get-sb-constraint-slot
					   cn :mg-os))
					 slot))))))
	       (let ((new1
		      (loop for var-os in constraint-slot
			 collect (let ((obj (car var-os))
				       (slot (cdr var-os)))
				   (s-value-fn obj slot)
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

(s-value-fn *graphical-object*
	    :initialize)

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
