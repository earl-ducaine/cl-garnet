
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

;; This must be the core of the bug
(declaim (inline get-local-value))
(defun get-local-value (schema slot)
  (locally (declare #.*special-kr-optimization*)
    (let ((entry (slot-accessor schema slot)))
      (if (if entry (not (logbitp 10 (sl-bits entry))))
	  (sl-value entry)))))

(defun link-in-relation (schema slot values)
  (let ((inverse (when (eq slot :is-a)
		   :is-a-inv)))
    (when inverse
      (dolist (value values)
	(let* ((entry (if (eq slot :is-a)
			  (slot-accessor value :is-a-inv)
			  (slot-accessor value inverse)))
	       (previous-values (when entry (sl-value entry))))
	  (unless entry
	    (let* ((schema-bins (schema-bins value))
		   (a-hash (gethash inverse schema-bins)))
	      (setf a-hash (make-sl))
	      (setf (sl-name a-hash) inverse)
	      (setf (sl-value a-hash) (list schema))
	      (setf (sl-bits a-hash) 0)
	      (setf (gethash inverse schema-bins) a-hash))))))))

(defun s-value-fn (schema slot value)
  (locally (declare #.*special-kr-optimization*)
    (let* ((entry (slot-accessor schema slot)))
      (let ((is-formula nil)
	    (is-relation nil))
	(when nil
	  (setf is-formula T)
	  (unless (schema-name value)
	    (incf *schema-counter*)
	    (setf (schema-name value) *schema-counter*)))
	(when is-relation
	  (link-in-relation schema slot value))
	(let ((new-bits (or the-bits 0)))
	  (if entry
	      (setf (sl-value entry) value
		    (sl-bits entry) new-bits)
	      (let* ((schema-bins (schema-bins schema))
		     (a-hash (gethash slot schema-bins)))
		(setf a-hash (make-sl))
		(setf (sl-name a-hash) slot)
		(setf (sl-value a-hash) value)
		(setf (sl-bits a-hash) new-bits)
		(setf (gethash slot schema-bins) a-hash))))
	(when (and the-bits (is-parent the-bits))
	  (update-inherited-values schema slot value t))
	(values value nil)))))

(defun set-is-a (schema value)
  (let* ((schema-bins (schema-bins schema))
	 (a-hash (make-sl)))
    (setf (sl-name a-hash) :is-a)
    (setf (sl-value a-hash) value)
    (setf (gethash :is-a schema-bins) a-hash))
  (link-in-relation schema :is-a value)
  value)

(defun find-parent (schema slot)
  (dolist (a-parent (get-local-value schema :is-a))
    (when a-parent
      (multiple-value-bind (value the-parent)
	  (find-parent a-parent slot)
	(when value
	  (return-from find-parent (values value the-parent)))))))

(defun kr-init-method (schema &optional the-function)
  (multiple-value-setq (the-function)
    (find-parent schema :initialize))
  (when the-function
    (funcall the-function schema)))

(defun get-sb-constraint-slot (obj slot)
  (getf (sb-constraint-other-slots obj) slot nil))

(defmacro cn-variables (cn) `(sb-constraint-variables ,cn))

(defsetf cn-variables (cn) (val)
  `(let ((cn ,cn)
	 (val ,val))
     (setf (sb-constraint-variables cn) val)))

(defstruct sb-variable
  other-slots
  set-slot-fn)

(defun constraint-p (obj)
  (and (sb-constraint-p obj)
       (not (null (GET-SB-CONSTRAINT-SLOT OBJ :MG-CONNECTION)))))

(defun connect-constraint (cn)
  (let* ((cn-var-paths (GET-SB-CONSTRAINT-SLOT CN :MG-VARIABLE-PATHS)))
    (let* ((root-obj (car (GET-SB-CONSTRAINT-SLOT CN :MG-OS)))
	   (cn-path-links nil)
	   (paths-broken nil)
	   var-os-list)
      (setf var-os-list
	    (loop for path in cn-var-paths collect
		 (let ((obj root-obj))
		   (loop for (slot next-slot) on path do
			(when (null next-slot)
			  (return (cons obj slot)))
			(set-object-slot-prop obj slot :sb-path-constraints
					      (adjoin cn nil))
			(push (cons obj slot) cn-path-links)
			(s-value-fn obj slot nil)
			(setf obj nil)))))
      (setf (cn-variables cn)
	    (loop for var-os in var-os-list
	       collect (create-object-slot-var
			(car var-os)
			(cdr var-os))))
        (sb-constraint-set-slot-fn cn)
  (setf (getf (sb-constraint-other-slots cn) :MG-CONNECTION nil) :CONNECTED))))

(defun activate-new-instance-cns (schema)
  (locally
      (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
    (maphash
     #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	 (declare (ignore iterate-ignored-slot-name))
	 (let* ((slot (sl-name iterate-slot-value-entry))
		(cn (get-local-value schema slot)))
	   (when (constraint-p cn)
	     (sb-constraint-set-slot-fn cn)
	     (setf (getf (sb-constraint-other-slots cn) :mg-os nil) (cons schema slot))
	     (connect-constraint cn))))
     (schema-bins schema))))

(defun kr-init-method-hook (schema)
  (let ((parent (car (sl-value (slot-accessor schema :is-a)))))
    (locally
	(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
      (maphash
       #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	   (declare (ignore iterate-ignored-slot-name))
	   (s-value-fn schema (sl-name iterate-slot-value-entry)
		       (get-local-value parent slot)))
       (schema-bins parent))))
  (activate-new-instance-cns schema))



(defun set-object-slot-prop (obj slot prop val)
  (let* (os-props
	 (slot-props (getf os-props slot nil)))
    (setf (getf slot-props prop) val)
    (setf (getf os-props slot) slot-props)
    val))

(defun create-object-slot-var (obj slot)
  (let ((var (make-sb-variable)))
    (set-object-slot-prop obj slot :sb-variable var)
    (s-value-fn obj slot nil)
    var))

(defun initialize-method-graphical-object (gob)
  (s-value-fn gob :update-info 'a))

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
	   (sl (make-sl)))
      (setf (sl-name sl) symbol)
      (setf (sl-value sl) cn)
      (setf (gethash symbol (schema-bins schema)) sl))))

(defvar *graphical-object* (make-schema :bins (make-hash-table :test #'eq)))
(defvar *rectangle* (make-schema :bins (make-hash-table :test #'eq)))

(set-is-a *graphical-object* nil)

(setf (gethash :filling-style (schema-bins *graphical-object*))
      (make-sl :name :filling-style
	       :bits 33))

(setf (gethash :line-style (schema-bins *graphical-object*))
      (make-sl :name :line-style
	       :bits 33))

(kr-init-method *graphical-object*)

(set-is-a *rectangle* (list *graphical-object*))

(setf (gethash :update-slots (schema-bins *rectangle*))
      (make-sl :value '(:fast-redraw-p)))

(kr-init-method *rectangle*)

(s-value-fn *graphical-object*
	    :initialize 'initialize-method-graphical-object)

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
	 (format t "slot: ~s~%" slot)
	 (setf (gethash slot (schema-bins *rectangle*))
	       (make-sl :name slot
			:value *no-value*
			:bits bits)))))
 (schema-bins *graphical-object*))

(defvar *axis-rectangle* (make-schema))

(setf (schema-bins *axis-rectangle*) (make-hash-table :test #'eq))


(set-is-a *axis-rectangle* (list *rectangle*))

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
