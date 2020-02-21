
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

(defstruct (sl)
  name
  value
  (bits 0 :type fixnum))

(defstruct (full-sl (:include sl))
  dependents)

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

(defun g-value-no-copy (schema)
  (let ((value (slot-accessor schema :UPDATE-SLOTS)))
    (cond
      (value
       (sl-value value))
      (t
       ;; note the following creates bug in SBCL that's not
       ;; recreatatable without the dolist.
       (let ((schema-self (get-local-value schema :IS-A)))
	 (dolist (*schema-self* schema-self)
	   (let ((value (g-value-no-copy *schema-self*)))
	     (return-from g-value-no-copy value))))))))

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


(defun make-a-new-schema (name)
  (locally (declare #.*special-kr-optimization*)
    (eval `(defvar ,name))
    (let ((schema (make-schema)))
      (allocate-schema-slots schema)
      (set name schema))))

(defun process-constant-slots (the-schema parents)
  (locally (declare #.*special-kr-optimization*)
    (dolist (slot (g-value-no-copy the-schema))
      (let ((entry (slot-accessor the-schema slot)))
	(let* ((schema-bins (schema-bins the-schema))
	       (a-hash (gethash slot schema-bins)))
	  (setf a-hash (make-sl))
	  (setf (sl-name a-hash) slot)
	  (setf (sl-value a-hash) *no-value*)
	  (setf (sl-bits a-hash) 8192)
	  (setf (full-sl-dependents a-hash) dependants)
	  (setf (gethash slot schema-bins) a-hash))))
    (dolist (parent parents)
      (locally
	  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
	(progn
	  (maphash
	   #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	       (declare (ignore iterate-ignored-slot-name))
	       (let ((slot (sl-name iterate-slot-value-entry))
		     (bits (logand (sl-bits iterate-slot-value-entry)
				   1023)))
		 (unless (zerop bits)
		   (let ((the-entry (slot-accessor the-schema slot)))
		     (if the-entry
			 (sl-bits the-entry)
			 (let* ((schema-bins (schema-bins the-schema))
				(a-hash (gethash slot schema-bins)))
			   (setf a-hash (make-sl))
			   (setf (sl-name a-hash) slot)
			   (setf (sl-value a-hash) *no-value*)
			   (setf (sl-bits a-hash) bits)
			   (when dependants
			     (setf (full-sl-dependents a-hash) dependants))
			   (setf (gethash slot schema-bins) a-hash)))))))
	   (schema-bins parent)))))))

;; (defun do-schema-body (schema is-a generate-instance override types
;; 		       &rest slot-specifiers)
;;   (unless (listp is-a)
;;     (setf is-a (list is-a)))
;;   (set-is-a schema is-a)
;;   (do* ((slots slot-specifiers (cdr slots))
;; 	(slot (car slots) (car slots)))
;;        ((null slots)
;; 	(unless (eq types :NONE)
;; 	  (dolist (type types)
;; 	    (dolist (slot (cdr type))
;; 	      (let* ((schema-bins (schema-bins schema))
;; 		     (hash (gethash slot schema-bins)))
;; 		(setf hash (make-sl))
;; 		(setf (sl-name hash) slot)
;; 		(setf (sl-value hash) *no-value*)
;; 		(setf (sl-bits hash) 33)
;; 		(setf (gethash slot schema-bins) hash)))))
;; 	(process-constant-slots schema is-a)
;; 	(kr-init-method schema))
;;     (let* ((slot-name (car slot))
;; 	   (slot-value (cdr slot))
;; 	   (schema-bins (schema-bins schema))
;; 	   (new-slot (make-sl)))
;;       (setf (sl-name new-slot) slot-name)
;;       (setf (sl-value new-slot) slot-value)
;;       (setf (sl-bits new-slot) 0)
;;       (setf (gethash slot-name schema-bins) new-slot)
;;       slot-value)))


(defun get-sb-constraint-slot (obj slot)
  (getf (sb-constraint-other-slots obj) slot nil))

(defmacro cn-variables (cn) `(sb-constraint-variables ,cn))

(defsetf cn-variables (cn) (val)
  `(let ((cn ,cn)
	 (val ,val))
     (setf (sb-constraint-variables cn) val)))

(defstruct (sb-variable)
  other-slots
  set-slot-fn)

(defun set-sb-variable-slot (var slot val)
  (setf (getf (sb-variable-other-slots var) slot nil) val))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun get-save-fn-symbol-name (sym)
    (intern (concatenate 'string (symbol-name sym) "-SAVE-FN-SYMBOL"))))

(defsetf var-os (v) (val)
  `(set-sb-variable-slot ,v :mg-os ,val))

(defun create-mg-variable ()
  (let* ((var (make-sb-variable)))
    (setf (VAR-os var) nil)
    var))

(defun constraint-p (obj)
  (and (sb-constraint-p obj)
       (not (null (GET-SB-CONSTRAINT-SLOT OBJ :MG-CONNECTION)))))

(defun kr-init-method-hook (schema)
  (let ((parent (car (sl-value (slot-accessor schema :is-a)))))
    (locally
	(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
      (maphash
       #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	   (declare (ignore iterate-ignored-slot-name))
	   (let ((slot (sl-name iterate-slot-value-entry))
		 (value (get-local-value parent slot)))
	       (s-value-fn schema slot value)))
       (schema-bins parent))))
  (activate-new-instance-cns schema))

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


(defun set-object-slot-prop (obj slot prop val)
  (let* (os-props
	 (slot-props (getf os-props slot nil)))
    (setf (getf slot-props prop) val)
    (setf (getf os-props slot) slot-props)
    val))

(defun create-object-slot-var (obj slot)
  (let ((var (create-mg-variable)))
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

(defun do-schema-body-alt ()
  (let ((schema (make-a-new-schema '*axis-rectangle*)))
    (set-is-a schema (list *rectangle*))
    (loop repeat 4
       collect (create-mg-constraint schema))
    (process-constant-slots schema (list *rectangle*))
    (kr-init-method-hook schema)))

(defun allocate-schema-slots (schema)
    (setf (schema-bins schema)
	  (make-hash-table :test #'eq)))

(defvar *graphical-object* (make-schema :bins (make-hash-table :test #'eq)))
(defvar *rectangle* (make-schema :bins (make-hash-table :test #'eq)))

(set-is-a *graphical-object* nil)

(setf (gethash :filling-style (schema-bins *graphical-object*))
      (make-sl :name :filling-style
	       :value *no-value*
	       :bits 33))

(let* ((schema-bins (schema-bins *graphical-object*))
       (hash (make-sl)))
  (setf (sl-name hash) :line-style)
  (setf (sl-value hash) *no-value*)
  (setf (sl-bits hash) 33)
  (setf (gethash :line-style schema-bins) hash))

(process-constant-slots *graphical-object* nil)

(kr-init-method *graphical-object*)

(set-is-a *rectangle* (list *graphical-object*))

(setf (gethash :update-slots (schema-bins *rectangle*))
      (make-sl :value '(:fast-redraw-p)))

(process-constant-slots *rectangle* (list *graphical-object*))

(kr-init-method *rectangle*)

(s-value-fn *graphical-object*
	    :initialize 'initialize-method-graphical-object)

;; Note, eliminating this and change the corresponding call to
;; kr-init-method-hook caused the memorry error #0x rather than #5x
(setf (symbol-function 'kr-init-method) #'kr-init-method-hook)

(do-schema-body-alt)
