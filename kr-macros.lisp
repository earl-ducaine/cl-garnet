
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *special-kr-optimization*
    '(optimize
      (speed 3)
      (safety 0)
      (space 0)
      (debug 0))))

(defstruct (schema (:predicate is-schema))
  name
  bins)

(declaim (inline schema-p))
(defun schema-p (obj)
  (locally (declare #.*special-kr-optimization*)
    (and (is-schema obj)
	 (hash-table-p (schema-bins obj))
	 T)))

(defstruct (a-formula (:include schema))
  schema
  slot
  cached-value
  is-a)

(defstruct (sl)
  name
  value
  (bits 0 :type fixnum))

(defstruct (full-sl (:include sl))
  dependents)

(defparameter *no-value* '(:no-value)
  "A cons cell which is used to mark the value of non-existent slots.")

(declaim (fixnum *schema-counter*))
(defvar *schema-counter* 0
  "This variable is used to generate schema numbers for schemata that
  are created with (create-schema NIL).")


(declaim (fixnum *type-bits* *type-mask* *inherited-bit*
		 *is-parent-bit* *is-constant-bit* *is-update-slot-bit*
		 *is-local-only-slot-bit* *is-parameter-slot-bit*))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defparameter *type-bits* 10)  ;; # of bits for encoding type
  (defparameter *type-mask* (1- (expt 2 *type-bits*))) ;; to extract type
  (defparameter *inherited-bit*          *type-bits*)
  (defparameter *is-parent-bit*          (1+ *inherited-bit*))
  (defparameter *is-constant-bit*        (1+ *is-parent-bit*))
  (defparameter *is-update-slot-bit*     (1+ *is-constant-bit*)))

(declaim (fixnum *local-mask* *constant-mask* *is-update-slot-mask*
		 *inherited-mask* *is-parent-mask*
		 *inherited-parent-mask* *not-inherited-mask*
		 *not-parent-mask* *not-parent-constant-mask*
		 *all-bits-mask*))
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defparameter *local-mask* 0)
  (defparameter *constant-mask* (ash 1 *is-constant-bit*))
  (defparameter *is-update-slot-mask* (ash 1 *is-update-slot-bit*))
  (defparameter *inherited-mask* (ash 1 *inherited-bit*))
  (defparameter *is-parent-mask* (ash 1 *is-parent-bit*))
  (defparameter *inherited-parent-mask*
    (logior *inherited-mask* *is-parent-mask*))
  (defparameter *not-inherited-mask* (lognot *inherited-mask*))
  (defparameter *not-parent-mask* (lognot *is-parent-mask*))
  (defparameter *not-parent-constant-mask*
    (lognot (logior *is-parent-mask* *constant-mask*)))
  (defparameter *all-bits-mask* (lognot *type-mask*)))

(declaim (inline
	  is-inherited is-parent
	  extract-type-code))

(defun formula-p (thing)
  (a-formula-p thing))

(defun is-inherited (bits)
  (declare (fixnum bits))
  (logbitp *inherited-bit* bits))

(defun is-parent (bits)
  (declare (fixnum bits))
  (logbitp *is-parent-bit* bits))

(defun set-is-update-slot (bits)
  (declare (fixnum bits))
  (logior *is-update-slot-mask* bits))

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
      (if (if entry (not (is-inherited (sl-bits entry))))
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
	      (setf (sl-bits a-hash) *local-mask*)
	      (setf (gethash inverse schema-bins) a-hash))))))))

(defun s-value-fn (schema slot value)
  (locally (declare #.*special-kr-optimization*)
    (unless (schema-p schema)
      (return-from s-value-fn (values value t)))
    (let* ((entry (slot-accessor schema slot)))
      (let ((is-formula nil)
	    (is-relation nil))
	(when (formula-p value)
	  (setf is-formula T)
	  (unless (schema-name value)
	    (incf *schema-counter*)
	    (setf (schema-name value) *schema-counter*)))
	(when is-relation
	  (link-in-relation schema slot value))
	(let ((new-bits (or the-bits *local-mask*)))
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
	 (a-hash (gethash :is-a schema-bins)))
    (setf a-hash (make-sl))
    (setf (sl-name a-hash) :is-a)
    (setf (sl-value a-hash) value)
    (setf (sl-bits a-hash) *local-mask*)
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

(defun allocate-schema-slots (schema)
  (locally (declare #.*special-kr-optimization*)
    (setf (schema-bins schema)
	  (make-hash-table :test #'eq
			   )))
  schema)

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
	(if entry
	    (setf (sl-bits entry) (set-is-update-slot (sl-bits entry)))
	    (let* ((schema-bins (schema-bins the-schema))
		   (a-hash (gethash slot schema-bins)))
	      (setf a-hash (make-sl))
	      (setf (sl-name a-hash) slot)
	      (setf (sl-value a-hash) *no-value*)
	      (setf (sl-bits a-hash) (set-is-update-slot *local-mask*))
	      (when dependants (setf (full-sl-dependents a-hash) dependants))
	      (setf (gethash slot schema-bins) a-hash)))))
    (dolist (parent parents)
      (locally
	  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
	(progn
	  (maphash
	   #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	       (declare (ignore iterate-ignored-slot-name))
	       (let ((slot (sl-name iterate-slot-value-entry))
		     (bits (logand (sl-bits iterate-slot-value-entry)
				   *type-mask*)))
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
			   (when dependants (setf (full-sl-dependents a-hash) dependants))
			   (setf (gethash slot schema-bins) a-hash)))))))
	   (schema-bins parent)))))))

(defun do-schema-body (schema is-a generate-instance override types
		       &rest slot-specifiers)
  (unless (listp is-a)
    (setf is-a (list is-a)))
  (when is-a
    (set-is-a schema is-a))
  (do* ((slots slot-specifiers (cdr slots))
	(slot (car slots) (car slots)))
       ((null slots)
	(unless (eq types :NONE)
	  (dolist (type types)
	    (dolist (slot (cdr type))
	      (let* ((schema-bins (schema-bins schema))
		     (hash (gethash slot schema-bins))
		     (dependants nil))
		(if hash
		    (progn
		      (when (and dependants (not (full-sl-p hash)))
			(setf (gethash slot schema-bins) (setf hash (make-full-sl)))
			(setf (sl-name hash) slot))
		      (setf (sl-value hash) *no-value*)
		      (setf (sl-bits hash) 33)
		      (when dependants (setf (full-sl-dependents hash) dependants))
		      hash)
		    (progn
		      (setf hash
			    (if dependants
				(make-full-sl)
				(make-sl)))
		      (setf (sl-name hash) slot)
		      (setf (sl-value hash) *no-value*)
		      (setf (sl-bits hash) 33)
		      (when dependants (setf (full-sl-dependents hash) dependants))
		      (setf (gethash slot schema-bins) hash)))))))
	(process-constant-slots schema is-a)
	(kr-init-method schema))
    (let ((slot-name (car slot))
	  (slot-value (cdr slot)))
      (let* ((slot slot-name)
	    (value slot-value)
	    (schema-bins (schema-bins schema))
	    (a-hash (gethash slot-name schema-bins)))
	  (setf a-hash (make-sl))
	  (setf (sl-name a-hash) slot-name)
	  (setf (sl-value a-hash) value)
	  (setf (sl-bits a-hash) *local-mask*)
	  (setf (gethash slot-name schema-bins) a-hash)
	  value))))

(defun do-schema-body-alt (&rest slot-specifiers)
  (let ((schema (make-a-new-schema '*axis-rectangle*))
	(is-a rectangle)
	(types nil))
    (setf is-a (list is-a))
    (set-is-a schema is-a)
    (do* ((slots slot-specifiers (cdr slots))
	  (slot (car slots) (car slots)))
	 ((null slots)
	  (process-constant-slots schema is-a)
	  (kr-init-method schema)
	  schema)
      (let ((a-hash (make-sl)))
	  (setf (sl-name a-hash) (car slot))
	  (setf (sl-value a-hash) (cdr slot))
	  (setf (gethash (car slot) (schema-bins schema)) a-hash)
	  (cdr slot)))))


(do-schema-body (make-a-new-schema 'graphical-object) nil t nil
                '(((or (is-a-p filling-style) null) :filling-style)
                  ((or (is-a-p line-style) null) :line-style)))

(do-schema-body (make-a-new-schema 'rectangle) graphical-object t nil nil
                (cons :update-slots '(:fast-redraw-p)))

(defun initialize-method-graphical-object (gob)
  (s-value-fn gob :update-info 'a))

(s-value-fn graphical-object :initialize 'initialize-method-graphical-object)

(defstruct (sb-constraint)
  variables
  other-slots
  set-slot-fn)

(defun get-sb-constraint-slot (obj slot)
  (getf (sb-constraint-other-slots obj) slot nil))

(defmacro cn-variables (cn) `(sb-constraint-variables ,cn))

(defun set-sb-constraint-slot (cn slot val)
  (call-set-slot-fn (sb-constraint-set-slot-fn cn) cn slot val)
  (setf (getf (sb-constraint-other-slots cn) slot nil) val)
  val)

(defsetf cn-variables (cn) (val)
  `(let ((cn ,cn)
	 (val ,val))
     (call-set-slot-fn (sb-constraint-set-slot-fn cn) cn :variables val)
     (setf (sb-constraint-variables cn) val)))

(defstruct (sb-variable)
  other-slots
  set-slot-fn)

(defun set-sb-variable-slot (var slot val)
  (call-set-slot-fn (sb-variable-set-slot-fn var) var slot val)
  (setf (getf (sb-variable-other-slots var) slot nil) val))

(defun call-set-slot-fn (fns obj slot val)
  (cond ((null fns)
	 nil)
	((listp fns)
	 (loop for fn in fns do (call-set-slot-fn fn obj slot val)))
	(t
	 (funcall fns obj slot val))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun get-save-fn-symbol-name (sym)
    (cond ((symbolp sym)
	   (intern (concatenate 'string (symbol-name sym) "-SAVE-FN-SYMBOL")
		   (find-package :cl-user)))
	  ((and (consp sym)
		(eq :quote (first sym))
		(symbolp (second sym)))
	   (get-save-fn-symbol-name (second sym)))
	  (t (cerror "cont" "get-save-fn-symbol-name: bad symbol ~S" sym)))))

(defun install-hook (fn hook)
  (if  (fboundp fn)
       (let ((save-fn (get-save-fn-symbol-name fn)))
	 (unless (fboundp save-fn)
	   (setf (symbol-function save-fn)
		 (symbol-function fn)))
	 (setf (symbol-function fn)
	       (symbol-function hook)))))

(defmacro os (obj slot) `(cons ,obj ,slot))

(defmacro os-p (os)
  `(let ((os ,os))
     (and (consp os)
	  (schema-p (car os)))))

(defmacro os-object (os) `(car ,os))
(defmacro os-slot (os) `(cdr ,os))
(defmacro cn-os (v) `(get-sb-constraint-slot ,v :mg-os))
(defmacro cn-connection (v) `(get-sb-constraint-slot ,v :mg-connection))
(defmacro cn-variable-paths (c) `(get-sb-constraint-slot ,c :mg-variable-paths))
(defsetf cn-os (v) (val) `(set-sb-constraint-slot ,v :mg-os ,val))
(defsetf cn-connection (v) (val) `(set-sb-constraint-slot ,v :mg-connection ,val))

(defsetf cn-variable-paths (c) (val)
  `(set-sb-constraint-slot ,c :mg-variable-paths ,val))

(defun create-mg-constraint (&key variable-paths variable-names)
  (let ((cn (make-sb-constraint)))
    (setf (cn-connection cn) :unconnected)
    (setf (cn-variable-paths cn) variable-paths)
    cn))

(defsetf var-os (v) (val)
  `(set-sb-variable-slot ,v :mg-os ,val))

(defun create-mg-variable ()
  (let* ((var (make-sb-variable)))
    (setf (VAR-os var) nil)
    var))

(defun constraint-p (obj)
  (and (sb-constraint-p obj)
       (not (null (CN-connection obj)))))

(defun add-constraint-to-slot (obj slot cn)
  (setf (CN-os cn) (os obj slot))
  (connect-constraint cn))

(defun kr-init-method-hook (schema)
  (let ((parent (car
		 (LOCALLY
		     (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0) (SPACE 0) (DEBUG 0)))
		   (LET* ((specific-slot-accessor (SLOT-ACCESSOR SCHEMA :IS-A))
			  (slot-accessor
			   (IF specific-slot-accessor
			       (IF (IS-INHERITED (SL-BITS specific-slot-accessor))
				   (IF (A-FORMULA-P (SL-VALUE specific-slot-accessor))
				       (SL-VALUE specific-slot-accessor))
				   (SL-VALUE specific-slot-accessor))
			       *NO-VALUE*)))
		     (IF (A-FORMULA-P slot-accessor)
			 NIL
			 slot-accessor))))))
    (locally
	(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
      (progn
	(maphash
	 #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	     (declare (ignore iterate-ignored-slot-name))
	     (let ((slot (sl-name iterate-slot-value-entry))
		   (value (sl-value iterate-slot-value-entry)))
	       (unless (is-inherited (sl-bits iterate-slot-value-entry))
		 (unless (eq value *no-value*)
		   (let ((slot slot))
		     (let ((value (get-local-value parent slot)))
		       (s-value-fn-save-fn-symbol schema slot value)))))))
	 (schema-bins parent)))))
  (activate-new-instance-cns schema))

(defun activate-new-instance-cns (schema)
  (locally
      (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
    (maphash
     #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	 (declare (ignore iterate-ignored-slot-name))
	 (let* ((slot (sl-name iterate-slot-value-entry))
		(value (get-local-value schema slot)))
	   (if (constraint-p value)
	       (add-constraint-to-slot schema slot value))))
     (schema-bins schema))))

(defun connect-constraint (cn)
  (let* ((cn-var-paths (CN-variable-paths cn)))
    (let* ((root-obj (os-object (CN-os cn)))
	   (cn-path-links nil)
	   (paths-broken nil)
	   var-os-list)
      (setf var-os-list
	    (loop for path in cn-var-paths collect
		 (let ((obj root-obj))
		   (loop for (slot next-slot) on path do
			(when (null next-slot)
			  (return (os obj slot)))
			(set-object-slot-prop obj slot :sb-path-constraints
					      (adjoin cn nil))
			(push (os obj slot) cn-path-links)
			(s-value-fn-save-fn-symbol obj slot nil)
			(setf obj nil)))))
      (setf (cn-variables cn)
	    (loop for var-os in var-os-list
	       collect (create-object-slot-var
			(os-object var-os)
			(os-slot var-os))))
      (setf (CN-connection cn) :connected))))

(defun set-object-slot-prop (obj slot prop val)
  (let* (os-props
	 (slot-props (getf os-props slot nil)))
    (setf (getf slot-props prop) val)
    (setf (getf os-props slot) slot-props)
    val))

(defun create-object-slot-var (obj slot)
  (let ((var (create-mg-variable)))
    (set-object-slot-prop obj slot :sb-variable var)
    (s-value-fn-save-fn-symbol obj slot nil)
    var))

(eval-when (:load-toplevel :execute)
  (loop for (fn hook-fn) on '(s-value-fn s-value-fn-save-fn-symbol
			      kr-init-method kr-init-method-hook) by #'CDDR
     do (install-hook fn hook-fn)))

(do-schema-body-alt
    (cons :height-cn
	  (create-mg-constraint
	   :variable-paths '((:box) (:height))
	   :variable-names '(box height)))
    (cons :width-cn
	  (create-mg-constraint
	   :variable-paths '((:box) (:width))
	   :variable-names
	   '(box width)))
    (cons :top-cn
	  (create-mg-constraint
	   :variable-paths '((:box) (:top))
	   :variable-names '(box top)))
    (cons :left-cn
	  (create-mg-constraint
	   :variable-paths '((:box) (:left))
	   :variable-names '(box left))))
