
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
	 (a-hash (gethash :is-a schema-bins)))
    (setf a-hash (make-sl))
    (setf (sl-name a-hash) :is-a)
    (setf (sl-value a-hash) value)
    (setf (sl-bits a-hash) 0)
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
    (setf (schema-bins schema)
	  (make-hash-table :test #'eq)))

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

(defun do-schema-body (schema is-a generate-instance override types
		       &rest slot-specifiers)
  (unless (listp is-a)
    (setf is-a (list is-a)))
  (set-is-a schema is-a)
  (do* ((slots slot-specifiers (cdr slots))
	(slot (car slots) (car slots)))
       ((null slots)
	(unless (eq types :NONE)
	  (dolist (type types)
	    (dolist (slot (cdr type))
	      (let* ((schema-bins (schema-bins schema))
		     (hash (gethash slot schema-bins)))
		(setf hash (make-sl))
		(setf (sl-name hash) slot)
		(setf (sl-value hash) *no-value*)
		(setf (sl-bits hash) 33)
		(setf (gethash slot schema-bins) hash)))))
	(process-constant-slots schema is-a)
	(kr-init-method schema))
    (let* ((slot-name (car slot))
	   (slot-value (cdr slot))
	   (schema-bins (schema-bins schema))
	   (new-slot (make-sl)))
      (setf (sl-name new-slot) slot-name)
      (setf (sl-value new-slot) slot-value)
      (setf (sl-bits new-slot) 0)
      (setf (gethash slot-name schema-bins) new-slot)
      slot-value)))

(defun do-schema-body-alt (&rest slot-specifiers)
  (let ((schema (make-a-new-schema '*axis-rectangle*)))
    (set-is-a schema (list rectangle))
    (do* ((slots slot-specifiers (cdr slots)))
	 ((null slots)
	  (process-constant-slots schema (list rectangle))
	  (kr-init-method schema)
	  schema)
      (let ((a-hash (make-sl)))
	  (setf (sl-name a-hash) (caar slots))
	  (setf (sl-value a-hash) (cdar slots))
	  (setf (gethash (caar slots) (schema-bins schema)) a-hash)
	  (cdar slots)))))

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
    (intern (concatenate 'string (symbol-name sym) "-SAVE-FN-SYMBOL"))))

(defmacro cn-os (v) `(get-sb-constraint-slot ,v :mg-os))
(defmacro cn-connection (v) `(get-sb-constraint-slot ,v :mg-connection))
(defmacro cn-variable-paths (c) `(get-sb-constraint-slot ,c :mg-variable-paths))
(defsetf cn-os (v) (val) `(set-sb-constraint-slot ,v :mg-os ,val))
(defsetf cn-connection (v) (val) `(set-sb-constraint-slot ,v :mg-connection ,val))

(defsetf cn-variable-paths (c) (val)
  `(set-sb-constraint-slot ,c :mg-variable-paths ,val))

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
  (setf (cn-os cn) (cons obj slot))
  (connect-constraint cn))

(defun kr-init-method-hook (schema)
  (let ((parent (car (sl-value (slot-accessor schema :is-a)))))
    (locally
	(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
      (maphash
       #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	   (declare (ignore iterate-ignored-slot-name))
	   (let ((slot (sl-name iterate-slot-value-entry))
		 (value (get-local-value parent slot)))
	       (s-value-fn-save-fn-symbol schema slot value)))
       (schema-bins parent))))
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
    (let* ((root-obj (car (CN-os cn)))
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
			(s-value-fn-save-fn-symbol obj slot nil)
			(setf obj nil)))))
      (setf (cn-variables cn)
	    (loop for var-os in var-os-list
	       collect (create-object-slot-var
			(car var-os)
			(cdr var-os))))
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

(defun install-hook (fn hook)
  (let ((save-fn (get-save-fn-symbol-name fn)))
    (setf (symbol-function save-fn)
	  (symbol-function fn))
    (setf (symbol-function fn)
	  (symbol-function hook))))

(defun initialize-method-graphical-object (gob)
  (s-value-fn gob :update-info 'a))

(let* ((graphical-object-schema-name 'graphical-object)
       (rectangle-schema-name 'rectangle)
       (graphical-object-schema (make-schema))
       (rectangle-schema (make-schema)))
  (eval `(defvar ,graphical-object-schema-name))
  (eval `(defvar ,rectangle-schema-name))
  (allocate-schema-slots graphical-object-schema)
  (allocate-schema-slots rectangle-schema)
  (set graphical-object-schema-name graphical-object-schema)
  (set rectangle-schema-name rectangle-schema)
  (do-schema-body graphical-object-schema nil t nil
		  '(((or (is-a-p filling-style) null) :filling-style)
		    ((or (is-a-p line-style) null) :line-style)))
  (do-schema-body rectangle-schema graphical-object-schema t nil nil
		  (cons :update-slots '(:fast-redraw-p))))

(s-value-fn graphical-object
	    :initialize 'initialize-method-graphical-object)

(install-hook 's-value-fn 's-value-fn-save-fn-symbol)
(install-hook 'kr-init-method 'kr-init-method-hook)

(defun do-schema-body-alt (&rest slot-specifiers)
  (let ((schema (make-a-new-schema '*axis-rectangle*)))
    (set-is-a schema (list rectangle))
    (do* ((slots slot-specifiers (cdr slots)))
	 ((null slots)
	  (process-constant-slots schema (list rectangle))
	  (kr-init-method schema)
	  schema)
      (let ((a-hash (make-sl)))
	  (setf (sl-name a-hash) (caar slots))
	  (setf (sl-value a-hash) (cdar slots))
	  (setf (gethash (caar slots) (schema-bins schema)) a-hash)
	  (cdar slots)))))

(defun create-mg-constraint (leaf)
  (let ((cn (make-sb-constraint))
	(path (list '(:box) leaf)))
    (setf (cn-connection cn) :unconnected)
    (format t "variable-paths ~s~%" path)
    (setf (cn-variable-paths cn) path)
    cn))

(do-schema-body-alt
    (cons :height-cn (create-mg-constraint '(:height)))
    (cons :width-cn (create-mg-constraint '(:width)))
    (cons :top-cn (create-mg-constraint '(:top)))
    (cons :left-cn (create-mg-constraint '(:left))))
