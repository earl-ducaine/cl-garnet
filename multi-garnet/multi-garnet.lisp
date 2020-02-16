

;; (in-package :kr)

(defvar *multi-garnet-version* "2.2")

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
  (if (not (fboundp fn))
      (cerror "noop" "can't install hook on fn ~S -- no fn def" fn)
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
(defmacro cn-variable-names (c) `(get-sb-constraint-slot ,c :mg-variable-names))
(defmacro cn-path-slot-list (c) `(get-sb-constraint-slot ,c :mg-path-slot-list))
(defsetf cn-os (v) (val) `(set-sb-constraint-slot ,v :mg-os ,val))
(defsetf cn-connection (v) (val) `(set-sb-constraint-slot ,v :mg-connection ,val))
(defsetf cn-variable-paths (c) (val) `(set-sb-constraint-slot ,c :mg-variable-paths ,val))
(defsetf cn-variable-names (c) (val) `(set-sb-constraint-slot ,c :mg-variable-names ,val))
(defsetf cn-path-slot-list (c) (val) `(set-sb-constraint-slot ,c :mg-path-slot-list ,val))

(defmacro cn-connection-p (cn val)
  `(eq (cn-connection ,cn) ,val))

(defun create-mg-constraint (&key (variable-paths nil) (variable-names nil))
  (let ((cn (make-sb-constraint :strength :max
				:methods (list
					  (create-mg-method :output-indices '(1) :code
							    #'(lambda (cn) nil)))
				:variables nil
				:selected-method nil
				:mark nil
				:set-slot-fn nil
				:other-slots nil)))
    (setf (cn-connection cn) :unconnected)
    (setf (cn-variable-paths cn) variable-paths)
    cn))

(defsetf var-os (v) (val) `(set-sb-variable-slot ,v :mg-os ,val))

(defun create-mg-variable (&key (name nil)
			     (os nil))
  (let* ((val (if (os-p os)
		  (g-value (os-object os) (os-slot os))
		  nil))
	 (var (create-sb-variable :name name
				  :value val)))
    (setf (VAR-os var) os)
    var))

;; returns true if this is a *multi-garnet* constraint
(defun constraint-p (obj)
  (and (sb-constraint-p obj)
       (not (null (CN-connection obj)))))

(defun create-mg-method (&key (output-indices nil)
			   (code #'(lambda (cn) cn)))
  (let ((mt (create-sb-method :outputs nil
			      :code code)))
    nil
    mt))

(defun init-method-outputs (cn)
  (let ((vars (cn-variables cn)))
    (loop for mt in (cn-methods cn) do
	 (setf (mt-outputs mt)
	       (loop for index in (get-sb-method-slot mt :mg-output-indices)
		  collect (nth index vars))))))

(defvar *update-invalid-paths-formulas* t)

(defmacro with-no-invalidation-update (&rest forms)
  (let* ((old-val-var (gentemp)))
    `(let* ((,old-val-var *update-invalid-paths-formulas*))
       (unwind-protect
	    (progn
	      (setq *update-invalid-paths-formulas* nil)
	      (progn ,@forms))
	 (setq *update-invalid-paths-formulas* ,old-val-var)))
    ))

(defvar *invalidated-path-constraints* nil)

(defun constraint-in-obj-slot (cn obj slot)
  (let* ((os (cn-os cn)))
    (and os
	 (eql (os-object os) obj)
	 (eql (os-slot os) slot))))

(defun add-constraint-to-slot (obj slot cn)
  (cond ((null (CN-variable-paths cn))
         nil)
        ((CN-os cn)
         (error "shouldn't happen: can't add constraint ~S to <~S,~S>, already stored in <~S,~S>"
		cn obj slot (os-object (CN-os cn)) (os-slot (CN-os cn))))
        (t
         (setf (CN-os cn) (os obj slot))
         (connect-add-constraint cn))))

(defun save-invalidated-path-constraints (obj slot)
  (setf *invalidated-path-constraints*
	(append nil
		*invalidated-path-constraints*)))

(defun s-value-fn-hook (schema slot value)
  (multi-garnet-s-value-fn schema slot value))

(defun multi-garnet-s-value-fn (schema slot value)
  (when (not (eq slot :is-a))
    (LET ((OBJ SCHEMA) (SLOT SLOT) (VALUE VALUE))
      (LET ((OLD-VALUE (GET-LOCAL-VALUE OBJ SLOT)))
	(WHEN
	    (AND (CONSTRAINT-P OLD-VALUE)
		 (CONSTRAINT-IN-OBJ-SLOT OLD-VALUE OBJ SLOT))))
      (S-VALUE-FN-SAVE-FN-SYMBOL OBJ SLOT VALUE)
      (WHEN (AND (CONSTRAINT-P VALUE) (NULL (CN-OS VALUE)))
	(ADD-CONSTRAINT-TO-SLOT OBJ SLOT VALUE))
      (SAVE-INVALIDATED-PATH-CONSTRAINTS OBJ SLOT)
      VALUE))
  value)

(defun kr-init-method-hook (schema &optional the-function)
  (KR-INIT-METHOD-SAVE-FN-SYMBOL SCHEMA THE-FUNCTION)
  (copy-down-and-activate-constraints schema))

(defun copy-down-and-activate-constraints (schema)
  (copy-down-mg-constraints schema)
  (activate-new-instance-cns schema)
  )

;; copies down cns from parent, _without_ activating them
(defun copy-down-mg-constraints (schema)
  (let ((parent (car (get-value schema :is-a))))
    (when parent
      (let* ((local-only-slots-val (g-value-no-copy parent :LOCAL-ONLY-SLOTS))
	     (local-only-slots (if (listp local-only-slots-val)
				   local-only-slots-val
				   (list local-only-slots-val))))
	(doslots (slot parent)
	  (when (and (not (eq slot :is-a))
		     (not (member slot local-only-slots))
		     (not (has-slot-p schema slot))
		     (constraint-p (get-local-value parent slot))
		     (constraint-in-obj-slot (get-local-value parent slot) parent slot))
	    (LET ((OBJ SCHEMA) (SLOT SLOT) (VALUE (GET-LOCAL-VALUE PARENT SLOT)))
	      (S-VALUE-FN-SAVE-FN-SYMBOL OBJ SLOT VALUE)
	      (SAVE-INVALIDATED-PATH-CONSTRAINTS OBJ SLOT)
	      VALUE)
	    ))
	))))

;; activates all cns in new instance (which all should be unconnected)
(defun activate-new-instance-cns (schema)
  (doslots
      (slot schema)
    (let ((value (get-local-value schema slot)))
      (cond ((not (constraint-p value))
	     nil)
	    ((not (null (CN-os value)))
	     ;; inherited cn that belongs to another os
	     nil)
	    ((cn-connection-p value :unconnected)
	     (add-constraint-to-slot schema slot value))
	    (t
	     (cerror "don't activate cn" "initializing <~S,~S>: found connected cn ~S with os ~S"
		     schema slot value (CN-os value)))
	    ))))

(defun connect-add-constraint (cn)
  (when (and (os-p (cn-os cn))
	     (cn-connection-p cn :unconnected))
    (connect-constraint cn))
  (when (cn-connection-p cn :connected)
    (mg-add-constraint cn)))

(defun connect-constraint (cn)
  (let* ((cn-os (CN-os cn))
	 (cn-var-paths (CN-variable-paths cn)))
    (cond ((not (cn-connection-p cn :unconnected))
	   (cerror "noop" "trying to connect constraint ~S with connection ~S"
		   cn (CN-connection cn))
	   nil)
	  (t
	   (let* ((root-obj (os-object cn-os))
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
			       (copy-down-slot-value obj slot)
			       (setf obj (g-value obj slot))
			       (when (not (schema-p obj))
				 (setf paths-broken t)
				 (return nil))
			       ))))
	     (setf (CN-path-slot-list cn) cn-path-links)
	     (cond (paths-broken
		    (setf (CN-variables cn) nil)
		    (setf (CN-connection cn) :broken-path))
		   (t
		    (setf (CN-variables cn)
			  (loop for var-os in var-os-list
			     collect (create-object-slot-var (os-object var-os) (os-slot var-os))))
 		    (init-method-outputs cn)
		    (setf (CN-connection cn) :connected))))))))

(defun set-object-slot-prop (obj slot prop val)
  (let* ((os-props (g-value-body OBJ :SB-OS-PROPS))
	 (slot-props (getf os-props slot nil)))
    (setf (getf slot-props prop) val)
    (setf (getf os-props slot) slot-props)
    val))

(defun create-object-slot-var (obj slot)
  (let ((var (create-mg-variable :os (os obj slot))))
    (set-object-slot-prop obj slot :sb-variable var)
    (copy-down-slot-value obj slot)
    var))

(defun copy-down-slot-value (obj slot)
  (unless (has-slot-p obj slot)
    (LET ((OBJ OBJ) (SLOT SLOT) (VALUE (G-VALUE OBJ SLOT)))
      (IF (OR (CONSTRAINT-P VALUE) (CONSTRAINT-P (GET-LOCAL-VALUE OBJ SLOT)))
	  (CERROR "cont" "can't set <~S,~S> to constraint" OBJ SLOT))
      (S-VALUE-FN-SAVE-FN-SYMBOL OBJ SLOT VALUE)
      VALUE)))

(defun get-variable-value (var)
  (var-value var))

(defun set-variable-value (var val)
  (setf (var-value var) val))

(defun mg-add-constraint (cn)
  (with-no-invalidation-update
      (when (not (cn-connection-p cn :connected))
	(cerror "cont" "trying to add constraint ~S with connection ~S"
		cn (CN-connection cn)))
    ;; note that cn is in graph, even if it is an unsat req cn
    (setf (CN-connection cn) :graph)
    (add-constraint cn)
    (cond ((and (not (enforced cn))
		(eq :max (CN-strength cn))))))
  cn)

(defvar *fn-to-hook-plist* '(s-value-fn                   s-value-fn-hook
			     kr-init-method               kr-init-method-hook
			     ))

(defun enable-multi-garnet ()
  (loop for (fn hook-fn) on *fn-to-hook-plist* by #'CDDR
     do (install-hook fn hook-fn)))

(eval-when (:load-toplevel :execute)
  (enable-multi-garnet))
