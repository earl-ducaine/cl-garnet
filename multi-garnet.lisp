

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

(defun create-mg-constraint (&key variable-paths variable-names)
  (let ((cn (make-sb-constraint)))
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

(defun constraint-p (obj)
  (and (sb-constraint-p obj)
       (not (null (CN-connection obj)))))

(defun add-constraint-to-slot (obj slot cn)
  (setf (CN-os cn) (os obj slot))
  (connect-constraint cn))

(defun kr-init-method-hook (schema &optional the-function)
  (kr-init-method-save-fn-symbol schema the-function)
  (copy-down-and-activate-constraints schema))

(defun copy-down-and-activate-constraints (schema)
  (let ((parent (car (get-value schema :is-a))))
    (let* ((local-only-slots-val (g-value-no-copy parent :LOCAL-ONLY-SLOTS))
	   (local-only-slots (if (listp local-only-slots-val)
				 local-only-slots-val
				 (list local-only-slots-val))))
      (doslots (slot parent)
	(let ((value (get-local-value parent slot)))
	  (s-value-fn-save-fn-symbol schema slot value)))))
  (activate-new-instance-cns schema))

(defun activate-new-instance-cns (schema)
  (doslots (slot schema)
    (let ((value (get-local-value schema slot)))
      (if (constraint-p value)
	  (add-constraint-to-slot schema slot value)))))

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
			(s-value-fn-save-fn-symbol obj slot (g-value obj slot))
			(setf obj (g-value obj slot))))))
      (setf (CN-path-slot-list cn) cn-path-links)
      (setf (CN-variables cn)
	    (loop for var-os in var-os-list
	       collect (create-object-slot-var
			(os-object var-os)
			(os-slot var-os))))
      (setf (CN-connection cn) :connected))))

(defun set-object-slot-prop (obj slot prop val)
  (let* ((os-props (g-value-body OBJ :SB-OS-PROPS))
	 (slot-props (getf os-props slot nil)))
    (setf (getf slot-props prop) val)
    (setf (getf os-props slot) slot-props)
    val))

(defun create-object-slot-var (obj slot)
  (let ((var (create-mg-variable :os (os obj slot))))
    (set-object-slot-prop obj slot :sb-variable var)
    (s-value-fn-save-fn-symbol obj slot (g-value obj slot))
    var))

(eval-when (:load-toplevel :execute)
  (loop for (fn hook-fn) on '(s-value-fn s-value-fn-save-fn-symbol
			      kr-init-method kr-init-method-hook
			      ) by #'CDDR
     do (install-hook fn hook-fn)))

(do-schema-body-alt
    (make-a-new-schema '*axis-rectangle*) rectangle
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
